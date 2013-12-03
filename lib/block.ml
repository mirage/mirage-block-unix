(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Raw = struct
  external openfile_direct: string -> bool -> int -> Unix.file_descr = "stub_openfile_direct"

  external blkgetsize64: string -> int64 = "stub_blkgetsize64"

  external fsync : Unix.file_descr -> unit = "stub_fsync"

  external alloc_pages: int -> buf = "caml_alloc_pages"
end

module Memory = struct

  let get n =
    if n < 1
    then raise (Invalid_argument "The number of page should be greater or equal to 1")
    else
      try Raw.alloc_pages n with _ ->
      Gc.compact ();
      try Raw.alloc_pages n with _ -> raise Out_of_memory

  let page_size = 4096

  let alloc_bigarray bytes =
    (* round up to next PAGE_SIZE *)
    let pages = (bytes + page_size - 1) / page_size in
    (* but round-up 0 pages to 0 *)
    let pages = max pages 1 in
    get pages

  let alloc bytes =
    let larger_than_we_need = Cstruct.of_bigarray (alloc_bigarray bytes) in
    Cstruct.sub larger_than_we_need 0 bytes
end

open Lwt

type 'a io = 'a Lwt.t

type page_aligned_buffer = Cstruct.t

type error =
| Unknown of string
| Unimplemented
| Is_read_only
| Disconnected

type info = {
  read_write: bool;
  sector_size: int;
  size_sectors: int64;
}

type t = {
  mutable fd: Lwt_unix.file_descr option;
  name: string;
  info: info;
}

module Result = struct
  type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
  ]

  let ( >>= ) x f = match x with
  | `Error y -> `Error y
  | `Ok z -> f z

  let wrap_exn f' x' f x =
    try `Ok (f x)
    with e -> `Error (Unknown (Printf.sprintf "%s %s: %s" f' x' (Printexc.to_string e)))
end

let stat x = Result.wrap_exn "stat" x Unix.LargeFile.stat x
let blkgetsize64 x = Result.wrap_exn "BLKGETSIZE64" x Raw.blkgetsize64 x

let get_file_size x =
  let open Result in
  stat x >>= fun st -> match st.Unix.LargeFile.st_kind with
  | Unix.S_REG -> `Ok st.Unix.LargeFile.st_size
  | Unix.S_BLK -> blkgetsize64 x
  | _ -> `Error (Unknown (Printf.sprintf "get_file_size %s: neither a file nor a block device" x))

let connect name =
  (* first try read/write and then fall back to read/only *)
  try
    let fd, read_write =
      try
        Raw.openfile_direct name true 0o0, true
      with _ ->
      Raw.openfile_direct name false 0o0, false in
    match get_file_size name with
    | `Error e ->
      Unix.close fd;
      return (`Error e)
    | `Ok x ->
      let sector_size = 512 in (* XXX: hardcoded *)
      let size_sectors = Int64.(div x (of_int sector_size)) in
      let fd = Lwt_unix.of_unix_file_descr fd in
      return (`Ok { fd = Some fd; name; info = { sector_size; size_sectors; read_write } })
  with e ->
    return (`Error (Unknown (Printf.sprintf "connect %s: failed to oppen file" name)))

let disconnect t = match t.fd with
  | Some fd ->
    Lwt_unix.close fd >>= fun () ->
    t.fd <- None;
    return ()
  | None ->
    return ()

let get_info { info } = return info

let complete op fd buffer =
  let open Lwt in
  let ofs = buffer.Cstruct.off in
  let len = buffer.Cstruct.len in
  let buf = buffer.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()

let really_read = complete Lwt_bytes.read
let really_write = complete Lwt_bytes.write

let lwt_wrap_exn name op offset length f =
  Lwt.catch f
    (function
     | End_of_file -> return (`Error (Unknown (Printf.sprintf "%s: End_of_file at file %s offset %Ld with length %d" op name offset length)))
     | Unix.Unix_error(code, fn, arg) -> return (`Error (Unknown (Printf.sprintf "%s: %s in %s '%s' at file %s offset %Ld with length %d" op (Unix.error_message code) fn arg name offset length)))
     | e -> return (`Error (Unknown (Printf.sprintf "%s: %s at file %s offset %Ld with length %d" op (Printexc.to_string e) name offset length))))

let rec read x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    begin match x.fd with
    | None -> return (`Error Disconnected)
    | Some fd ->
      let offset = Int64.(mul sector_start (of_int x.info.sector_size))  in
      lwt_wrap_exn x.name "read" offset (Cstruct.len b)
        (fun () ->
          Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
          really_read fd b >>= fun () ->
          return (`Ok ())
        ) >>= function
      | `Ok () -> read x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs
      | `Error x -> return (`Error x)
    end

let rec write x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    begin match x with
    | { fd = None } -> return (`Error Disconnected)
    | { info = { read_write = false } } -> return (`Error Is_read_only)
    | { fd = Some fd } ->
      let offset = Int64.(mul sector_start (of_int x.info.sector_size)) in
      lwt_wrap_exn x.name "write" offset (Cstruct.len b)
        (fun () ->
          Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
          really_write fd b >>= fun () ->
          return (`Ok ())
        ) >>= function
      | `Ok () -> write x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs
      | `Error x -> return (`Error x)
    end
