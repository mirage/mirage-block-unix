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

module Raw = struct
  external openfile_direct: string -> bool -> int -> Unix.file_descr = "stub_openfile_direct"

  external blkgetsize64: string -> int64 = "stub_blkgetsize64"

  external fsync : Unix.file_descr -> unit = "stub_fsync"
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
      return (`Ok { fd = Some fd; info = { sector_size; size_sectors; read_write } })
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

let rec read x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    begin match x.fd with
    | None -> return (`Error Disconnected)
    | Some fd ->
      Lwt_unix.LargeFile.lseek fd Int64.(mul sector_start (of_int x.info.sector_size)) Unix.SEEK_SET >>= fun _ ->
      really_read fd b >>= fun () ->
      read x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs
    end

let rec write x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    begin match x.fd with
    | None -> return (`Error Disconnected)
    | Some fd ->
      Lwt_unix.LargeFile.lseek fd Int64.(mul sector_start (of_int x.info.sector_size)) Unix.SEEK_SET >>= fun _ ->
      really_write fd b >>= fun () ->
      write x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs
    end
