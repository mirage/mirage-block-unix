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

type id = string

module Raw = struct
  external openfile_unbuffered: string -> bool -> int -> Unix.file_descr = "stub_openfile_direct"
  let openfile_buffered name rw perm =
    Unix.openfile name [ if rw then Unix.O_RDWR else Unix.O_RDONLY ] perm

  external blkgetsize: Unix.file_descr -> int64 = "stub_blkgetsize"

  external lseek_data : Unix.file_descr -> int64 -> int64 = "stub_lseek_data_64"

  external lseek_hole : Unix.file_descr -> int64 -> int64 = "stub_lseek_hole_64"

end

open Lwt

type 'a io = 'a Lwt.t

type page_aligned_buffer = Cstruct.t

type error = [
  | `Unknown of string
  | `Unimplemented
  | `Is_read_only
  | `Disconnected
]

type info = {
  read_write: bool;
  sector_size: int;
  size_sectors: int64;
}

type t = {
  mutable fd: Lwt_unix.file_descr option;
  m: Lwt_mutex.t;
  name: string;
  mutable info: info;
}

let id { name } = name

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
    with e ->
      `Error (`Unknown (Printf.sprintf "%s %s: %s" f' x' (Printexc.to_string e)))
end

let stat filename fd = Result.wrap_exn "stat" filename Unix.LargeFile.fstat fd
let blkgetsize filename fd = Result.wrap_exn "BLKGETSIZE" filename Raw.blkgetsize fd

let get_file_size filename fd =
  let open Result in
  stat filename fd
  >>= fun st ->
  match st.Unix.LargeFile.st_kind with
  | Unix.S_REG -> `Ok st.Unix.LargeFile.st_size
  | Unix.S_BLK -> blkgetsize filename fd
  | _ ->
    `Error
      (`Unknown
         (Printf.sprintf "get_file_size %s: neither a file nor a block device" filename))

(* prefix which signals we want to use buffered I/O *)
let buffered_prefix = "buffered:"

let remove_prefix prefix x =
  let prefix' = String.length prefix and x' = String.length x in
  if x' >= prefix' && (String.sub x 0 prefix' = prefix)
  then true, String.sub x prefix' (x' - prefix')
  else false, x

let connect name =
  let buffered, name = remove_prefix buffered_prefix name in
  let openfile = if buffered then Raw.openfile_buffered else Raw.openfile_unbuffered in
  (* first try read/write and then fall back to read/only *)
  try
    let fd, read_write =
      try
        openfile name true 0o0, true
      with _ ->
        openfile name false 0o0, false in
    match get_file_size name fd with
    | `Error e ->
      Unix.close fd;
      return (`Error e)
    | `Ok x ->
      let sector_size = 512 in (* XXX: hardcoded *)
      let size_sectors = Int64.(div x (of_int sector_size)) in
      let fd = Lwt_unix.of_unix_file_descr fd in
      let m = Lwt_mutex.create () in
      return (`Ok { fd = Some fd; m; name; info = { sector_size; size_sectors; read_write } })
  with e ->
    return (`Error (`Unknown (Printf.sprintf "connect %s: failed to oppen file" name)))

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
      | End_of_file ->
        return (`Error
                  (`Unknown
                     (Printf.sprintf "%s: End_of_file at file %s offset %Ld with length %d"
                        op name offset length)))
      | Unix.Unix_error(code, fn, arg) ->
        return (`Error
                  (`Unknown
                     (Printf.sprintf "%s: %s in %s '%s' at file %s offset %Ld with length %d"
                        op (Unix.error_message code) fn arg name offset length)))
      | e ->
        return (`Error
                  (`Unknown
                     (Printf.sprintf "%s: %s at file %s offset %Ld with length %d"
                        op (Printexc.to_string e) name offset length))))

let rec read x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    begin match x.fd with
      | None -> return (`Error `Disconnected)
      | Some fd ->
        let offset = Int64.(mul sector_start (of_int x.info.sector_size))  in
        lwt_wrap_exn x.name "read" offset (Cstruct.len b)
          (fun () ->
             if Int64.(add sector_start (of_int ((Cstruct.len b) / x.info.sector_size))) >
                x.info.size_sectors then fail End_of_file else
             Lwt_mutex.with_lock x.m
               (fun () ->
                 Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
                 really_read fd b
               ) >>= fun () ->
             return (`Ok ())
          ) >>= function
        | `Ok () -> read x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs
        | `Error x -> return (`Error x)
    end

let rec write x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    begin match x with
      | { fd = None } ->
        return (`Error `Disconnected)
      | { info = { read_write = false } } ->
        return (`Error `Is_read_only)
      | { fd = Some fd } ->
        let offset = Int64.(mul sector_start (of_int x.info.sector_size)) in
        lwt_wrap_exn x.name "write" offset (Cstruct.len b)
          (fun () ->
             if Int64.(add sector_start (of_int ((Cstruct.len b) / x.info.sector_size))) >
                x.info.size_sectors then fail End_of_file else
             Lwt_mutex.with_lock x.m
               (fun () ->
                 Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
                 really_write fd b
               ) >>= fun () ->
             return (`Ok ())
          ) >>= function
        | `Ok () ->
          write x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs
        | `Error x ->
          return (`Error x)
    end

let resize t new_size_sectors =
  let new_size_bytes = Int64.(mul new_size_sectors (of_int t.info.sector_size)) in
  match t.fd with
    | None -> return (`Error `Disconnected)
    | Some fd ->
      lwt_wrap_exn t.name "ftruncate" new_size_bytes 0
        (fun () ->
          Lwt_unix.LargeFile.ftruncate fd new_size_bytes
          >>= fun () ->
          t.info <- { t.info with size_sectors = new_size_sectors };
          return (`Ok ())
        )

let flush t =
  match t.fd with
    | None -> return (`Error `Disconnected)
    | Some fd ->
      lwt_wrap_exn t.name "fsync" 0L 0
        (fun () ->
          Lwt_unix.fsync fd
          >>= fun () ->
          return (`Ok ())
        )

let seek_mapped t from =
  match t.fd with
    | None -> return (`Error `Disconnected)
    | Some fd ->
      let offset = Int64.(mul from (of_int t.info.sector_size)) in
      lwt_wrap_exn t.name "seek_mapped" offset 0
        (fun () ->
          let fd = Lwt_unix.unix_file_descr fd in
          let offset = Raw.lseek_data fd offset in
          return (`Ok Int64.(div offset (of_int t.info.sector_size)))
        )

let seek_unmapped t from =
  match t.fd with
    | None -> return (`Error `Disconnected)
    | Some fd ->
      let offset = Int64.(mul from (of_int t.info.sector_size)) in
      lwt_wrap_exn t.name "seek_unmapped" offset 0
        (fun () ->
          let fd = Lwt_unix.unix_file_descr fd in
          let offset = Raw.lseek_hole fd offset in
          return (`Ok Int64.(div offset (of_int t.info.sector_size)))
        )
