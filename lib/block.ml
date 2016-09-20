(*
 * Copyright (C) 2011-2013 Citrix Inc
 * Copyright (C) 2016 Docker Inc
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

let src =
  let src = Logs.Src.create "mirage-block-unix" ~doc:"Mirage BLOCK interface for Unix" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let is_win32 = Sys.os_type = "Win32"

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

module Config = struct
  type t = {
    buffered: bool;
    sync: bool;
    path: string;
  }

  let create ?(buffered = false) ?(sync = true) path =
    { buffered; sync; path }

  let to_string t =
    let query = [
      "buffered", [ if t.buffered then "1" else "0" ];
      "sync",     [ if t.sync then "1" else "0" ];
    ] in
    let u = Uri.make ~scheme:"file" ~path:t.path ~query () in
    Uri.to_string u

  let of_string x =
    let u = Uri.of_string x in
    match Uri.scheme u with
    | Some "file" ->
      let query = Uri.query u in
      let buffered = try List.assoc "buffered" query = [ "1" ] with Not_found -> false in
      let sync     = try List.assoc "sync"     query = [ "1" ] with Not_found -> false in
      let path = Uri.(pct_decode @@ path u) in
      `Ok { buffered; sync; path }
    | _ ->
      `Error (`Msg "Config.to_string expected a string of the form file://<path>?sync=(0|1)&buffered=(0|1)")
end

type t = {
  mutable fd: Lwt_unix.file_descr option;
  m: Lwt_mutex.t;
  mutable info: info;
  config: Config.t;
  use_fsync_after_write: bool;
  use_fsync_on_flush: bool;
}

let get_config { config } = config

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

let (>>*=) m f = m >>= function
  | `Ok x -> f x
  | `Error x -> Lwt.return (`Error x)

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
    Log.err (fun f -> f "get_file_size %s: entity is neither a file nor a block device" filename);
    `Error
      (`Unknown
         (Printf.sprintf "get_file_size %s: neither a file nor a block device" filename))

let connect_from_config ({ Config.buffered; sync; path } as config) =
  let openfile, use_fsync_after_write = match buffered, is_win32 with
    | true, _ -> Raw.openfile_buffered, false
    | false, false -> Raw.openfile_unbuffered, false
    | false, true ->
      (* We can't use O_DIRECT or F_NOCACHE on Win32, so for now
         we will use `fsync` after every write. *)
      Raw.openfile_buffered, true in
  (* first try read/write and then fall back to read/only *)
  try
    let fd, read_write =
      try
        openfile path true 0o0, true
      with _ ->
        openfile path false 0o0, false in
    match get_file_size path fd with
    | `Error e ->
      Unix.close fd;
      return (`Error e)
    | `Ok x ->
      let sector_size = 512 in (* XXX: hardcoded *)
      let size_sectors = Int64.(div x (of_int sector_size)) in
      let fd = Lwt_unix.of_unix_file_descr fd in
      let m = Lwt_mutex.create () in
      let use_fsync_on_flush = sync in
      return (`Ok { fd = Some fd; m; info = { sector_size; size_sectors; read_write };
        config; use_fsync_after_write; use_fsync_on_flush })
  with e ->
    Log.err (fun f -> f "connect %s: failed to open file" path);
    return (`Error (`Unknown (Printf.sprintf "connect %s: failed to open file" path)))

(* prefix which signals we want to use buffered I/O *)
let buffered_prefix = "buffered:"

let remove_prefix prefix x =
  let prefix' = String.length prefix and x' = String.length x in
  if x' >= prefix' && (String.sub x 0 prefix' = prefix)
  then true, String.sub x prefix' (x' - prefix')
  else false, x

let connect name =
  let buffered, path = remove_prefix buffered_prefix name in
  let config = { Config.buffered; sync = false; path } in
  connect_from_config config

let disconnect t = match t.fd with
  | Some fd ->
    Lwt_unix.close fd >>= fun () ->
    t.fd <- None;
    return ()
  | None ->
    return ()

let get_info { info } = return info

let really_read fd = Lwt_cstruct.complete (Lwt_cstruct.read fd)
let really_write fd = Lwt_cstruct.complete (Lwt_cstruct.write fd)

let lwt_wrap_exn t op offset ?buffer f =
  let fatalf fmt = Printf.ksprintf (fun s ->
      Log.err (fun f -> f "%s" s);
      return (`Error (`Unknown s))
    ) fmt in
  let describe_buffer = function
    | None -> ""
    | Some x -> "with buffer of length " ^ (string_of_int (Cstruct.len x)) in
  (* Buffer must be a multiple of sectors in length *)
  ( match buffer with
    | None -> Lwt.return (`Ok ())
    | Some b ->
      let len = Cstruct.len b in
      if len mod t.info.sector_size <> 0
      then fatalf "%s: buffer length (%d) is not a multiple of sector_size (%d) for file %s" op len t.info.sector_size t.config.Config.path
      else Lwt.return (`Ok ())
  ) >>*= fun () ->
  Lwt.catch f
    (function
      | End_of_file ->
        fatalf "%s: End_of_file at file %s offset %Ld %s" op t.config.Config.path offset (describe_buffer buffer)
      | Unix.Unix_error(code, fn, arg) ->
        fatalf "%s: %s in %s '%s' at file %s offset %Ld %s" op (Unix.error_message code) fn arg t.config.Config.path offset (describe_buffer buffer)
      | e ->
        fatalf "%s: %s at file %s offset %Ld %s" op (Printexc.to_string e) t.config.Config.path offset (describe_buffer buffer)
    )

let rec read x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    begin match x.fd with
      | None -> return (`Error `Disconnected)
      | Some fd ->
        let offset = Int64.(mul sector_start (of_int x.info.sector_size))  in
        lwt_wrap_exn x "read" offset ~buffer:b
          (fun () ->
             Lwt_mutex.with_lock x.m
               (fun () ->
                  let b_sectors = (Cstruct.len b) / x.info.sector_size in
                  if Int64.(add sector_start (of_int b_sectors) > x.info.size_sectors) then begin
                    Log.err (fun f -> f "read beyond end of file: sector_start (%Ld) + b (%d) > size_sectors (%Ld)"
                                sector_start b_sectors x.info.size_sectors);
                    fail End_of_file
                  end else begin
                    Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
                    really_read fd b
                  end
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
        lwt_wrap_exn x "write" offset ~buffer:b
          (fun () ->
             Lwt_mutex.with_lock x.m
               (fun () ->
                  let b_sectors = (Cstruct.len b) / x.info.sector_size in
                  if Int64.(add sector_start (of_int b_sectors) > x.info.size_sectors) then begin
                    Log.err (fun f -> f "write beyond end of file: sector_start (%Ld) + b (%d) > size_sectors (%Ld)"
                                sector_start b_sectors x.info.size_sectors);
                    fail End_of_file
                  end else begin
                    Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun _ ->
                    really_write fd b
                  end
               ) >>= fun () ->
             ( if x.use_fsync_after_write then Lwt_unix.fsync fd else Lwt.return () )
             >>= fun () ->
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
    if is_win32
    then return (`Error `Unimplemented)
    else lwt_wrap_exn t "ftruncate" new_size_bytes
        (fun () ->
           Lwt_mutex.with_lock t.m
             (fun () ->
                Lwt_unix.LargeFile.ftruncate fd new_size_bytes
                >>= fun () ->
                t.info <- { t.info with size_sectors = new_size_sectors };
                return (`Ok ())
             )
        )

external flush_job: Unix.file_descr -> unit Lwt_unix.job = "mirage_block_unix_flush_job"

let flush t =
  match t.fd with
  | None -> return (`Error `Disconnected)
  | Some fd ->
    lwt_wrap_exn t "fsync" 0L
      (fun () ->
         ( if t.use_fsync_on_flush
           then Lwt_unix.run_job (flush_job (Lwt_unix.unix_file_descr fd))
           else Lwt.return_unit )
         >>= fun () ->
         return (`Ok ())
      )

let seek_mapped t from =
  match t.fd with
  | None -> return (`Error `Disconnected)
  | Some fd ->
    let offset = Int64.(mul from (of_int t.info.sector_size)) in
    lwt_wrap_exn t "seek_mapped" offset
      (fun () ->
         Lwt_mutex.with_lock t.m
           (fun () ->
              let fd = Lwt_unix.unix_file_descr fd in
              let offset = Raw.lseek_data fd offset in
              return (`Ok Int64.(div offset (of_int t.info.sector_size)))
           )
      )

let seek_unmapped t from =
  match t.fd with
  | None -> return (`Error `Disconnected)
  | Some fd ->
    let offset = Int64.(mul from (of_int t.info.sector_size)) in
    lwt_wrap_exn t "seek_unmapped" offset
      (fun () ->
         Lwt_mutex.with_lock t.m
           (fun () ->
              let fd = Lwt_unix.unix_file_descr fd in
              let offset = Raw.lseek_hole fd offset in
              return (`Ok Int64.(div offset (of_int t.info.sector_size)))
           )
      )
