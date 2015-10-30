(*
 * Copyright (C) 2013 Citrix Inc
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

open Lwt
open Block
open OUnit
open Utils

let test_enoent () =
  let t =
    let name = find_unused_file () in
    Block.connect name >>= function
    | `Ok _ -> failwith (Printf.sprintf "Block.connect %s should have failed" name)
    | `Error _ -> return () in
    Lwt_main.run t

let test_open_read () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0444 >>= fun fd ->
    let size = Int64.(mul 1024L 1024L) in
    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
    let message = "All work and no play makes Dave a dull boy.\n" in
    let sector = alloc 512 in
    for i = 0 to 511 do
      Cstruct.set_char sector i (message.[i mod (String.length message)])
    done;
    Block.really_write fd sector >>= fun () ->
    let sector' = alloc 512 in
    Block.connect name >>= function
    | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" name)
    | `Ok device ->
      Block.read device Int64.(sub (div size 512L) 1L) [ sector' ] >>= function
      | `Error _ -> failwith (Printf.sprintf "Block.read %s failed" name)
      | `Ok () -> begin
        assert_equal ~printer:Cstruct.to_string ~cmp:cstruct_equal sector sector';
        return ()
      end in
  Lwt_main.run t

let test_open_block () =
  let t =
    with_temp_file
      (fun file ->
        Block.connect file >>= function
        | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" file)
        | `Ok device1 ->
          Block.get_info device1
          >>= fun info1 ->
          let size1 = Int64.(mul info1.Block.size_sectors (of_int info1.Block.sector_size)) in
          with_temp_volume file
            (fun volume ->
               Block.connect volume >>= function
               | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" volume)
               | `Ok device2 ->
                  Block.get_info device2
                  >>= fun info2 ->
                  let size2 = Int64.(mul info2.Block.size_sectors (of_int info2.Block.sector_size)) in
                  (* The size of the file and the block device should be the same *)
                  assert_equal ~printer:Int64.to_string size1 size2;
                  Block.disconnect device2
            )
      ) in
  Lwt_main.run t

let test_eof () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0444 >>= fun fd ->
    let size = Int64.(mul 1024L 1024L) in
    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
    let message = "All work and no play makes Dave a dull boy.\n" in
    let sector = alloc 512 in
    for i = 0 to 511 do
      Cstruct.set_char sector i (message.[i mod (String.length message)])
    done;
    Block.really_write fd sector >>= fun () ->
    let sector' = alloc 512 in
    let sector'' = alloc 1024 in
    Block.connect name >>= function
    | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" name)
    | `Ok device ->
      Block.write device 2046L [ sector'; sector'' ] >>= function
      | `Ok _ -> failwith (Printf.sprintf "Block.write %s should have failed" name)
      | `Error _ ->
        Block.read device 2046L [ sector'; sector'' ] >>= function
        | `Ok _ -> failwith (Printf.sprintf "Block.read %s should have failed" name)
        | `Error _ -> return () in
  Lwt_main.run t

let test_resize () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0644
    >>= fun fd ->
    Lwt_unix.close fd
    >>= fun () ->
    Block.connect name
    >>= function
    | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" name)
    | `Ok device ->
      Block.get_info device
      >>= fun info1 ->
      assert_equal ~printer:Int64.to_string 0L info1.Block.size_sectors;
      Block.resize device 1L
      >>= function
      | `Error _ -> failwith (Printf.sprintf "Block.resize %s failed" name)
      | `Ok () ->
        Block.get_info device
        >>= fun info2 ->
        assert_equal ~printer:Int64.to_string 1L info2.Block.size_sectors;
        return () in
  Lwt_main.run t

let _ =
  let suite = "block" >::: [
    "test ENOENT" >:: test_enoent;
    "test open read" >:: test_open_read;
    (* Doesn't work on travis
    "test opening a block device" >:: test_open_block;
    *)
    "test read/write after last sector" >:: test_eof;
    "test resize" >:: test_resize;
  ] in
  OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
