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
open Utils

let cstruct =
  Alcotest.testable (Fmt.of_to_string Cstruct.to_string) cstruct_equal

let or_failwith = function
  | Error e -> failwith @@ Format.asprintf "%a" Block.pp_error e
  | Ok x -> x

let write_or_failwith = function
  | Error e -> failwith @@ Format.asprintf "%a" Block.pp_write_error e
  | Ok x -> x

let test_enoent () =
  let t =
    let name = find_unused_file () in
    Lwt.catch
      (fun () ->
        Block.connect name >>= fun _b ->
        failwith (Printf.sprintf "Block.connect %s should have failed" name))
      (fun _ -> Lwt.return_unit)
  in
  Lwt_main.run t

let test_open_read () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0444
    >>= fun fd ->
    let size = Int64.(mul 1024L 1024L) in
    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR
    >>= fun _ ->
    let message = "All work and no play makes Dave a dull boy.\n" in
    let sector = alloc 512 in
    for i = 0 to 511 do
      Cstruct.set_char sector i message.[i mod String.length message]
    done;
    Block.really_write fd sector >>= fun () ->
    let sector' = alloc 512 in
    Block.connect name >>= fun device ->
    Block.read device Int64.(sub (div size 512L) 1L) [ sector' ] >>= function
    | Error _ -> failwith (Printf.sprintf "Block.read %s failed" name)
    | Ok () ->
        Alcotest.check cstruct "open-read" sector sector';
        return ()
  in
  Lwt_main.run t

open Mirage_block

let test_open_block () =
  let t =
    with_temp_file (fun file ->
        Block.connect file >>= fun device1 ->
        Block.get_info device1 >>= fun info1 ->
        let size1 = Int64.(mul info1.size_sectors (of_int info1.sector_size)) in
        with_temp_volume file (fun volume ->
            Block.connect volume >>= fun device2 ->
            Block.get_info device2 >>= fun info2 ->
            let size2 =
              Int64.(mul info2.size_sectors (of_int info2.sector_size))
            in
            (* The size of the file and the block device should be the same *)
            Alcotest.(check int64) "open-block" size1 size2;
            Block.disconnect device2))
  in
  Lwt_main.run t

let test_write_read () =
  let t =
    with_temp_file (fun file ->
        Block.connect file >>= fun device1 ->
        Block.get_info device1 >>= fun info1 ->
        let sector = alloc info1.sector_size in
        let rec write x =
          if x = 0 then Lwt.return (Ok ())
          else (
            Cstruct.memset sector x;
            Block.write device1 (Int64.of_int x) [ sector ] >>= fun r ->
            let () = write_or_failwith r in
            write (x - 1) )
        in
        write 255 >>= fun x ->
        let () = or_failwith x in
        let sector' = alloc info1.sector_size in
        let rec read x =
          if x = 0 then Lwt.return (Ok ())
          else (
            Cstruct.memset sector' x;
            Block.read device1 (Int64.of_int x) [ sector ] >>= fun r ->
            let () = or_failwith r in
            if not (Cstruct.equal sector sector') then
              failwith (Printf.sprintf "test_write_read: sector %d not equal" x);
            read (x - 1) )
        in
        read 255 >>= fun x ->
        let () = or_failwith x in
        Lwt.return ())
  in
  Lwt_main.run t

let test_buffer_wrong_length () =
  let t =
    with_temp_file (fun file ->
        Block.connect file >>= fun device1 ->
        Block.get_info device1 >>= fun info1 ->
        let sector = alloc info1.sector_size in
        Block.write device1 0L [ Cstruct.shift sector 1 ] >>= function
        | Error _ -> Lwt.return ()
        | Ok () -> failwith "a write with a bad length succeeded")
  in
  Lwt_main.run t

let test_eof () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0444
    >>= fun fd ->
    let size = Int64.(mul 1024L 1024L) in
    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR
    >>= fun _ ->
    let message = "All work and no play makes Dave a dull boy.\n" in
    let sector = alloc 512 in
    for i = 0 to 511 do
      Cstruct.set_char sector i message.[i mod String.length message]
    done;
    Block.really_write fd sector >>= fun () ->
    let sector' = alloc 512 in
    let sector'' = alloc 1024 in
    Block.connect name >>= fun device ->
    Block.write device 2046L [ sector'; sector'' ] >>= function
    | Ok _ -> failwith (Printf.sprintf "Block.write %s should have failed" name)
    | Error _ -> (
        Block.read device 2046L [ sector'; sector'' ] >>= function
        | Ok _ ->
            failwith (Printf.sprintf "Block.read %s should have failed" name)
        | Error _ -> return () )
  in
  Lwt_main.run t

let test_resize () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0644
    >>= fun fd ->
    Lwt_unix.close fd >>= fun () ->
    Block.connect name >>= fun device ->
    Block.get_info device >>= fun info1 ->
    Alcotest.(check int64) "resize" 0L info1.size_sectors;
    Block.resize device 1L >>= function
    | Error _ -> failwith (Printf.sprintf "Block.resize %s failed" name)
    | Ok () ->
        Block.get_info device >>= fun info2 ->
        Alcotest.(check int64) "resize" 1L info2.size_sectors;
        return ()
  in
  Lwt_main.run t

let test_flush () =
  let t file =
    let do_flush sync =
      Block.connect ~sync file >>= fun device1 ->
      Block.flush device1 >>= function
      | Error _ -> failwith (Printf.sprintf "Block.flush %s failed" file)
      | Ok () -> Block.disconnect device1
    in
    do_flush (Some `ToDrive) >>= fun () ->
    do_flush (Some `ToOS) >>= fun () -> do_flush None
  in
  with_temp_file (fun file -> Lwt_main.run (t file))

let config_t = Alcotest.testable (Fmt.of_to_string Config.string_of_sync) ( = )

let test_parse_print_config config =
  let open Block.Config in
  let s = to_string config in
  Alcotest.test_case s `Quick @@ fun () ->
  match of_string s with
  | Error (`Msg m) -> failwith m
  | Ok config' ->
      Alcotest.(check bool) "buffered" config.buffered config'.buffered;
      Alcotest.check config_t "sync" config.sync config'.sync;
      Alcotest.(check string) "path" config.path config'.path

let test_not_multiple_of_sectors () =
  let t =
    let file = find_unused_file () in
    Lwt.finalize
      (fun () ->
        (* Create a file containing < 512 bytes *)
        Lwt_unix.openfile file [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0644
        >>= fun fd ->
        let message = "Hello" in
        let buf = Cstruct.create (String.length message) in
        Cstruct.blit_from_string message 0 buf 0 (String.length message);
        Lwt_cstruct.(complete (write fd) buf) >>= fun () ->
        Lwt_unix.close fd >>= fun () ->
        (* We should see 1 sector *)
        (* NB we only test buffered mode because O_DIRECT read on Linux will fail
           with EINVAL if the file length is not sector-aligned. Arguably buffered
           mode should actually be the default anyway. *)
        Block.connect ~buffered:true file >>= fun device ->
        Block.get_info device >>= fun info1 ->
        Alcotest.(check int64) "1 sector" 1L info1.Mirage_block.size_sectors;
        (* We should be able to read 1 sector, padded with zeroes *)
        let sector = Cstruct.create info1.Mirage_block.sector_size in
        Block.read device 0L [ sector ] >>= function
        | Error _ ->
            failwith
              (Printf.sprintf "Block.read %s: failed to read sector 0" file)
        | Ok () -> (
            let message' =
              Cstruct.(to_string (sub sector 0 (String.length message)))
            in
            Alcotest.(check string) "message" message message';
            for i = String.length message to Cstruct.len sector - 1 do
              Alcotest.(check int) "0 sector" 0 (Cstruct.get_uint8 sector i)
            done;
            (* We should be able to write 1 sector *)
            Cstruct.memset sector 0xff;
            Block.write device 0L [ sector ] >>= function
            | Error _ ->
                failwith
                  (Printf.sprintf "Block.write %s: failed to write sector 0"
                     file)
            | Ok () -> (
                (* We should be able to read back the contents *)
                Block.read device 0L [ sector ]
                >>= function
                | Error _ ->
                    failwith
                      (Printf.sprintf "Block.read %s: failed to read sector 0"
                         file)
                | Ok () ->
                    for i = 0 to Cstruct.len sector - 1 do
                      Alcotest.(check int)
                        "read sector" 0xff
                        (Cstruct.get_uint8 sector i)
                    done;
                    (* The file should still be 1 sector in length *)
                    Block.disconnect device >>= fun () ->
                    Block.connect file >>= fun device ->
                    Block.get_info device >>= fun info1 ->
                    Alcotest.(check int64)
                      "size_sectcor" 1L info1.Mirage_block.size_sectors;
                    Block.disconnect device ) ))
      (fun () -> Lwt_unix.unlink file)
  in
  Lwt_main.run t

let tests =
  let test n f = Alcotest.test_case n `Quick f in
  [
    test "test ENOENT" test_enoent;
    test "test open read" test_open_read;
    (* Doesn't work on travis
       "test opening a block device" >:: test_open_block;
    *)
    test "test read/write after last sector" test_eof;
    test "test flush" test_flush;
    test_parse_print_config
      {
        Block.Config.buffered = true;
        sync = None;
        path = "C:\\cygwin";
        lock = false;
      };
    test_parse_print_config
      {
        Block.Config.buffered = false;
        sync = Some `ToOS;
        path = "/var/tmp/foo.qcow2";
        lock = false;
      };
    test_parse_print_config
      {
        Block.Config.buffered = false;
        sync = Some `ToDrive;
        path = "/var/tmp/foo.qcow2";
        lock = true;
      };
    test "test write then read" test_write_read;
    test "test that writes fail if the buffer has a bad length"
      test_buffer_wrong_length;
    test "files which aren't a whole number of sectors"
      test_not_multiple_of_sectors;
    test "test resize" test_resize;
  ]

let () = Alcotest.run "mirage-block" [ ("block", tests) ]
