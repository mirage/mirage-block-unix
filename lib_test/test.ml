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
open OUnit

let test_enoent () =
  let t =
    (* Find a filename which doesn't exist *)
    let rec does_not_exist i =
      let name = Printf.sprintf "%s/mirage-block-test-missing.%d"
        Filename.temp_dir_name i in
      if Sys.file_exists name
      then does_not_exist (i + 1)
      else name in
    let name = does_not_exist 0 in
    Block.connect name >>= function
    | `Ok _ -> failwith (Printf.sprintf "Block.connect %s should have failed" name)
    | `Error _ -> return () in
    Lwt_main.run t

let test_open_rdonly () =
  ()

let test_open_file () =
  ()



let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
  "Test unix block driver";

  let suite = "block" >::: [
    "test ENOENT" >:: test_enoent;
    "test open read/only" >:: test_open_rdonly;
    "test open read/write" >:: test_open_file;
  ] in
  run_test_tt ~verbose:!verbose suite
