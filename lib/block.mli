(*
 * Copyright (C) 2013 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Block device on top of Lwt_unix *)

include V1.BLOCK
with type 'a io = 'a Lwt.t
and type     page_aligned_buffer = Cstruct.t
and type     id = string

(** {0} low-level convenience functions *)

val really_read: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t

val really_write: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t

val blkgetsize: string -> Unix.file_descr -> [ `Ok of int64 | `Error of error ]
(** [blkgetsize path fd]: returns the size of the open block device
    given by [fd]. [path] is only used to construct a human-readable error
    message. *)

val connect : string -> [`Ok of t | `Error of error] io

val resize : t -> int64 -> [ `Ok of unit | `Error of error ] io
(** [resize t new_size_sectors] attempts to resize the connected device
    to have the given number of sectors. If successful, subsequent calls
    to [get_info] will reflect the new size. *)
