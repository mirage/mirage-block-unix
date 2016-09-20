(*
 * Copyright (C) 2013 Citrix Systems Inc
 * Copyright (C) 2016 Docker Inc
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

(** {0} low-level convenience functions *)

val really_read: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t

val really_write: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t

val blkgetsize: string -> Unix.file_descr -> [ `Ok of int64 | `Error of error ]
(** [blkgetsize path fd]: returns the size of the open block device
    given by [fd]. [path] is only used to construct a human-readable error
    message. *)

val connect : string -> [`Ok of t | `Error of error] io

module Config: sig
  type t = {
    buffered: bool; (** true if I/O hits the OS disk caches, false if "direct" *)
    sync: bool; (** true if [flush] flushes all caches, including disk drive caches *)
    path: string; (** path to the underlying file *)
  }
  (** Configuration of a device *)

  val create: ?buffered:bool -> ?sync:bool -> string -> t
  (** [create ?buffered ?sync path] constructs a configuration referencing the
      file stored at [path]/ *)

  val to_string: t -> string
  (** Marshal a config into a string of the form
      file://<path>?sync=(0|1)&buffered=(0|1) *)

  val of_string: string -> [ `Ok of t | `Error of [ `Msg of string ] ]
  (** Parse the result of a previous [to_string] invocation *)
end

val connect_uri : Uri.t -> [`Ok of t | `Error of error] io
(** [connect_uri uri] connects to [uri], respecting options provided as
    query parameters:
      buffered=(0|1): 1 means use the underlying host's buffer cache
      sync=(0|1): 1 means `flush` will also flush any storage hardware caches
        (which will be slow but writes will persist even over a power loss)
  *)

val resize : t -> int64 -> [ `Ok of unit | `Error of error ] io
(** [resize t new_size_sectors] attempts to resize the connected device
    to have the given number of sectors. If successful, subsequent calls
    to [get_info] will reflect the new size. *)

val flush : t -> [ `Ok of unit | `Error of error ] io
(** [flush t] flushes any buffers, if the file has been opened in buffered
    mode *)

val seek_unmapped: t -> int64 -> [ `Ok of int64 | `Error of error ] io
(** [seek_unmapped t start] returns the sector offset of the next guaranteed
    zero-filled region (typically guaranteed because it is unmapped) *)

val seek_mapped: t -> int64 -> [ `Ok of int64 | `Error of error ] io
(** [seek_mapped t start] returns the sector offset of the next regoin of the
    device which may have data in it (typically this is the next mapped
    region) *)

val get_config: t -> Config.t
(** [get_config t] returns the configuration of a device *)
