/*
 * Copyright (c) 2016 Docker Inc
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
 */

#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/callback.h>

#include "lwt_unix.h"

struct job_flush {
  struct lwt_unix_job job;
  int fd;
  int errno_copy;
};

static void worker_flush(struct job_flush *job)
{
  int result = 0;
#if defined(__APPLE__)
  result = fcntl(job->fd, F_FULLFSYNC);
#else
  result = fsync(job->fd);
#endif
  if (result == -1) {
    job->errno_copy = errno;
  }
}

static value result_flush(struct job_flush *job)
{
  CAMLparam0 ();
  int errno_copy = job->errno_copy;
  lwt_unix_free_job(&job->job);
  if (errno_copy != 0) {
#if defined(__APPLE__)
    unix_error(errno_copy, "fcntl", Nothing);
#else
    unix_error(errno_copy, "fsync", Nothing);
#endif
  }
  CAMLreturn(Val_unit);
}

CAMLprim
value mirage_block_unix_flush_job(value handle)
{
  CAMLparam1(handle);
  LWT_UNIX_INIT_JOB(job, flush, 0);
  job->fd = Int_val(handle);
  job->errno_copy = 0;
  CAMLreturn(lwt_unix_alloc_job(&(job->job)));
}
