## unreleased

* Add `Block.with_block` to limit the lifetime of a connected block. (#???, @MisterDA)

## v2.14.2 (2022-09-15)

* Raise an exception on connect if the file is not aligned to the `sector_size` (#117, @reynir)
* Demote log to debug when an error is returned (#119 @reynir, fixes #109)
* Remove io-page dependency (#118 @hannesm)
* Add GitHub actions for testing on macos and windows (retire Appveyor)

## v2.14.1 (2022-06-15)

* Ensure compatibility with OCaml 5.0, after `uerror` change (ocaml/ocaml#10926) (#115, @dra27)

## v2.14.0 (2022-04-07)

* OCaml 5.00 support (#112, @Sudha247)
* Remove io-page.unix dependency, require io-page >= 2.4.0 (#114 @hannesm)

## v2.13.0 (2022-01-16)

* Add a configuration field for the size of sectors (#106, @Julow, @hannesm, @dinosaure)
* Update to cstruct.6.0.0 (#108, @MisterDA, @dinosaure)
* Depends on `io-page.unix` instead of `io-page-unix` (@dinosaure)

## v2.12.1 (2020-07-19)

* Fix stress test in environments without `O_DIRECT` (#103 @kit-ty-kate)

## v2.12.0 (2019-10-30)

* Adapt to mirage-block 2.0.0 interface changes (#99 @hannesm)
* Fix opam file (#84 @djs55)
* Fix testsuite bitrot (#99 @hannesm)

## v2.11.2 (2019-05-11)
* Fix test use of io-page-unix to work with io-page>2.0.0 (@avsm)
* Remove unnecessary opam dependencies and refine `ppx_sexp_conv` one (@avsm)

## v2.11.1 (2019-03-10)
* Fix some warnings in dev mode (#94 @emillon)
* Clarify documentation about behaviour of `discard` (#95 @g2p)
* Use modern cstruct-lwt ocamlfind name (#96 @g2p)

## v2.11.0 (2019-02-03)
* Port build to Dune from jbuilder (#93 @avsm)
* Update opam metadata to 2.0 format (#92 @avsm @jonludlam)
* Use dune-release instead of topkg (#93 @avsm)
* Ocamldoc fixes for odoc support (#93 @avsm)
* Fix build on older macOS (#90 @djs55)

## v2.10.0 (2018-06-11)
* Add support for `discard` (also known as TRIM)
* Add support for `resize` on Windows

## v2.9.0 (2017-12-07)
* Add support for `flock` file locking

## v2.8.4 (2017-11-25)
* opam: add lower bound on uri.1.9.0
* fix build with appveyor
* fix build with OCaml 4.06 (and -safe-string)

## v2.8.3 (2017-07-05)
* Use latest `cstruct-lwt` linkage.
* Minimum OCaml version now 4.03.0 in line with rest of MirageOS.
* Modernise jbuilder+topkg workflow.

## v2.8.2 (2017-05-22)
* Link to `cstruct.lwt` correctly, fixing the 2.8.1 fix.

## v2.8.1 (2017-05-19)
* Link to `lwt.unix` correctly, regressed in jbuilder port (report by @dim)
* Remove duplicated entries from `opam` file.
* Modernise the Travis OS build matrix and test OCaml 4.04.0/1 as well.

## v2.8.0 (2017-05-16)
* switch to buffered I/O by default
* better support for unbuffered I/O for non-512 byte sectors
* for files which are not a whole number of sectors, pad with virtual zeroes
* switch to jbuilder
* add a Dockerfile

## v2.7.0 (2017-02-19)
* add a simple file copy benchmark
* add bindings for readv, writev
* cache the file offset and avoid unnecessary calls to lseek

## v2.6.0 (2017-02-13)
* allow fsync to be used in preference to fcntl for flushing on macOS

## v2.5.0 (2017-01-17)
* use MirageOS 3 error scheme & new dependencies; drop support for earlier versions

## v2.4.0 (21-Sept-2016)
* add optional arguments with safe defaults to `Block.connect`.
* add a new Block.Config.t containing configuration parameters,
  serialisation and deserialisation
* fix appveyor build by removing cygwin curl

## v2.3.0 (2-Aug-2016)
* use fcntl(F_FULLFSYNC) on OSX rather than plain fsync
* add linux-headers depext for alpine
* hold the mutex around `ftruncate` while updating `size_sectors`
* hold the mutex around bounds checks
* require cstruct >= 1.3.0

## v2.2.0 (3-Mar-2016)
* Add basic Win32 support, tested via appveyor
* Use logs library
* Check buffers have proper lengths in `read` and `write`

## v2.1.0 (9-Nov-2015)
* Add `Block.resize` to resize file-backed devices
* Unit tests now work on OSX
* opam file improvements
* fail on read/write attempts after last sector
* add `Block.flush` for when operating in buffered mode
* add support for sparseness via `Block.seek_mapped` and `Block.seek_unmapped`

## v2.0.0 (27-Mar-2015):
* Incompatible API change: Block.blkgetsize takes an extra argument (a file descriptor)
* Support NetBSD through DIOCGMEDIASIZE
* Support rumprun by avoiding re-opening files, instead we use file descriptors internally

## v1.2.2 (07-Mar-2015)
* Expose an explicit `connect` function in the interface signature. (#20)
* Modernise Travis scripts with centralised ones.
* Add local `opam` file for OPAM 1.2 workflow.

## v1.2.1 (01-Feb-2014)
* Update to new io-page{,.unix} ocamlfind structure

## v1.2.0 (18-Dec-2013)
* Fix a serious race condition exposed when multiple threads access
  the same block device
* Block.connect: open in buffered mode if the filename has prefix "buffered:".
  The default is still to use unbuffered (like mirage-block-xen)

## v1.1.0 (16-Dec-2013)
* Expose the `blkgetsize` function in the external signature.
* Regenerate build files with OASIS 0.4.0

## v1.0.0 (08-Dec-2013)
* depend on mirage-types-0.4.0+
* catch low-level I/O errors and return an `Error
* Reformat with `ocp-indent` to deal with the outrageously large monitors that Citrix use.

0.1.1 (2-Dec-2013)
* add missing file

0.1.0 (1-Dec-2013)
* import first version of block driver from ocaml-fat
