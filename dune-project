(lang dune 3.0)
(name mirage-block-unix)

(formatting disabled)
(generate_opam_files true)

(source (github mirage/mirage-block-unix))
(authors "Dave Scott <dave@recoil.org>")
(maintainers "dave@recoil.org")
(documentation "https://mirage.github.io/mirage-block-unix/")
(license "ISC")

(package
 (name mirage-block-unix)
 (synopsis "MirageOS disk block driver for Unix")
 (description "\
Unix implementation of the Mirage `BLOCK_DEVICE` interface.

This module provides raw I/O to files and block devices with as little caching as possible.")
 (depends
  (ocaml (>= "4.06.0"))
  (cstruct (>= "6.0.0"))
  cstruct-lwt
  (mirage-block (>= "2.0.0"))
  rresult
  (uri (>= "1.9.0"))
  logs
  (lwt (>= "5.4.2"))
  (ounit2 :with-test)
  (diet (and :with-test (>= "0.4")))
  (fmt :with-test)
  (conf-linux-libc-dev (= :os "linux")))
 (tags (org:mirage)))
