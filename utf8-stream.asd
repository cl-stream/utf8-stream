;;
;;  utf8-stream  -  UTF-8 for cl-stream using babel
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :common-lisp-user)

(defpackage :utf8-stream.system
  (:use :common-lisp :asdf))

(in-package :utf8-stream.system)

(defsystem :utf8-stream
  :name "utf8-stream"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "UTF-8 for cl-stream using babel"
  :depends-on ("babel"
	       "cl-stream")
  :components
  ((:file "package")
   (:file "utf8-input-stream" :depends-on ("utf8-stream"))
   (:file "utf8-io-stream" :depends-on ("utf8-input-stream"
                                         "utf8-output-stream"))
   (:file "utf8-output-stream" :depends-on ("utf8-stream"))
   (:file "utf8-stream" :depends-on ("package"))))
