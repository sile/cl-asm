(in-package :asdf)

(defsystem cl-asm
  :name "cl-asm"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A x86 assembler in Common Lisp"

  :serial t
  :components ((:file "package")
               (:file "cl-asm")))
