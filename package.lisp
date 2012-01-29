(defpackage cl-asm
  (:use :common-lisp)
  (:export assemble
           
           #+SBCL
           execute))
(in-package :cl-asm)
