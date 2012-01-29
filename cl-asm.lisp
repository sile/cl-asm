(in-package :cl-asm)
#|
(defmacro seq-to-alien-function (seq function-type-spec)
  `(loop WITH len = (length ,seq)
         WITH codes = (sb-alien:make-alien unsigned-char len)
         FOR i FROM 0 BELOW len 
     DO
     (setf (sb-alien:deref codes i) (elt ,seq i))
     FINALLY
     (return (sb-alien:cast codes ,function-type-spec))))

(defun hex-to-seq (s)
  (loop FOR i FROM 0 BELOW (length s) BY 2
        COLLECT (parse-integer s :radix 16 :start i :end (+ i 2))))

(hex-to-seq "554889E5897DFC8975F88B45F88B55FC01D05DC3")

|#