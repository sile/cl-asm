(in-package :cl-asm)

(defmacro sequence-to-alien-function (seq function-type-spec)
  `(loop WITH len = (length ,seq)
         WITH codes = (sb-alien:make-alien sb-alien:unsigned-char len)
         FOR i FROM 0 BELOW len 
     DO
     (setf (sb-alien:deref codes i) (elt ,seq i))
     FINALLY
     (return (sb-alien:cast codes ,function-type-spec))))
#|

pushq %rbp
movq %rsp, %rbp
movl %edi, -4(%rbp)
movl %esi, -8(%rbp)
movl -8(%rbp), %eax
movl -4(%rbp), %edx
addl %edx, %eax
popq %rbp
ret
|#

(defun flatten (x)
  (typecase x
    (null x)
    (atom (list x))
    (cons (destructuring-bind (car . cdr) x
            (append (flatten car) (flatten cdr))))))

(defun get-rd (sym)
  (ecase (intern (symbol-name sym) :cl-asm)
    ((%eax %rax) 0)
    ((%ecx %rcx) 1)
    ((%edx %rdx) 2)
    ((%ebx %rbx) 3)
    ((%esp %rsp) 4)
    ((%ebp %rbp) 5)
    ((%esi %rsi) 6)
    ((%edi %rdi) 7)))

(defun @push (arg)
  (if (= (length arg) 1)
      (+ #x50 (get-rd (car arg)))
    (error "unsupported")))

(defun @pop (arg)
  (if (= (length arg) 1)
      (+ #x58 (get-rd (car arg)))
    (error "unsupported")))

(defparameter +rex.w+ #x48)
(defparameter +mod.3+ #b11)

(defun mk-r/r-modrm (from to)
  (+ (ash +mod.3+       6)
     (ash (get-rd from) 3)
     (ash (get-rd to)   0)))
 
(defun mk-m/r-modrm (base to-reg)
  (+ (ash #b01            6)
     (ash (get-rd to-reg) 3)
     (ash (get-rd base)   0)))

(defun reg-p (sym)
  (and (symbolp sym)
       (member sym '(%rax %rcx %rdx %rbx %rsp %rbp %rsi %rdi
                     %eax %ecx %edx %ebx %esp %ebp %esi %edi) :test #'string=)))
                 
(defun r/r-p (arg)
  (and (= (length arg) 2)
       (reg-p (first arg)) 
       (reg-p (second arg))))


(defun m/r-p (arg)
  (and (= (length arg) 2)
       (consp (first arg))
       (reg-p (second arg))))

(defun to-ubyte (n)
  (ldb (byte 8 0) n))

(defun @mov (arg)
  (cond ((r/r-p arg)
         `(,+rex.w+ #x89 ,(mk-r/r-modrm (first arg) (second arg))))
        ((m/r-p arg) ; ex: (:mov (%rbp -4) %eax)
         (let* ((displacement (to-ubyte (second (first arg)))))
           `(#x8B ,(mk-m/r-modrm (caar arg) (second arg)) ,displacement)))
        (t
         (error "unsupported"))))

(defun @add (arg)
  (cond ((r/r-p arg)
         `(#x01 ,(mk-r/r-modrm (first arg) (second arg))))
        (t
         (error "unsupported"))))

(defun @sub (arg)
  (cond ((r/r-p arg)
         `(#x2b ,(mk-r/r-modrm (second arg) (first arg))))
        (t
         (error "unsupported"))))

(defun @cmp (arg)
  (cond ((r/r-p arg)
         `(#x39 ,(mk-r/r-modrm (second arg) (first arg))))
        (t
         (error "unsupported"))))

(defun @ret (arg)
  (declare (ignore arg))
  #xC3)

(defun int32-to-bytes (n)
  (loop FOR i FROM 0 BELOW 4
        COLLECT (ldb (byte 8 (* 8 i)) n)))

(defun @jmp-unresolve ()
  `(#xE9 0 0 0 0))

(defun @jmp (arg)
  (assert (and (= (length arg) 1)
               (integerp (first arg))))
  `(#xE9 ,@(int32-to-bytes (first arg)))) ; relative 32bit jump

(defun to-list (x) (if (listp x) x (list x)))

(defun assemble (mnemonics)
  (loop WITH labels = '()
        WITH unresolves = '()
        FOR mnemonic IN (mapcar #'to-list mnemonics)
    APPEND
    (if (not (keywordp (car mnemonic)))
        ;; label
        (progn (push (cons (car mnemonic) (length list)) labels)
               '())
      (to-list
       (ecase (car mnemonic)
         (:push (@push (cdr mnemonic)))
         (:pop (@pop (cdr mnemonic)))
         (:mov (@mov (cdr mnemonic)))
         (:add (@add (cdr mnemonic)))
         (:sub (@sub (cdr mnemonic)))
         (:cmp (@cmp (cdr mnemonic)))
         (:jmp (if (symbolp (second mnemonic))
                   (progn (push (cons (second mnemonic) (1+ (length list))) unresolves)
                          (@jmp-unresolve))
                 (@jmp (cdr mnemonic))))
         (:ret (@ret (cdr mnemonic))))))
    INTO list
    FINALLY
    (loop FOR (sym . offset) IN unresolves
          FOR pos = (- (cdr (assoc sym labels)) (+ 4 offset)) ; relative
          DO
          (setf (subseq list offset (+ offset 4)) (int32-to-bytes pos)))
    (return (flatten list))))

#+SBCL
(defmacro execute (mnemonics fn-type &rest args)
  (let ((fn (gensym)))
    `(let ((,fn (sequence-to-alien-function (assemble ,mnemonics) ,fn-type)))
       (unwind-protect
           (sb-alien:alien-funcall ,fn ,@args)
         (sb-alien:free-alien ,fn)))))
