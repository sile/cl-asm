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

(defun reg-code (sym)
  (ecase (intern (symbol-name sym) :cl-asm)
    ((%al %ax %eax %rax) 0)
    ((%cl %cx %ecx %rcx) 1)
    ((%dl %dx %edx %rdx) 2)
    ((%bl %bx %ebx %rbx) 3)
    ((%ah %sp %esp %rsp) 4)
    ((%ch %bp %ebp %rbp) 5)
    ((%dh %si %esi %rsi) 6)
    ((%bh %di %edi %rdi) 7)))

(defun @push (arg)
  (assert (= 1 (length arg)))
  (etypecase #1=(car arg)
    (symbol (+ #x50 (get-rd #1#)))
    (integer `(#x68 ,@(int32-to-bytes #1#)))))

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

(defun i/r-p (arg)
  (and (= (length arg) 2)
       (integerp (first arg))
       (reg-p (second arg))))

(defun i/eax-p (arg)
  (and (= (length arg) 2)
       (integerp (first arg))
       (member (second arg) '(%eax %ax %al) :test #'string=)))
  
(defun to-ubyte (n)
  (ldb (byte 8 0) n))

(defun reg-type-of (reg)
  (ecase (intern (symbol-name reg) :cl-asm)
    ((%al %cl %bl %bl %ah %ch %dh %bh)         :r8)
    ((%ax %cx %dx %bx %sp %bp %si %di)         :r16)
    ((%eax %ecx %edx %ebx %esp %ebp %esi %edi) :r32)
    ((%rax %rcx %rdx %rbx %rsp %rbp %rsi %rdi) :r64)))

(defun operand-type-of (operand)
  (etypecase operand
    (integer 
     (let ((len (integer-length operand)))
       (cond ((< len 08) :imm8)
             ((< len 16) :imm16)
             ((< len 32) :imm32)
             ((< len 64) :imm64)
             (t (error "unsupported")))))
    (symbol 
     (if (char/= #\% (char (symbol-name operand) 0))
         :address
       (reg-type-of operand)))
    (cons 
     (destructuring-bind (tag a &optional b) operand
       (ecase tag
         (:ref
          (if (and (null b)
                   (integerp a)
                   (>= (integer-length a) 32))
              :m64
            :m))
         ((:imm8 :imm16 :imm32 :imm64) tag))))))

(defun modrm (mod r/m reg)
  (+ (ash mod 6)
     (ash reg 3)
     (ash r/m 0)))

;; TODO: r/m-modrmと統合可能
(defun r/r-modrm (from to)
  (modrm #b11 (reg-code to) (reg-code from)))

(defparameter +sib.none+ #x25)

(defun r/m-modrm (from to)
  (destructuring-bind (tag a1 &optional a2) to
    (declare (ignore tag))
    (etypecase a1
      (integer 
       (assert (null a2))
       `(,(modrm #b00 #b100 (reg-code from)) ,+sib.none+ ,@(int32-to-bytes a1)))
      (symbol
       (if (null a2)
           (let ((cd (reg-code a1)))
             (assert (member cd '(0 1 2 3 6 7)))
             (modrm #b00 cd (reg-code from)))
         (let ((cd (reg-code a1)))
           (assert (member cd '(0 1 2 3 4 6 7)))
           (ecase (operand-type-of a2)
             (:imm8 `(,(modrm #b01 cd (reg-code from)) ,(to-ubyte a2)))
             ((:imm16 :imm32) `(,(modrm #b10 cd (reg-code from)) ,@(int32-to-bytes a2)))))))
      )))

(defun mk-modrm (from to)
  (if (and (symbolp from) (symbolp to))
      (r/r-modrm from to)
    (if (symbolp from)
        (r/m-modrm from to)
      (r/m-modrm to from))))
     
(defun reg-a-p (reg)
  (member reg '(%al %ax %eax %rax) :test #'string=))

(defun int-to-bytes (width n)
  (loop FOR i FROM 0 BELOW width
        COLLECT (ldb (byte 8 (* i 8)) n)))

;; TODO
(defun mk-mov-op (base opd &optional (offset 1))
  (ecase (operand-type-of opd)
    ((:r8 :imm8) `(,base))
    ((:r16 :imm16) `(#x66 ,(+ base offset)))
    ((:r32 :imm32) `(,(+ base offset)))
    ((:r64 :imm64) `(,+rex.w+ ,(+ base offset)))))

(defmacro def-ins (name args &body body)
  (let ((arg (gensym)))
    `(defun ,name (,arg)
       ;; TODO: error message
       (destructuring-bind ,args ,arg
         (flatten (locally ,@body))))))

(case x
  (:reg->xxx)
  (:mem->xxx)
  (:imm->xxx)
  (:reg->mem64)
  (:mem64->xxx))

(def-ins @mov (from to)
  (ecase (bin-type-of from to)
    (:reg->xxx 
     `(,(mk-mov-op #x88 from) ,(mk-modrm from to)))
    (:mem->xxx 
     `(,(mk-mov-op #x8B to) ,(mk-modrm from to)))
    (:imm->xxx 
     (ecase (operand-type-of to)
       (:m `(,(mk-mov-op #xC6 from) ,(mk-modrm '%ax to) ,(int-to-bytes len n))) ; TODO: len:max4
       (:reg `(,(mk-mov-op (+ #xb0 (reg-code to)) to 8) ,(int-to-bytes len from))) ; TODO: len
       ))
    (:reg->mem64 
     `(,(mk-mov-op #xA2 from) ,@(int-to-bytes 8 (second to))))
    (:mem64->xxx
     `(,(mk-mov-op #xA0 to) ,(int-to-bytes 8 (second from))))))

(defun @add (arg)
  (cond ((r/r-p arg)
         `(#x01 ,(mk-r/r-modrm (first arg) (second arg))))
        ((i/eax-p arg)
         (let ((len (integer-length (first arg))))
           (cond ((< len  8) `(#x04 ,(to-ubyte (first arg))))
                 (t (error "unsupported")))))
        (t
         (error "unsupported"))))

(defun @sub (arg)
  (cond ((r/r-p arg)
         `(#x29 ,(mk-r/r-modrm (first arg) (second arg))))
        (t
         (error "unsupported"))))

(defun @cmp (arg)
  (cond ((r/r-p arg)
         `(#x39 ,(mk-r/r-modrm (first arg) (second arg))))
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

(defun jmp-cond (cond)
  (ecase cond
    (:<  #x77)
    (:<= #x73)
    (:>  #x72)
    (:>= #x76)
    (:=  #x74)
    (:zero #x74) ; XXX
    (:/= #x75)
    ))

(defun @jmp-if-unresolve (arg)
  `(,(jmp-cond (first arg)) 0)) ; relative 8bit jump

(defun @jmp-if (arg)
  `(,(jmp-cond (first arg)) ,(second arg)))

(defun to-list (x) (if (listp x) x (list x)))

#+SBCL
(defun extern-fn-ptr (name)
  (let ((fn (eval `(sb-alien:extern-alien ,name (function sb-alien:int)))))
    (sb-sys:sap-int (sb-alien:alien-sap fn))))

(defun @call (arg)
  (assert (= 1 (length arg)))
  (typecase #1=(first arg)
    (integer `(,#xE8 ,@(int32-to-bytes #1#)))
    (cons
     (destructuring-bind (tag name) #1#
       (assert (eq tag :extern))
       (let ((ptr (extern-fn-ptr name)))
         (append (@mov `(,ptr %eax)) ; TODO: 64bit mov
                 `(#xFF ,#b11010000))))))) ; /2 = 010

(defun @call-unresolve ()
  `(,#xE8 0 0 0 0))

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
         (:call (if (symbolp (second mnemonic))
                    (progn (push (list (second mnemonic) (1+ (length list)) 4) unresolves)
                           (@call-unresolve))
                  (@call (cdr mnemonic))))
         (:jmp (if (symbolp (second mnemonic))
                   (progn (push (list (second mnemonic) (1+ (length list)) 4) unresolves)
                          (@jmp-unresolve))
                 (@jmp (cdr mnemonic))))
         (:jmp-if (if (symbolp (third mnemonic))
                      (progn (push (list (third mnemonic) (1+ (length list)) 1) unresolves)
                             (@jmp-if-unresolve (cdr mnemonic)))
                    (@jmp-if (cdr mnemonic))))
         (:ret (@ret (cdr mnemonic))))))
    INTO list
    FINALLY
    (loop FOR (sym offset len) IN unresolves
          FOR pos = (- (cdr (assoc sym labels)) (+ len offset)) ; relative
          DO
          (ecase len
            (4 (setf (subseq list offset (+ offset 4)) (int32-to-bytes pos)))
            (1 (setf (nth offset list) pos))))
             
    (return (flatten list))))

#+SBCL
(defmacro execute (mnemonics fn-type &rest args)
  (let ((fn (gensym)))
    `(let ((,fn (sequence-to-alien-function (assemble ,mnemonics) ,fn-type)))
       (unwind-protect
           (sb-alien:alien-funcall ,fn ,@args)
         (sb-alien:free-alien ,fn)))))
