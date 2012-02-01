(in-package :cl-asm)

;;; TODO: 32bit対応

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; types
(deftype byte-width () '(member 1 2 4 8))
(deftype address () 'integer)

(defstruct operand)
(defstruct (destination (:include operand))
  (size 0 :type byte-width))

(defstruct (reg (:include destination)) ;; XXX: incorrect-name
  (name t :type symbol)
  (code 0 :type (mod 8)))

(defstruct (label (:include operand))
  (name t :type symbol)
  (addr 0 :type address))

(defstruct (imm (:include operand))
  (size 0 :type byte-width)
  (value 0 :type integer))

(defstruct (mem (:include destination)))

(defstruct (mem-direct (:include mem))
  (addr 0 :type address))

(defstruct (mem-indirect (:include mem))
  (reg    0 :type reg)
  (offset 0 :type address))

(defparameter *regs*
  '((%al 1 0) (%ax 2 0) (%eax 4 0) (%rax 8 0)
    (%cl 1 1) (%cx 2 1) (%ecx 4 1) (%rcx 8 1)
    (%dl 1 2) (%dx 2 2) (%edx 4 2) (%rdx 8 2)
    (%bl 1 3) (%bx 2 3) (%ebx 4 3) (%rbx 8 3)
    (%ah 1 4) (%sp 2 4) (%esp 4 4) (%rsp 8 4)
    (%ch 1 5) (%bp 2 5) (%ebp 4 5) (%rbp 8 5)
    (%dh 1 6) (%si 2 6) (%esi 4 6) (%rsi 8 6)
    (%bh 1 7) (%di 2 7) (%edi 4 7) (%rdi 8 7)))

(defmacro sequence-to-alien-function (seq function-type-spec)
  `(loop WITH len = (length ,seq)
         WITH codes = (sb-alien:make-alien sb-alien:unsigned-char len)
         FOR i FROM 0 BELOW len 
     DO
     (setf (sb-alien:deref codes i) (elt ,seq i))
     FINALLY
     (return (sb-alien:cast codes ,function-type-spec))))

(defun flatten (x)
  (typecase x
    (null x)
    (atom (list x))
    (cons (destructuring-bind (car . cdr) x
            (append (flatten car) (flatten cdr))))))

(defparameter +rex.w+ #x48)
(defparameter +sib.none+ #x25)

(defun int-to-bytes (width n)
  (loop FOR i FROM 0 BELOW width
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defmacro def-ins (name args &body body)
  (let ((arg (gensym)))
    `(defun ,name (,arg)
       ;; TODO: error message
       (destructuring-bind ,args ,arg
         (flatten (locally (to-list ,@body)))))))

;; XXX: 分かりにくい名前
(defun mem64-p (operand)
  (and (mem-direct-p operand)
       (> (integer-length (mem-direct-addr operand)) 32)))

(defun r/m-p (operand)
  (or (reg-p operand) (mem-p operand)))

(defun make-mod-r/m (mod reg/opcode r/m)
  (+ (ash mod 6) (ash reg/opcode 3) r/m))

(defun make-x/m-mod-r/m (op/reg-code mem)
  (etypecase mem
    (mem-direct 
     (list (make-mod-r/m #b00 op/reg-code #b100)
           +SIB.NONE+ (int-to-bytes 4 (mem-direct-addr mem))))
    (mem-indirect
     (let ((mem-off (mem-indirect-offset mem))
           (mem-reg-code (reg-code (mem-indirect-reg mem))))
     (if (zerop mem-off)
         (make-mod-r/m #b00 op/reg-code mem-reg-code)
       (multiple-value-bind (mod addr-size)
                            (ecase (imm-byte-width mem-off)
                              (1     (values #b01 1))
                              ((2 4) (values #b10 4)))
         (list (make-mod-r/m mod op/reg-code mem-reg-code) (int-to-bytes addr-size mem-off))))))))

(defun make-r/m-mod-r/m (reg mem)
  (make-x/m-mod-r/m (reg-code reg) mem))

(defun make-o/m-mod-r/m (opcode mem)
  (make-x/m-mod-r/m opcode mem))

(defun mod-r/m (reg/opcode r/m)
  (flet ((mod-type-of (r/o r/m)
           (cond ((and (integerp r/o) (mem-p r/m)) :o/m)
                 ((and (reg-p r/o) (reg-p r/m)) :r/r)
                 ((and (reg-p r/o) (mem-p r/m)) :r/m)
                 (t (error "unsupported")))))
    (ecase (mod-type-of reg/opcode r/m)
      (:r/r (make-mod-r/m #b11 (reg-code reg/opcode) (reg-code r/m)))
      (:r/m (make-r/m-mod-r/m reg/opcode r/m))
      (:o/m (make-o/m-mod-r/m reg/opcode r/m))
      )))

(defun reg@a-p (operand)
  (and (reg-p operand)
       (= 0 (reg-code operand))))

(def-ins @mov (dst src)
  (flet ((operands-type-of (dst src)
           (cond ((and (mem64-p dst) (reg@a-p src)) :mem64<-reg@a)
                 ((and (reg@a-p dst) (mem64-p src)) :reg@a<-mem64)
                 ((and (r/m-p dst) (reg-p src))   :r/m<-reg)
                 ((and (r/m-p dst) (mem-p src))   :r/m<-mem)
                 ((and (mem-p dst) (imm-p src))   :mem<-imm)
                 ((and (reg-p dst) (imm-p src))   :reg<-imm)
                 (t (error "unsupported"))))
         
         (op (base &optional (offset 1))
           (assert (or (not (destination-p src)) 
                       (= (destination-size dst) (destination-size src))))
           (case (destination-size dst)
             (1 base)
             (2 (list #x66 (+ base offset)))
             (4 (+ base offset))
             (8 (list +REX.W+ (+ base offset)))))

         (disp (n &optional (size (destination-size dst)))
           (int-to-bytes size n)))

    (ecase (operands-type-of dst src)
      (:r/m<-reg (list (op #x88) (mod-r/m src dst)))
      (:r/m<-mem (list (op #x8B) (mod-r/m dst src)))
      (:reg<-imm (list (op (+ #xB0 (reg-code dst)) 8) (disp (imm-value src))))
      (:mem<-imm (list (op #xC6) (mod-r/m 0 dst) (disp (imm-value src))))
      (:mem64<-reg@a (list (op #xA2) (disp (mem-direct-addr dst) 8))) ; XXX: 32bit対応
      (:reg@a<-mem64 (list (op #xA0) (disp (mem-direct-addr src) 8))) ; XXX: 32bit対応
      )))

;; TODO: extend
(def-ins @ret ()
  #xC3)


(flet ((operand-type-of (o)
         (cond ((and (mem-p o) (= 2 (mem-size o))) :mem16)
               ((and (mem-p o) (= 8 (mem-size o))) :mem64)
               ((and (reg-p o) (= 2 (reg-size o))) :reg16)
               ((and (reg-p o) (= 8 (reg-size o))) :reg64)
               ((and (imm-p o) (= 1 (imm-size o))) :imm8)
               ((and (imm-p o) (= 2 (imm-size o))) :imm16)
               ((and (imm-p o) (= 4 (imm-size o))) :imm32)
               (t (error "unsupported")))))

  (def-ins @push (src)
    (ecase (operand-type-of src)
      (:mem16 (list #x66 #xFF (mod-r/m 6 src)))
      (:mem64 (list #xFF (mod-r/m 6 src)))
      (:reg16 (list #x66 (+ #x50 (reg-code src))))
      (:reg64 (list (+ #x50 (reg-code src))))
      (:imm8  (list #x6A (int-to-bytes 1 (imm-value src))))
      (:imm16 (list #x66 #x68 (int-to-bytes 2 (imm-value src))))
      (:imm32 (list #x68 (int-to-bytes 4 (imm-value src))))))

  (def-ins @pop (dst)
    (ecase (operand-type-of dst)
      (:mem16 (list #x66 #x8F (mod-r/m 0 dst)))
      (:mem64 (list #x8F (mod-r/m 0 dst)))
      (:reg16 (list #x66 (+ #x58 (reg-code dst))))
      (:reg64 (list (+ #x58 (reg-code dst)))))))

(defun to-list (x) (if (listp x) x (list x)))

#+SBCL
(defun extern-fn-ptr (name)
  (let ((fn (eval `(sb-alien:extern-alien ,name (function sb-alien:int)))))
    (sb-sys:sap-int (sb-alien:alien-sap fn))))

(defun operand-label-p (operand)
  (and (symbolp operand)
       (char= #\& (char (symbol-name operand) 0))))

(defun operand-register-p (operand)
  (and (symbolp operand)
       (member operand *regs* :key #'first :test #'string=)))

(defun mnemonic-label-p (mnemonic)
  (and (= 1 (length mnemonic))
       (operand-label-p (first mnemonic))))

(defun mnemonic-instruction-p (mnemonic)
  (keywordp (first mnemonic)))

;; XXX: name
(defun imm-byte-width (n)
  (let ((len (integer-length n)))
    (cond ((< len 8) 1)
          ((< len 16) 2)
          ((< len 32) 4)
          ((< len 64) 8)
          (t (error "unsupported")))))
           
(defun ref-imm-p (args)
  (and (= 1 (length args))
       (integerp (first args))))

(defun ref-extern-p (args &aux (fst (first args)))
  (and (= 1 (length args))
       (consp fst)
       (eq (first fst) :extern)
       (stringp (second fst))))

(defun ref-register-p (args)
  (ignore-errors
    (destructuring-bind (reg &optional (offset 0)) args
      (and (operand-register-p reg)
           (integerp offset)))))

(defun extern-address (name)
  (declare (ignore name))
  (error "TODO")) ; TODO

(defun to-operand (operand)
  (etypecase operand
    (integer (make-imm :size (imm-byte-width operand) :value operand))
    (symbol  (cond ((operand-label-p operand)
                    (make-label :name operand))
                   ((operand-register-p operand)
                    (destructuring-bind (name size code) 
                                        (assoc operand *regs* :test #'string=)
                      (make-reg :name name :size size :code code)))
                   (t
                    (error "unsupported"))))
    (cons
     (destructuring-bind (tag . args) operand
       (ecase tag
         ((:immb :immw :immd :immq)
          (assert (and (= 1 (length args))
                       (integerp (first args))))
          (let ((size (ecase tag (:immb 1) (:immw 2) (:immd 4) (:immq 8))))
            (make-imm :size size :value (first args))))
         ((:refb :refw :refd :refq)
          (let ((size (case tag (:refb 1) (:refw 2) (:refd 4) (:refq 8))))
            (cond ((ref-imm-p args)
                   (make-mem-direct :size size :addr (first args)))
                  ((ref-extern-p args)
                   (make-mem-direct :size size :addr (extern-address (second (first args)))))
                  ((ref-register-p args)
                   (destructuring-bind (reg &optional (offset 0)) args
                     ;; TODO: check size of register
                     (make-mem-indirect :size size :reg (to-operand reg) :offset offset)))
                  (t
                   (error "unsupported"))))))))))

(defun assemble-instruction (opcode operands)
  (ecase opcode
    (:mov (@mov operands))
    (:ret (@ret operands))
    (:push (@push operands))
    (:pop (@pop operands))
    ))

(defun assemble (mnemonics)
  (loop WITH labels = '()
        ;WITH unresolves = '()
        FOR mnemonic IN (mapcar #'to-list mnemonics)
    APPEND
    (cond ((mnemonic-label-p mnemonic)
           (push (make-label :name (first mnemonic)) labels)
           '())
          ((mnemonic-instruction-p mnemonic)
           (destructuring-bind (opcode . operands) mnemonic
             (assemble-instruction opcode (mapcar #'to-operand operands))))
          (t
           (error "unsupported")))
    INTO list
    FINALLY
    (return list)))

#+SBCL
(defmacro execute (mnemonics fn-type &rest args)
  (let ((fn (gensym)))
    `(let ((,fn (sequence-to-alien-function (assemble ,mnemonics) ,fn-type)))
       (unwind-protect
           (sb-alien:alien-funcall ,fn ,@args)
         (sb-alien:free-alien ,fn)))))
