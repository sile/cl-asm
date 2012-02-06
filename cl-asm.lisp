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
  (pos  0 :type fixnum)
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

(defmacro sequence-to-alien-function (seq-form function-type-spec &aux (seq (gensym)))
  `(loop WITH ,seq = ,seq-form
         WITH len = (length ,seq)
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
         (flatten (to-list (locally ,@body)))))))

;; XXX: 分かりにくい名前
(defun mem64-p (operand)
  (and (mem-direct-p operand)
       (> (integer-length (mem-direct-addr operand)) 32)))

(defun r/m-p (operand)
  (or (reg-p operand) (mem-p operand)))

(defun make-mod-r/m (mod reg/opcode r/m)
  (+ (ash mod 6) (ash reg/opcode 3) r/m))

(defun sib.none (reg-code)
  (ecase reg-code
    (#b000 #x20)
    (#b001 #x21)
    (#b010 #x22)
    (#b011 #x23)
    (#b100 #x24)
    (#b110 #x25)
    (#b111 #x26)))

(defun make-x/m-mod-r/m (op/reg-code mem)
  (etypecase mem
    (mem-direct 
     (list (make-mod-r/m #b00 op/reg-code #b100)
           +SIB.NONE+ (int-to-bytes 4 (mem-direct-addr mem))))
    (mem-indirect
     (let ((mem-off (mem-indirect-offset mem))
           (mem-reg-code (reg-code (mem-indirect-reg mem))))
     (if (zerop mem-off)
         (list (make-mod-r/m #b00 op/reg-code mem-reg-code) (sib.none mem-reg-code))
       (multiple-value-bind (mod addr-size)
                            (ecase (imm-byte-width mem-off)
                              (1     (values #b01 1))
                              ((2 4) (values #b10 4)))
         (list (make-mod-r/m mod op/reg-code mem-reg-code) 
               (sib.none mem-reg-code) (int-to-bytes addr-size mem-off))))))))

(defun make-r/m-mod-r/m (reg mem)
  (make-x/m-mod-r/m (reg-code reg) mem))

(defun make-o/m-mod-r/m (opcode mem)
  (make-x/m-mod-r/m opcode mem))

(defun mod-r/m (reg/opcode r/m)
  (flet ((mod-type-of (r/o r/m)
           (cond ((and (integerp r/o) (mem-p r/m)) :o/m)
                 ((and (integerp r/o) (reg-p r/m)) :o/r)
                 ((and (reg-p r/o) (reg-p r/m)) :r/r)
                 ((and (reg-p r/o) (mem-p r/m)) :r/m)
                 (t (error "unsupported")))))
    (ecase (mod-type-of reg/opcode r/m)
      (:r/r (make-mod-r/m #b11 (reg-code reg/opcode) (reg-code r/m)))
      (:o/r (make-mod-r/m #b11 reg/opcode (reg-code r/m)))
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
      (:r/m<-mem (list (op #x8A) (mod-r/m dst src)))
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


(flet ((operands-type-of (d s)
         (cond ((and (r/m-p d) (> (destination-size d) 1)
                     (imm-p s) (= (imm-size s) 1)) :r/m<-imm8)
               ((and (reg@a-p d) (imm-p s)) :reg@a<-imm)
               ((and (r/m-p d) (imm-p s)) :r/m<-imm)
               ((and (r/m-p d) (reg-p s)) :r/m<-reg)
               ((and (reg-p d) (r/m-p s)) :reg<-r/m)
               (t (error "unsupported"))))

       (!op (base dst src &aux (offset 1))
         (assert (or (not (destination-p src)) 
                     (= (destination-size dst) (destination-size src))))
         (case (destination-size dst)
           (1 base)
           (2 (list #x66 (+ base offset)))
           (4 (+ base offset))
           (8 (list +REX.W+ (+ base offset)))))
       
       (!disp (n dst &optional (size (destination-size dst)) &aux (max 4))
         (int-to-bytes (min size max) n)))
  (macrolet ((op (base) ; XXX
               `(!op ,base dst src))
             (disp (n &optional size)
               (if size
                   `(!disp ,n dst ,size)
                 `(!disp ,n dst))))

    (def-ins @add (dst src)
      (ecase (operands-type-of dst src)
        (:reg@a<-imm (list (op #x04) (disp (imm-value src))))
        (:r/m<-imm (list (op #x80) (mod-r/m 0 dst) (disp (imm-value src))))
        (:r/m<-imm8 (list (op #x82) (mod-r/m 0 dst) (disp (imm-value src) 1)))
        (:r/m<-reg (list (op #x00) (mod-r/m src dst)))
        (:reg<-r/m (list (op #x02) (mod-r/m dst src)))))

    (def-ins @sub (dst src)
      (ecase (operands-type-of dst src)
        (:reg@a<-imm (list (op #x2C) (disp (imm-value src))))
        (:r/m<-imm (list (op #x80) (mod-r/m 5 dst) (disp (imm-value src))))
        (:r/m<-imm8 (list (op #x82) (mod-r/m 5 dst) (disp (imm-value src) 1)))
        (:r/m<-reg (list (op #x28) (mod-r/m src dst)))
        (:reg<-r/m (list (op #x2A) (mod-r/m dst src)))))

    (def-ins @cmp (dst src)
      (ecase (operands-type-of dst src)
        (:reg@a<-imm (list (op #x3C) (disp (imm-value src))))
        (:r/m<-imm (list (op #x80) (mod-r/m 7 dst) (disp (imm-value src))))
        (:r/m<-imm8 (list (op #x82) (mod-r/m 7 dst) (disp (imm-value src) 1)))
        (:r/m<-reg (list (op #x38) (mod-r/m src dst)))
        (:reg<-r/m (list (op #x3A) (mod-r/m dst src)))))))

(flet ((op (code dst)
         (when (not (r/m-p dst))
           (error "unsupported"))
         (ecase (destination-size dst)
           (1 (list #xFE (mod-r/m code dst)))
           (2 (list #x66 #xFF (mod-r/m code dst)))
           (4 (list #xFF (mod-r/m code dst)))
           (8 (list +REX.W+ #xFF (mod-r/m code dst))))))

  (def-ins @inc (dst) (op 0 dst))
  (def-ins @dec (dst) (op 1 dst)))

(defun jcc-cond-op (cnd imm)
  (multiple-value-bind (code imm32-compatible)
                       (ecase cnd
                         (:jo (values #x70 t))
                         (:jno (values #x71 t))
                         ((:jb :jc :jnae) (values #x72 t))
                         ((:jae :jnb :jnc) (values #x73 t))
                         ((:je :jz) (values #x74 t))
                         ((:jne :jnz) (values #x75 t))
                         ((:jbe :jna) (values #x76 t))
                         ((:ja :jnbe) (values #x77 t))
                         (:js (values #x78 t))
                         (:jns (values #x79 t))
                         ((:jp :jpe) (values #x7A t))
                         ((:jnp :jpo) (values #x7B t))
                         ((:jl :jnge) (values #x7C t))
                         ((:jge :jnl) (values #x7D t))
                         ((:jle :jng) (values #x7E t))
                         ((:jg :jnle) (values #x7F t))
                         (:jecxz (values #xE3 nil))
                         (:jrcxz (values '(#x67 #xE3) nil)))
    (ecase (imm-size imm)
      (1 code)
      (4 (if imm32-compatible
             (+ code #x10)
           (error "unsupported"))))))

(flet ((operand-type-of (o)
         (cond ((and (imm-p o) (=  (imm-size o) 1)) :imm8)
               ((and (imm-p o) (<= (imm-size o) 4)) :imm32)
               ((and (r/m-p o) (= (destination-size o) 8)) :r/m64)
               (t (error "unsupported")))))

  (def-ins @call (dst)
    (ecase (operand-type-of dst)
      ((:imm8 :imm32) (list #xE8 (int-to-bytes 4 (imm-value dst))))
      (:r/m64 (list #xFF (mod-r/m 2 dst)))))

  (def-ins @jmp (dst)
    (ecase (operand-type-of dst)
      (:imm8  (list #xEB (int-to-bytes 1 (imm-value dst))))
      (:imm32 (list #xE9 (int-to-bytes 4 (imm-value dst))))
      (:r/m64 (list #xFF (mod-r/m 4 dst)))))

  (def-ins @jcc (opcode dst)
    (ecase (operand-type-of dst)
      (:imm8  (list (jcc-cond-op opcode dst) (int-to-bytes 1 (imm-value dst))))
      (:imm32 (list (jcc-cond-op opcode dst) (int-to-bytes 4 (imm-value dst)))))))

(def-ins @setcc (opcode dst)
  (unless (and (r/m-p dst) (= (destination-size dst) 1))
    (error "unsupported"))
  (let ((cnd (intern (concatenate 'string "J" (subseq (symbol-name opcode) 3)) :keyword)))
    (list #x0F (+ (jcc-cond-op cnd (make-imm :size 1)) #x20) (mod-r/m 0 dst))))

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
  (let ((fn (eval `(sb-alien:extern-alien ,name (function sb-alien:int)))))
    (sb-sys:sap-int (sb-alien:alien-sap fn))))

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
         (:extern
          (assert (= 1 (length args)))
          (make-imm :size 8 :value (extern-address (first args))))
         ((:immb :immw :immd :immq)
          (assert (and (= 1 (length args))
                       (integerp (first args))))
          (let ((size (ecase tag (:immb 1) (:immw 2) (:immd 4) (:immq 8))))
            (make-imm :size size :value (first args))))
         ((:refb :refw :refd :refq); TODO: (:size16 (:ref ...))とかを必要な時だけ行えば良いようにする
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

(def-ins @hlt ()
  #xF4)

(def-ins @nop (&optional dst)
  (if (null dst)
      #x90
    (cond ((and (r/m-p dst) (= (destination-size dst) 2)) 
           (list #x66 #x0F #x1F (mod-r/m 0 dst)))
          ((and (r/m-p dst) (= (destination-size dst) 4))
           (list #x0F #x1F (mod-r/m 0 dst)))
          (t
           (error "unsupported")))))

(defun assemble-instruction (opcode operands)
  (ecase opcode
    (:nop (@nop operands))
    (:hlt (@hlt operands))
    (:mov (@mov operands))
    (:ret (@ret operands))
    (:push (@push operands))
    (:pop (@pop operands))
    (:add (@add operands))
    (:sub (@sub operands))
    (:inc (@inc operands))
    (:dec (@dec operands))
    (:cmp (@cmp operands))
    (:jmp (@jmp operands))
    (:call (@call operands))
    ((:ja :jae :jb :jbe :jc :je :jecxz :jg :jge :jl :jle :jna :jnae :jnb :jnbe :jnc
      :jne :jng :jnge :jnl :jnle :jno :jnp :jns :jnz :jo :jp :jpe :jpo :jrcxz :js
      :jz) (@jcc (cons opcode operands)))
    ((:seta :setae :setb :setbe :setc :sete :setecxz :setg :setge :setl :setle 
      :setna :setnae :setnb :setnbe :setnc
      :setne :setng :setnge :setnl :setnle :setno :setnp :setns :setnz 
      :seto :setp :setpe :setpo :setrcxz :sets
      :setz) (@setcc (cons opcode operands)))
    ))

(defparameter *op-label-defs*
  `(
    (:call 1 4 ((4 4) (2 4) (1 4)))
    (:jmp #|index|# 1 #|default-imm-size|# 4 #|disp-len-map|# ((4 4) (2 4) (1 1)))

    (:jrcxz 1 1 ((1 2)))
    (:jecxz 1 1 ((1 1)))
    ,@(mapcar (lambda (op)
                `(,op #|index|# 1 #|default-imm-size|# 4 #|disp-len-map|# ((4 4) (2 4) (1 1))))
              '(:ja :jae :jb :jbe :jc :je :jg :jge :jl :jle :jna :jnae :jnb :jnbe :jnc
                :jne :jng :jnge :jnl :jnle :jno :jnp :jns :jnz :jo :jp :jpe :jpo :js :jz))
    ))

(defun mnemonic-unresolve-p (mnemonic)
  (and (mnemonic-instruction-p mnemonic)
       (let ((def (assoc (first mnemonic) *op-label-defs*)))
         (when def
           (mnemonic-label-p (list (nth (second def) mnemonic)))))))

(defun adjust-pos (pos diffs &optional (include t))
  (+ pos
     (loop FOR (_ p d) IN diffs
           WHILE (if include (<= p pos) (< p pos))
           SUM d)))

(defun ninsert (list pos list2)
  (append (subseq list 0 (1- pos))
          list2
          (subseq list pos)))

;; XXX: name
(defun expand-progn (mnemonics)
  (loop FOR m IN mnemonics
    APPEND
    (progn
      (loop WHILE (not (or (and (consp m) (keywordp (car m)))
                           (keywordp m)
                           (operand-label-p m)))
            DO (setf m (eval m)))
                           
      (if (and (consp m) (eq (car m) :progn))
          (expand-progn (cdr m))
        (list m)))))

(defun assemble (mnemonics)
  (macrolet ((label-addr (name)  `(gethash ,name label-addr))
             (pc () `(length list))
             (get-label (mnemonic) `(nth (second (assoc (first ,mnemonic) *op-label-defs*))
                                         ,mnemonic))
             (get-ins-size (mnemonic &optional imm-size)
               `(let ((def (assoc (first ,mnemonic) *op-label-defs*)))
                  (second (assoc (or ,imm-size (third def)) (fourth def))))))

  (loop WITH label-addr = (make-hash-table)
        WITH unresolves = '()
        FOR mnemonic IN (mapcar #'to-list (expand-progn mnemonics))
    APPEND
    (cond ((mnemonic-label-p mnemonic)
           (setf (label-addr (first mnemonic)) (pc))
           '())
          ((mnemonic-unresolve-p mnemonic)
           (push (list mnemonic (1+ (pc)) (get-ins-size mnemonic) nil) unresolves)
           `(,(first mnemonic)))
          ((mnemonic-instruction-p mnemonic)
           (destructuring-bind (opcode . operands) mnemonic
             (assemble-instruction opcode (mapcar #'to-operand operands))))
          (t
           (error "unsupported")))
    INTO list

    FINALLY
    ;; ラベル参照解決
    ;; TODO: 整理
    (setf unresolves (reverse unresolves))
    (loop WHILE
      (loop WITH changed = nil
            FOR unresolve IN unresolves
            FOR (mnemonic pos ins-size _) = unresolve
            FOR label-pos = (label-addr (get-label mnemonic))
            FOR offset = (- (adjust-pos label-pos unresolves) (adjust-pos pos unresolves))
            FOR new-ins-size = (get-ins-size mnemonic (imm-byte-width offset))
        DO
        (setf (fourth unresolve) offset
              (third unresolve) new-ins-size
              changed (or changed (/= ins-size new-ins-size)))
        FINALLY
        (return changed)))

    (loop FOR (mnemonic pos ins-size offset) IN unresolves
      DO
      (setf (get-label mnemonic) offset
            list (ninsert 
                  list
                  (adjust-pos pos unresolves nil)
                  (assemble-instruction (car mnemonic) 
                                        (mapcar #'to-operand (cdr mnemonic))))))
    (return list))))

#+SBCL
(defmacro execute (mnemonics fn-type &rest args)
  (let ((fn (gensym)))
    `(let ((,fn (sequence-to-alien-function (assemble ,mnemonics) ,fn-type)))
       (unwind-protect
           (sb-alien:alien-funcall ,fn ,@args)
         (sb-alien:free-alien ,fn)))))
