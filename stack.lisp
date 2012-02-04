(defun save-registers ()
  '(:progn
    (:push %rbp) (:mov %rbp %rsp) (:push %rdi) (:push %rsi) (:push %rbx)))

(defun restore-registers ()
  '(:progn
    (:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp)))

(defun ready-data-stack ()
   '(:progn (:push %rax)
            (:push %rdi)
            (:mov %edi 102400)
            (:mov %rax (:extern "malloc"))
            (:call %rax)
            (:mov %rcx %rax)
            (:pop %rdi)
            (:pop %rax)))

(defun destroy-data-stack ()
  '(:progn (:push %rax)
           (:push %rdi)
           (:mov %rdi %rcx)
           (:mov %rax (:extern "free"))
           (:call %rax)
           (:pop %rdi)
           (:pop %rax)))

(defmacro body (&rest mnemonics)
  `'(,(save-registers)
     ,(ready-data-stack)
     ,@mnemonics
     ,(destroy-data-stack)
     ,(restore-registers)
     :ret))

(defmacro defop (name args &body body)
  `(defmacro ,name ,args
     (list 'quote (locally ,@body))))

 ;; ecx: data-stack
 ;; eax, ebx: temporary

(defop @ds-get (dst index) `(:mov ,dst (:refd %rcx ,(* index -4))))
(defop @ds-set (index src) `(:mov (:refd %rcx ,(* index -4)) ,src))
(defop @ds-inc (&optional (n 1)) `(:add %rcx ,(* 4 n)))
(defop @ds-dec (&optional (n 1)) `(:sub %rcx ,(* 4 n)))
  

(defop @push (src)
  `(:progn (@ds-inc)
           (@ds-set 0 ,src)))

(defop @pop (dst)
  `(:progn (@ds-get ,dst 0)
           (@ds-dec)))

(defop @pop2 (dst1 dst2)
  `(:progn (@ds-get ,dst1 0)
           (@ds-get ,dst2 1)
           (@ds-dec 2)))

(defop @swap-impl (index1 index2)
  `(:progn (@ds-get %eax ,index1)
           (@ds-get %ebx ,index2)
           (@ds-set ,index1 %ebx)
           (@ds-set ,index2 %eax)))

(defop @swap ()
  '(@swap-impl 0 1))

(defop @dup ()
  `(:progn (@ds-get %eax 0)
           (@push %eax)))

(defop @drop ()
  '(@ds-dec))

(defop @over ()
  `(:progn (@ds-get %eax 1)
           (@push %eax)))

(defop @rot
  `(:progn (@swap-impl 2 0)
           (@swap-impl 1 2)))

(defop @add ()
  `(:progn (@pop2 %ebx %eax)
           (:add %eax %ebx)
           (@push %eax)))

(defop @sub ()
  `(:progn (@pop2 %ebx %eax)
            (:sub %eax %ebx)
            (@push %eax)))

(defop @eql ()
  `(:progn (@pop2 %ebx %eax)
           (:sub %eax %ebx)
           (@push %eax)))
 
(defop @less ()
  `(:progn (@pop2 %ebx %eax)
           (:cmp %eax %ebx)
           (:mov %eax 0)
           (:setl %al)
           (@push %eax)))

(defop @jump-if (pos)
  `(:progn (@pop %eax)
           (:cmp %eax 0)
           (:jne ,pos)))

(defop @jump (pos)
  `(:jmp ,pos))

(defop @call (pos)
  `(:call ,pos))

(defop @int (n)
  `(@push ,n))

(defop @return ()
  :ret)

(cl-asm:execute
 (body
  (@int 10)
  (@pop %eax)
  )
 (function int int) 30)

;; fib
(time
 (cl-asm:execute
 (body
  (@push %edi)
  (@call &fib-beg)
  (@jump &finish)

  &fib-beg
  (@dup) (@int 2) (@less) (@jump-if &fib-end)
  (@dup) (@int 2) (@sub) (@call &fib-beg)
  (@swap) (@int 1) (@sub) (@call &fib-beg)
  (@add)
  &fib-end
  (@return)

  &finish

  (@pop %eax)
  )
 (function int int) 35))
