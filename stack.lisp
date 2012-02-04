(progn
 (defparameter save-stack@ '((:push %rbp) (:mov %rbp %rsp) (:push %rdi) (:push %rsi) (:push %rbx)))
 (defparameter restore-stack@ '((:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp)))
 (defparameter in@ save-stack@)
 (defparameter out@ `(,@restore-stack@ :ret))

 (defmacro body (&rest mnemonics)
  `'(,@in@
     ,@mnemonics
     ,@out@))


 ;; ready/destroy data-stack
 (defun ready ()
   '(:progn (:push %rax)
            (:push %rdi)
            (:mov %edi 1024)
            (:mov %rax (:extern "malloc"))
            (:call %rax)
            (:mov %rcx %rax)
            (:pop %rdi)
            (:pop %rax)))

 (defun destroy ()
   '(:progn (:push %rax)
            (:push %rdi)
            (:mov %rdi %rcx)
            (:mov %rax (:extern "free"))
            (:call %rax)
            (:pop %rdi)
            (:pop %rax)))


 ;; registers
 (defparameter %edi '%edi)
 (defparameter %eax '%eax)
 (defparameter %ebx '%ebx)
 (defparameter %edx '%edx)
 (defparameter %ecx '%ecx)
 
 ;; ecx: data-stack
 ;; edx, ebx: temporary

 ;; stack-operation
 (defun @push (dst) ;
   `(:progn (:add %rcx 4)
            (:mov (:refd %rcx) ,dst)))
 
 (defun @pop (dst)  ;
   `(:progn (:mov ,dst (:refd %rcx))
            (:sub %rcx 4)))
 
 (defun @swap2 (a b)
   `(:progn (:mov %ebx (:refd %rcx ,(* a -4)))
            (:mov %edx (:refd %rcx ,(* b -4)))
            (:mov (:refd %rcx ,(* a -4)) %edx)
            (:mov (:refd %rcx ,(* b -4)) %ebx)))

 (defun @swap ()
   `(@swap2 0 1))
 
 (defun @dup ()
   `(:progn (:mov %ebx (:refd %rcx))
            (@push %ebx)))
 
 (defun @drop ()
   `(:progn (:sub %rcx 4)))

 (defun @over ()
   `(:progn (:mov %ebx (:refd %rcx -4))
            (@push %ebx)))

 (defun @rot ()
   `(:progn (@swap2 2 0)
            (@swap2 1 2)))

 (defun @add ()
   `(:progn (@pop %ebx)
            (@pop %eax)
            (:add %eax %ebx)
            (@push %eax)))

 (defun @sub ()
   `(:progn (@pop %ebx) ; TODO: pop2
            (@pop %eax)
            (:sub %eax %ebx)
            (@push %eax)))

 (defun @eql ()
   `(:progn (@pop %eax)
            (@pop %ebx)
            (:sub %eax %ebx)
            (@push %eax)))
 
 (defun @less ()
   `(:progn (@pop %ebx)
            (@pop %eax)
            (:cmp %eax %ebx)
            (:mov %eax 0)
            (:setl %al)
            (@push %eax)))

 (defun @jump-if (pos)
   `(:progn (@pop %eax)
            (:cmp %eax 0)
            (:jne ,pos)))

 (defun @jump (pos)
   `(:jmp ,pos))

 (defun @call (pos)
   `(:call ,pos))

 (defun @int (n)
   `(@push ,n))

 (defun @return ()
   :ret)
 )

(cl-asm:execute
 (body
  (ready)
  (@int 10)
  (@pop %eax)
  (destroy)
  )
 (function int int) 30)

;; fib
(cl-asm:execute
 (body
  (ready)
  (@push %edi)
  (@call '&fib-beg)
  (@jump '&finish)

  &fib-beg
  (@dup) (@int 2) (@less) (@jump-if '&fib-end)
  (@dup) (@int 2) (@sub) (@call '&fib-beg)
  (@swap) (@int 1) (@sub) (@call '&fib-beg)
  (@add)
  &fib-end
  (@return)

  &finish

  (@pop %eax)
  (destroy)
  )
 (function int int) 10)
