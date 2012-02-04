(progn
 (defparameter save-stack@ '((:push %rbp) (:mov %rbp %rsp) (:push %rdi) (:push %rsi) (:push %rbx)))
 (defparameter restore-stack@ '((:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp)))
 (defparameter in@ save-stack@)
 (defparameter out@ `(,@restore-stack@ :ret))

 (defmacro body (&rest mnemonics)
  `'(,@in@
     ,@mnemonics
     ,@out@))
 )

;; Fibonacci
(time
 (cl-asm:execute
 (body
  (:mov %eax %edi)
  (:call &fib-beg)
  (:jmp &finish)

  &fib-beg
  (:cmp %eax 2)
  (:jl &fib-end)
  
  (:push %rax)
  (:sub %eax 2)
  (:call &fib-beg)
  (:pop %rbx)

  (:push %rax)
  (:mov %eax %ebx)
  (:dec %eax)
  (:call &fib-beg)
  (:pop %rbx)
  
  (:add %eax %ebx)
  &fib-end
  :ret
  
  &finish)

 (function int int)
 35))
