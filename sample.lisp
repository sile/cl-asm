;; 関数呼び出し時の定型処理
(cl-asm:execute 
 '((:push %rbp)
   (:mov %rsp %rbp)
   (:push %rdi)
   (:push %rsi)
   (:push %rbx)
   
   (:pop %rbx)
   (:pop %rsi)
   (:pop %rdi)
   (:pop %rbp)
   :ret)

 (function int))

;; 引数使用
(cl-asm:execute 
 '((:push %rbp)
   (:mov %rsp %rbp)
   (:push %rdi)
   (:push %rsi)
   (:push %rbx)

   (:mov (%rbp -8) %eax)

   (:pop %rbx)
   (:pop %rsi)
   (:pop %rdi)
   (:pop %rbp)
   :ret)

 (function int int)
 
 18)

;; 加算
(cl-asm:execute 
 '((:push %rbp)
   (:mov %rsp %rbp)
   (:push %rdi)
   (:push %rsi)
   (:push %rbx)

   (:mov (%rbp -08) %eax)
   (:mov (%rbp -16) %edx)
   (:add %edx %eax)

   (:pop %rbx)
   (:pop %rsi)
   (:pop %rdi)
   (:pop %rbp)
   :ret)

 (function int int int)
 
 20 12)

;; TODO: cmp, call, jump-if, data-stack-op
;; rdiにdata-stackの先頭を入れておく

(extern-alien "malloc" (function (* t) int))

(cl-asm:assemble '((:mov (%rbp -4) %eax)))

0010 01D0     		addl	%edx, %eax