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

;;  比較
(cl-asm:execute 
 '((:push %rbp) (:mov %rsp %rbp) (:push %rdi) (:push %rsi) (:push %rbx)

   (:mov (%rbp -08) %eax)
   (:mov (%rbp -16) %edx)
   (:cmp %eax %edx)
   (:jmp-if :< then)
   
   else
   (:mov 1 %eax)
   (:jmp end)
   then
   (:mov 2 %eax)
   end
   
   (:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp) :ret)

 (function int int int)
 
 20 21)

;; 外部関数呼び出し (fastcall)
(cl-asm:execute 
 '((:push %rbp) (:mov %rsp %rbp) (:push %rdi) (:push %rsi) (:push %rbx)

   (:mov 32 %edi)
   (:call (:extern "isspace"))
   
   (:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp) :ret)

 (function int))

;; fib
(identity ;time
 (cl-asm:execute 
 '((:push %rbp) (:mov %rsp %rbp) (:push %rdi) (:push %rsi) (:push %rbx)
   (:mov %edi %eax) ; arg

   (:push %eax)
   (:call fib-begin)
   (:pop %ebx)

   (:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp) :ret

   fib-begin
   (:push %rbp) (:mov %rsp %rbp)
   (:mov (%rbp 16) %eax) ; arg
   
;   (:mov %eax %edi)
;   (:push %eax)
;   (:call (:extern "pn"))
;   (:pop %eax)

   (:mov 2 %edx)
   (:cmp %eax %edx)
   (:jmp-if :< fib-end) ; arg < 2
   
   (:push %eax)

   (:add -2 %eax)
   (:push %eax)
   (:call fib-begin)
   (:pop %edx)
   
   (:pop %edx)
   (:push %eax)
   
   (:mov %edx %eax)
   (:add -1 %eax)
   (:push %eax)
   (:call fib-begin)
   (:pop %edx)
   (:pop %edx)

   (:add %edx %eax)

   fib-end
   (:pop %rbp)
   :ret)

 (function int int)
 20)
 )


;; TODO: call, data-stack-op
;; rdiにdata-stackの先頭を入れておく

(extern-alien "malloc" (function (* t) int))
(load-shared-object "/home/ohta/dev/lisp/cl-asm/libp10.so")
#|
; p10.c
#include <stdio.h>

int p10() {
  printf("print 10\n");
  return 10;
}

int pn(int n) {
  printf("print %d\n", n);
  return 11;
}
|#

(cl-asm:assemble '((:jmp-if :< end) (:add %edx %eax) end))
(cl-asm:assemble '((:call (:extern "p10"))))
(cl-asm:assemble '((:mov (%rsp 4) %eax)))


;; fib
(time
 (cl-asm:execute 
 '((:push %rbp) (:mov %rsp %rbp) (:push %rdi) (:push %rsi) (:push %rbx)
   (:mov %edi %eax) ; arg

   (:call fib-begin)

   (:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp) :ret

   fib-begin
   (:push %rbp) (:mov %rsp %rbp)

   (:mov 2 %edx)
   (:cmp %eax %edx)
   (:jmp-if :< fib-end) ; arg < 2
   
   (:push %eax)

   (:add -2 %eax)
   (:call fib-begin)
   
   (:pop %edx)
   (:push %eax)
   
   (:mov %edx %eax)
   (:add -1 %eax)
   (:call fib-begin)
   (:pop %edx)

   (:add %edx %eax)

   fib-end
   (:pop %rbp)
   :ret)

 (function int int)
 10)
 )

;;;;;;;;;;;;;;;;;;;;;;;;
;; 関数呼び出し時の定型処理
(cl-asm:execute 
 '((:push %rbp)
   (:mov %rsp %rbp)
   (:push %rdi)
   (:push %rsi)
   (:push %rbx)
   
;   (:mov %edi %eax)
;   (:mov 6546447 %ebx)
;   (:mov %al (:ref %ebx))
;   (:mov (:ref %ebx 0) %ax)

;   (:mov %edi %eax)
;   (:mov %eax (:ref 6546448))
;   (:mov 1001 %eax)
   (:mov (:imm64 333) (:ref 6546448))
   
   (:pop %rbx)
   (:pop %rsi)
   (:pop %rdi)
   (:pop %rbp)
   :ret)

 (function int int)
 #xFF00)

;; TODO: unit-test

; アセンブラを書くのが少し楽しくなってきた。本筋からは外れていってるけど。