; ModuleID = '01_simple.rs'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

;;; Following constants can be named awfully

;; Value of immutable global variable
@M = internal constant i32 5, align 4                       ; static M: i32 = 5;

; Number constant mentioned directly in code
@const2778 = internal unnamed_addr constant i32 21, align 4
; const ANSWER: i32 = 42;
@const2779 = internal unnamed_addr constant i32 42, align 4

; Function Attrs: nounwind uwtable
define void @foo() #0 {
  %a = alloca i32, align 4                                         ; let a: i32;
  store i32 21, i32* %a, align 4                                       ; a = 21;
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %a = alloca i32                                                  ; let a: i32;
  %b = alloca i32                                                  ; let b: i32;

  store i32 5, i32* %a, align 4                                         ; a = M;
  store i32 42, i32* %b, align 4                                   ; b = ANSWER;

  ret i32 0                                                    ; Implicit return
}

;; See http://llvm.org/docs/LangRef.html#function-attributes
; nounwind - This function attribute indicates that the function never raises an exception. If the function does raise an exception, its runtime behavior is undefined. However, functions marked nounwind may still trap or generate asynchronous exceptions. Exception handling schemes that are recognized by LLVM to handle asynchronous exceptions, such as SEH, will still provide their implementation defined semantics.
; uwtable - This attribute indicates that the ABI being targeted requires that an unwind table entry be produced for this function even if we can show that no exceptions passes by it. This is normally the case for the ELF x86-64 abi, but it can be disabled for some compilation units.
attributes #0 = { nounwind uwtable }

; Compiler identificator
!llvm.ident = !{!0}

!0 = !{!"falsum version 0.1"}
