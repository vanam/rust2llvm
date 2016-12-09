; ModuleID = '02_int_op.rs'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

;;; Following constants can be named awfully

; Number constants mentioned directly in code
@const2759 = internal unnamed_addr constant i32 5, align 4
@const2760 = internal unnamed_addr constant i32 7, align 4
@const2761 = internal unnamed_addr constant i32 9, align 4
@const2762 = internal unnamed_addr constant i32 11, align 4

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %a = alloca i32                                                  ; let a: i32;
  %b = alloca i32                                                  ; let b: i32;
  %c = alloca i32                                                  ; let c: i32;
  %d = alloca i32                                                  ; let d: i32;
  %r = alloca i32                                                  ; let r: i32;

  store i32 5, i32* %a, align 4                                         ; a = 5;
  store i32 7, i32* %b, align 4                                         ; b = 7;
  store i32 9, i32* %c, align 4                                         ; c = 9;
  store i32 11, i32* %d, align 4                                       ; d = 11;

  ; Memset uninitialized variable 'r' (optional)
  ; Can check for usage of uninitialized variable - https://internals.rust-lang.org/t/attention-hackers-filling-drop/1715/8
  %1 = bitcast i32* %r to i8*
  ; Set to 0x1D1D1D1D Warning: Can be valid value
  call void @llvm.memset.p0i8.i64(i8* %1, i8 29, i64 4, i32 4, i1 false)

  ; r = a + b;
  %2 = load i32, i32* %a, align 4
  %3 = load i32, i32* %b, align 4
  %4 = add nsw i32 %2, %3
  store i32 %4, i32* %r, align 4

  ; r = b - c;
  %5 = load i32, i32* %b, align 4
  %6 = load i32, i32* %c, align 4
  %7 = sub nsw i32 %5, %6
  store i32 %7, i32* %r, align 4

  ; r = c * d;
  %8 = load i32, i32* %c, align 4
  %9 = load i32, i32* %d, align 4
  %10 = mul nsw i32 %8, %9
  store i32 %10, i32* %r, align 4

  ; r = d / a;
  %11 = load i32, i32* %d, align 4
  %12 = load i32, i32* %a, align 4
  %13 = sdiv i32 %11, %12
  store i32 %13, i32* %r, align 4

  ; r = d % c;
  %14 = load i32, i32* %d, align 4
  %15 = load i32, i32* %c, align 4
  %16 = srem i32 %14, %15
  store i32 %16, i32* %r, align 4

  ; r = a & b;
  %17 = load i32, i32* %a, align 4
  %18 = load i32, i32* %b, align 4
  %19 = and i32 %17, %18
  store i32 %19, i32* %r, align 4

  ; r = a & b;
  %20 = load i32, i32* %a, align 4
  %21 = load i32, i32* %b, align 4
  %22 = and i32 %20, %21
  store i32 %22, i32* %r, align 4

  ; r = b | c;
  %23 = load i32, i32* %b, align 4
  %24 = load i32, i32* %c, align 4
  %25 = or i32 %23, %24
  store i32 %25, i32* %r, align 4

  ; r = c ^ d;
  %26 = load i32, i32* %c, align 4
  %27 = load i32, i32* %d, align 4
  %28 = xor i32 %26, %27
  store i32 %28, i32* %r, align 4

  ; r = !a; Rust way
  %29 = load i32, i32* %a, align 4
  %30 = xor i32 %29, -1
  store i32 %30, i32* %r, align 4

  ; r = !a; Clang way
  ; %29 = load i32, i32* %a, align 4
  ; %30 = icmp ne i32 %29, 0
  ; %31 = xor i1 %30, true
  ; %32 = zext i1 %31 to i32
  ; store i32 %32, i32* %r, align 4

  ; r = a << b;
  %31 = load i32, i32* %a, align 4
  %32 = load i32, i32* %b, align 4
  %33 = shl i32 %31, %32
  store i32 %33, i32* %r, align 4

  ; r = a >> b;
  %34 = load i32, i32* %a, align 4
  %35 = load i32, i32* %b, align 4
  %36 = ashr i32 %34, %35
  store i32 %36, i32* %r, align 4

  ret i32 0                                                    ; Implicit return
}

; Function Attrs: nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) #1

;; See http://llvm.org/docs/LangRef.html#function-attributes
; nounwind - This function attribute indicates that the function never raises an exception. If the function does raise an exception, its runtime behavior is undefined. However, functions marked nounwind may still trap or generate asynchronous exceptions. Exception handling schemes that are recognized by LLVM to handle asynchronous exceptions, such as SEH, will still provide their implementation defined semantics.
; uwtable - This attribute indicates that the ABI being targeted requires that an unwind table entry be produced for this function even if we can show that no exceptions passes by it. This is normally the case for the ELF x86-64 abi, but it can be disabled for some compilation units.
attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind }

; Compiler identificator
!llvm.ident = !{!0}

!0 = !{!"falsum version 0.1"}
