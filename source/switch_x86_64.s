/*  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
*/

/* switch_context(context *from, context *to)
                  rdi,           rsi
*/
   .text
   .globl switch_context
   .type switch_context, @function

switch_context:
    .cfi_startproc
    pushq %rbp
    .cfi_def_cfa_offset 16
    .cfi_offset rbp, -16
    movq %rsp, %rbp
    .cfi_def_cfa_register rbp
    subq $64, %rsp
    fnstcw     -52(%rbp) /* 16 bits (54 bytes)*/
    stmxcsr    -48(%rbp) /* 32 bits */
    movq %r15, -40(%rbp) /* 64 bits */
    movq %r14, -32(%rbp) /* 64 bits */
    movq %r13, -24(%rbp) /* 64 bits */
    movq %r12, -16(%rbp) /* 64 bits */
    movq %rbx, -8(%rbp)  /* 64 bits */
    movq %rbp,  0(%rdi)  /* from */
    movq 0(%rsi), %rbp   /* to */
    fldcw      -52(%rbp) /* 16 bits */
    ldmxcsr    -48(%rbp) /* 32 bits */
    movq -40(%rbp), %r15 /* 64 bits */
    movq -32(%rbp), %r14 /* 64 bits */
    movq -24(%rbp), %r13 /* 64 bits */
    movq -16(%rbp), %r12 /* 64 bits */
    movq -8(%rbp), %rbx  /* 64 bits */
    leave
    .cfi_def_cfa rsp, 8
    ret
    .cfi_endproc

/* init_context(context, code, arg1, arg2, cont)
                rdi,     rsi,  rdx,  rcx,  r8
*/
   .globl init_context
   .type init_context, @function

init_context:
    .cfi_startproc
    pushq %rbp
    .cfi_def_cfa_offset 16
    .cfi_offset rbp, -16
    movq %rsp, %rbp
    .cfi_def_cfa_register rbp
    movq %rdi, %rax /* context */
    subq $16, %rax
    movq %rax, 0(%rdi)   /* stack_pointer */
    fnstcw     -52(%rax) /* 16 bits (54 bytes)*/
    stmxcsr    -48(%rax) /* 32 bits */
    movq %rsi, -40(%rax) /* r15 := code */
    movq %rdx, -32(%rax) /* r14 := arg1 */
    movq %rcx, -24(%rax) /* r13 := arg2 */
    movq %r8,  -16(%rax) /* r12 := cont */
    movq %rdi, -8(%rax)  /* rbx := context */
    xorq %r9, %r9
    movq %r9, (%rax)     /* rbp := 0 */
    lea  _bootstrap(%rip), %r9
    movq %r9, 8(%rax)   /* return addr */

    leave
    .cfi_def_cfa rsp, 8
    ret
    .cfi_endproc

   .globl _bootstrap
   .type _bootstrap, @function
   .hidden _bootstrap

_bootstrap:
    .cfi_startproc
    movq %r14, %rdi
    movq %r13, %rsi
    call *%r15
    movq 0(%r12), %rbp   /* cont */
    fldcw      -52(%rbp) /* 16 bits */
    ldmxcsr    -48(%rbp) /* 32 bits */
    movq -40(%rbp), %r15 /* 64 bits */
    movq -32(%rbp), %r14 /* 64 bits */
    movq -24(%rbp), %r13 /* 64 bits */
    movq -16(%rbp), %r12 /* 64 bits */
    movq -8(%rbp), %rbx  /* 64 bits */
    leave
    .cfi_def_cfa rsp, 8
    ret
    .cfi_endproc
