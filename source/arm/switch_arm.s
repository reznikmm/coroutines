/*  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
*/
   .cpu arm10tdmi
   .eabi_attribute 28, 1
   .eabi_attribute 20, 1
   .eabi_attribute 21, 1
   .eabi_attribute 23, 3
   .eabi_attribute 24, 1
   .eabi_attribute 25, 1
   .eabi_attribute 26, 2
   .eabi_attribute 30, 6
   .eabi_attribute 34, 0
	.syntax unified
	.arm
	.fpu vfp

/* switch_context(context *from, context *to)
                  rdi,           rsi
*/
   .text
   .globl switch_context
   .type switch_context, %function

switch_context:
    .cfi_startproc
	.fnstart
	push	{r4, r5, r6, r7, r8, r9, r10, fp, lr}
        str sp, [r0]
        ldr sp, [r1]
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}
	.align	2
	.cfi_endproc
	.fnend

   .globl init_context
   .type init_context, %function

/* init_context(context, code, arg1, arg2, cont)
                r0,      r1,   r2,   r3,   fp[?]
*/
init_context:
    .cfi_startproc
	.fnstart
	push	{fp, lr}
	.save {fp, lr}
	.cfi_def_cfa_offset 8
	.cfi_offset 11, -8
	.cfi_offset 14, -4

	sub	r12, r0, #36 /* 9 x 4 = 36 */
        str     r12, [r0]    /* stack_pointer */
        ldr     r0, [sp, #8]
        str     r0, [r12], #4 /* r4 := cont */
        sub     r0, r0, r0
        str     r0, [r12], #4 /* r5 := 0 */
        str     r0, [r12], #4 /* r6 := 0 */
        str     r0, [r12], #4 /* r7 := 0 */
        str     r1, [r12], #4 /* r8 := code */
        str     r2, [r12], #4 /* r9 := arg1 */
        str     r3, [r12], #4 /* r10 := arg2 */
        str     r0, [r12], #4 /* fp := 0 */
        ldr     r0, =_bootstrap
        str     r0, [r12], #4 /* return addr */

	pop	{fp, pc}
	.align	2
	.cfi_endproc
	.fnend


   .globl _bootstrap
   .type _bootstrap, %function
   .hidden _bootstrap

_bootstrap:
    .cfi_startproc
	.fnstart
        mov r0, r9
        mov r1, r10
        blx r8
        ldr sp, [r4]
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}
	.align	2
	.cfi_endproc
	.fnend
