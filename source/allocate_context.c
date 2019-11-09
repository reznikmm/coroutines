/*
--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
*/

#include <ucontext.h>
#include <stdlib.h>
#include <stdio.h>
extern void jump_to_runable (void *);

ucontext_t* allocate_context
  (ucontext_t * link,
  size_t stack_size,
  void * stack,
  void * runable)
{
  ucontext_t* result = malloc (sizeof(ucontext_t));
  getcontext (result);
  result->uc_link = link;
  result->uc_flags = 0;
/*  result->uc_sigmask = 0; */
  result->uc_stack.ss_size = stack_size;
  result->uc_stack.ss_sp = stack;
  result->uc_stack.ss_flags = 0;

  printf ("stack=%lx\n", (unsigned long)stack);
  if (stack_size != 0) {
    makecontext (result, (void (*)()) &jump_to_runable, 1, runable);
  }

  return result;
}
