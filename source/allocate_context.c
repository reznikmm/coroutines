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
#include <signal.h>

ucontext_t* allocate_context
  (ucontext_t * link,
  size_t stack_size,
  void (*wrapper)(void *, void *),
  void * code,
  void * argument)
{
  ucontext_t* result = malloc (sizeof(ucontext_t));
  getcontext (result);
  result->uc_link = link;
  result->uc_flags = 0;
  sigemptyset (&result->uc_sigmask);
  result->uc_stack.ss_size = stack_size;
  result->uc_stack.ss_flags = 0;

  if (stack_size != 0) {
    result->uc_stack.ss_sp = malloc (stack_size);
    makecontext (result, (void (*)()) wrapper, 2, code, argument);
  }else{
    result->uc_stack.ss_sp = 0;
  }

  printf ("stack=%lx\n", (unsigned long)result->uc_stack.ss_sp);

  return result;
}

void free_context (ucontext_t* context)
{
  if (context->uc_stack.ss_size != 0) {
    free (context->uc_stack.ss_sp);  /* ??? */
  }

  free (context);
}
