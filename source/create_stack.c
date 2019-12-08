#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>

struct header {
  void* stack_pointer;
  void* origin;
  off_t size;
};

#define HEADER_SIZE (((sizeof(struct header) + 15) / 16) * 16)
/*
 *  Result points to the top of allocated stack like this:
 *  result -> header                                    -> origin ->--+
 *            [stack content <char[(size rounded to page size)]>      |
 *            [read only page] <--------------------------------------+
 */
struct header * create_context (int size) {
  long pagesize = sysconf(_SC_PAGE_SIZE);
  /* total pages and one extra guard page: */
  int pages = (size + HEADER_SIZE + pagesize - 1) / pagesize + 1;
  void *result;
  struct header *context;

  if (pagesize == -1) return NULL;

  result = mmap
    (NULL, pages * pagesize,
     PROT_READ | PROT_WRITE,
     MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK,
     -1, 0);

  if (result == MAP_FAILED) return NULL;

  /* protect guard page from writting */
  if (mprotect(result, pagesize, PROT_READ) != 0) {
    munmap (result, pages * pagesize);  /* rollback mmap*/
    return NULL;
  }

  context = (struct header *)((char*)result + pages * pagesize - HEADER_SIZE);
  context->origin = result;
  context->size = pages * pagesize;
  context->stack_pointer = NULL;

  return context;
}

void free_context (struct header * context) {
  munmap (context->origin, context->size);
}
