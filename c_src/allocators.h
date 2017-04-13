#ifndef EZLIB_C_SRC_ALLOCATORS_H_
#define EZLIB_C_SRC_ALLOCATORS_H_

#include <stdlib.h>

void* mem_allocate(size_t size);
void mem_deallocate(void* ptr);

#endif
