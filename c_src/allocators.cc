#include "allocators.h"
#include "erl_nif.h"

void* mem_allocate(size_t size)
{
    return enif_alloc(size);
}

void mem_deallocate(void* ptr)
{
    enif_free(ptr);
}

