#include <stdio.h>
#include <string.h>
#include "gc.h"

#define malloc(x) GC_MALLOC(x)
#define free(x) GC_FREE(x)

#define new(type) ((__mem_block){malloc(sizeof(type)), sizeof(type)})
#define delete(block) free(block._ptr); block._size = 0; block._ptr = NULL;

#define len(block, base_type) (block._size/sizeof(base_type))
#define len2(array, base_type) sizeof(array)/sizeof(base_type)
#define len3(var) sizeof(var)

#define size(block) (block._size)
#define size2(var) sizeof(var)

#define cast(val, type) ((type)val)

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

typedef char i8;
typedef short i16;
typedef int i32;
typedef long i64;

typedef struct {
    void *_ptr;
    size_t _size;
} __mem_block;


#define v0_printf printf
#define v0_memcpy memcpy

static i32 v0_main();
int main() {
	return v0_main();
}