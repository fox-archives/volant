package compiler

var Default []byte = []byte(`
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

typedef unsigned char v_u8;
typedef unsigned short v_u16;
typedef unsigned int v_u32;
typedef unsigned long v_u64;

typedef char v_i8;
typedef short v_i16;
typedef int v_i32;
typedef long v_i64;
 
typedef void v_void;

typedef struct {
    char *_ptr;
    size_t _size;
} __mem_block;

size_t size = sizeof(char[10]);

#define v_printf printf
#define v_memcpy memcpy

static int v_main();

int main() {
	return v_main();
}
`)
