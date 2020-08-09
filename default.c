#include <stdio.h>
#include "gc.h"

static inline void defer_cleanup (void (^*b) ()) { (*b) (); }
#define defer_merge(a,b) a##b
#define defer_varname(a) defer_merge (defer_scopevar_, a)
#define defer __attribute__((cleanup (defer_cleanup))) __attribute__((unused)) void (^defer_varname (__COUNTER__)) () = ^

#define malloc(x) GC_MALLOC(x)
#define free(x) GC_FREE(x)

#define new(type, base_type) ((__mem_block){malloc(sizeof(type)), sizeof(type)/sizeof(base_type), sizeof(base_type)})
#define size(block) (block.len*block.el_size)
#define len(block) (block.len)
#define delete(block) free(block._ptr); block.len = 0; block._ptr = NULL;

#define cast(val, type) ((type)val)

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

typedef char i8;
typedef short i16;
typedef int i32;
typedef long i64;

#define intptr int *
 
typedef struct {
    char *_ptr;
    size_t len;
    size_t el_size;
} __mem_block;

size_t size = sizeof(char[10]);

#define v_printf printf

int v_main();

int main() {
	return v_main();
}