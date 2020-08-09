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

typedef struct {
	i32 v_a;
	i32 v_b;
} Kaka;

typedef enum {
	enum_Kakak_a = 0,
	enum_Kakak_b,
	enum_Kakak_c,
	enum_Kakak_d,
} Kakak;

typedef struct {
	i8 _0;
	i32 _1;
} Kakakak;

typedef i32 integer;
__mem_block v_function(){
	__mem_block v_nums = new(__mem_block,__mem_block);

	((__mem_block*)(v_nums._ptr))[0] = new(__mem_block,i32);

	((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0] = new(i32[10],i32);

	((i32*)(((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0]._ptr))[0] = 269488144;

	return v_nums;
}
i32 v_main(){
	v_printf("haha I wont decay to pointer.\n");
	__mem_block v_nums = v_function();

	delete(((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0]);
	delete(((__mem_block*)(v_nums._ptr))[0]);
	delete(v_nums);

	v_printf("haha I'll decay to pointer. %i.\n", ((i32*)(((__mem_block*)(((__mem_block*)(v_function()._ptr))[0]._ptr))[0]._ptr))[0]);
	v_printf("Kakak.a is %i.\n", enum_Kakak_a);
	return 0;
}
