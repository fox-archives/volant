#include <stdio.h>
#include <string.h>
#include "gc.h"
/*
static inline void defer_cleanup (void (^*b) ()) { (*b) (); }
#define defer_merge(a,b) a##b
#define defer_varname(a) defer_merge (defer_scopevar_, a)
#define defer __attribute__((cleanup (defer_cleanup))) __attribute__((unused)) void (^defer_varname (__COUNTER__)) () = ^
*/
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

#define intptr int *
 
typedef struct {
    char *_ptr;
    size_t _size;
} __mem_block;

size_t size = sizeof(char[10]);

#define v_printf printf
#define v_memcpy memcpy

int v_main();

int main() {
	return v_main();
}

typedef struct {
	i32 (v_a);
	i32 (v_b);
} Kaka;

typedef enum {
	enum_Kakak_a = 0,
	enum_Kakak_b,
	enum_Kakak_c,
	enum_Kakak_d,
} Kakak;

typedef struct {
	i8 (_0);
	i32 (_1);
} Kakakak;

int** (***v_o[10])(int);

typedef u16 (v_haha)[100];
typedef u8 (v_haa);
__mem_block (v_function)(void){
	__mem_block (v_nums) = new(__mem_block);

	((__mem_block*)(v_nums._ptr))[0] = new(__mem_block);

	((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0] = new(i32[10]);

	((i32*)(((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0]._ptr))[0] = 269488144;

	return v_nums;
};

i32 (v_main)(void){
	v_printf("haha I wont decay to pointer.\n");
	__mem_block (v_nums) = v_function();

	__mem_block (*v_numsptr) = (&v_nums);

	v_printf("Length of nums is %zu. Size of nums is %zu.\n", len(v_nums,__mem_block), size(v_nums));
	delete(((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0]);
	delete(((__mem_block*)(v_nums._ptr))[0]);
	delete(v_nums);

	v_printf("haha I'll decay to pointer. %i.\n", ((i32*)(((__mem_block*)(((__mem_block*)(v_function()._ptr))[0]._ptr))[0]._ptr))[0]);
	v_printf("Kakak.a is %i.\n", (int)(enum_Kakak_a));
	u8 (v_str)[7] = "hehehe";

	v_printf("hehe is %s.\n", v_str);
	__mem_block (v_dynStrs)[1] = {new(u8[7]), };

	v_memcpy((&(((u8*)(v_dynStrs[0]._ptr))[0])), v_str, 7);
	v_printf("dynamic string is %s.\n", (&(((u8*)(v_dynStrs[0]._ptr))[0])));
	i32 (v_x)[100];

	v_printf("Length of x is %zu. Size of x is %zu.\n", len2(i32[100],i32), size2(v_x));
	v_printf("Length of a is %zu. Size of a is %zu.\n", len2(int** (***[10])(int),int** (***)(int)), size2(v_o));
	return 0;
};

