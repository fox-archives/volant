
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

typedef struct {
	v_i32 (v_a);
	v_i32 (v_b);
	v_i32 (v_c);
	v_i32 (v_d);
} v_Kaka;
v_Kaka d_Kaka = (v_Kaka){.v_a = 0, .v_b = 1, .v_c = 2, .v_d = 3, };

typedef struct {
	v_i32 (v_e);
	v_i32 (v_f);
	v_i32 (v_a);
	v_i32 (v_b);
	v_i32 (v_c);
	v_i32 (v_d);
} v_Kaka2;
v_Kaka2 d_Kaka2 = (v_Kaka2){.v_e = 4, .v_f = 5, .v_a = 0, .v_b = 1, .v_c = 2, .v_d = 3, };

typedef enum {
	enum_Kakak_a = 0,
	enum_Kakak_b,
	enum_Kakak_c,
	enum_Kakak_d,
} v_Kakak;

typedef struct {
	v_i8 (_0);
	v_i32 (_1);
} v_Kakakak;

static v_i32** (***v_o[10])(v_i32);

typedef v_u16 (v_haha)[100];

typedef v_u8 (v_haa);

static __mem_block (v_function)(v_void){
	__mem_block (v_nums) = new(__mem_block);

	((__mem_block*)(v_nums._ptr))[0] = new(__mem_block);

	((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0] = new(v_i32[10]);

	((v_i32*)(((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0]._ptr))[0] = 269488144;

	return v_nums;
};

static v_i32 (v_main)(v_void){
	v_printf("haha I wont decay to pointer.\n");
	__mem_block (v_nums) = v_function();

	__mem_block (*v_numsptr) = (&v_nums);

	v_printf("Length of nums is %zu. Size of nums is %zu.\n", len(v_nums,__mem_block), size(v_nums));
	delete(((__mem_block*)(((__mem_block*)(v_nums._ptr))[0]._ptr))[0]);
	delete(((__mem_block*)(v_nums._ptr))[0]);
	delete(v_nums);

	v_printf("haha I'll decay to pointer. %i.\n", ((v_i32*)(((__mem_block*)(((__mem_block*)(v_function()._ptr))[0]._ptr))[0]._ptr))[0]);
	v_printf("Kakak.a is %i.\n", (v_i32)(enum_Kakak_a));
	v_u8 (v_str)[7] = "hehehe";

	v_printf("hehe is %s.\n", v_str);
	__mem_block (v_dynStrs)[1] = {new(v_u8[7]), };

	v_memcpy((&(((v_u8*)(v_dynStrs[0]._ptr))[0])), v_str, 7);
	v_printf("dynamic string is %s.\n", (&(((v_u8*)(v_dynStrs[0]._ptr))[0])));
	v_i32 (v_x)[100];

	v_printf("Length of x is %zu. Size of x is %zu.\n", len2(v_i32[100],v_i32), size2(v_x));
	v_printf("Length of a is %zu. Size of a is %zu.\n", len2(v_i32** (***[10])(v_i32),v_i32** (***)(v_i32)), size2(v_o));
	v_Kakakak (v_tupl) = (v_Kakakak){0, 1, };

	v_printf("tupl[0] is %i. tupl[1] is %i\n", v_tupl._0, v_tupl._1);
	v_Kaka2 (v_strct) = (v_Kaka2){.v_a = 1, .v_e = d_Kaka2.v_e, .v_f = d_Kaka2.v_f, };

	v_Kaka (v_strct2) = (v_Kaka)(v_strct);

	return 0;
};
