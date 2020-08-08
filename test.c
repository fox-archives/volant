#include <stdio.h>
#include "gc.h"

static inline void defer_cleanup (void (^*b) ()) { (*b) (); }
#define defer_merge(a,b) a##b
#define defer_varname(a) defer_merge (defer_scopevar_, a)
#define defer __attribute__((cleanup (defer_cleanup))) __attribute__((unused)) void (^defer_varname (__COUNTER__)) () = ^

#define malloc(x) GC_MALLOC(x)
#define free(x) GC_FREE(x)

#define new(type) ((__mem_block){malloc(sizeof(type)), sizeof(type)})
#define len(block) block.size
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
    size_t size;
} __mem_block;

size_t size = sizeof(char[10]);
typedef struct {
	i32 a;
	i32 b;
} Kaka;

typedef enum {
	a = 0,
	b,
	c,
	d,
} Kakak;

typedef struct {
	i8 _0;
	i32 _1;
} Kakakak;

i32 function(){
	return 100;
}
__mem_block input(__mem_block str){
	scanf("%s", str._ptr);
	return str;
}
i32 main(){
	i8 x[10] = {0, 1, 2, };

	i8* y[10];

	i8 const* const z;

	printf("function() returned %i\n", function());
	i8* str = "hehehehe";
	i32 num = (3-((2/2)*5))+10;

	defer {
		printf("Haha I'll be printed on the last\n");
	};
	__mem_block* a = &(new(i8));

	__mem_block m = new(i32);

	i32 uninitialized;

	(cast(m._ptr, intptr))[0] = 0;

	printf("size is %li.\n", size);
	printf("m is %i.\nLength of m is %li\n", *(cast(m._ptr, intptr)), len(m));
	printf("function() returned %i.\n", function());
	Kaka p = (Kaka){.a = 0, .b = 0, };

	Kakakak q = (Kakakak){0, 0, };

	printf("p.a is %i and p.b is %i.\n", p.a, p.b);
	printf("q[0] is %i and q[1] is %i.\n\n", q._0, q._1);
	i8* mem = malloc(sizeof(i8[10]));

	printf("Enter something: ");
	scanf("%s", mem);
	printf("First letter of what you entered is %c.\n", mem[0]);
	printf("Second letter of what you entered is %c.\n", (mem+1)[0]);
	printf("You entered %s.\n\nRandom stuff below...\n", mem);
	free(mem);
	{
		i32 x = 0;
		if(num){
			printf("str is \"%s\" and num is %i.\n", str, num);
			x ? printf("hehe x is not xero.\n\n") : printf("hehe x is zero.\n\n");
		} else {
		}
	}
	{
		i32 i = 10;
		while(i){
			printf("i is %i and num is %i\n", i, num);
			i--;
		}
	}
	{
		i32 p = 0;
		switch(num){
		case 90:
			printf("num is 90. p is %i.\n", cast(p, i32));
			break;
		default:
			printf("num is not 90. p is %i.\n", p);
		}
	}
	return 0;
}
