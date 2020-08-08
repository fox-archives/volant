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

#define intptr int *
 
typedef struct {
    char *_ptr;
    size_t size;
} __mem_block;

size_t size = sizeof(char[10]);
typedef struct {
	int a;
	int b;
} Kaka;

typedef enum {
	a = 0,
	b,
	c,
	d,
} Kakak;

typedef struct {
	char _0;
	int _1;
} Kakakak;

int function(){
	return 100;
}
__mem_block input(__mem_block str){
	scanf("%s", str._ptr);
	return str;
}
int main(){
	int (^function)() = ^int(){
		return 0;
	};
	printf("function() returned %i\n", function());
	char* str = "hehehehe";
	int num = (3-((2/2)*5))+10;

	defer {
		printf("Haha I'll be printed on the last\n");
	};
	__mem_block* a = &(new(char));

	__mem_block m = new(int);

	int uninitialized;

	(cast(m._ptr, intptr))[0] = 0;

	printf("size is %li.\n", size);
	printf("m is %i.\nLength of m is %li\n", *(cast(m._ptr, intptr)), len(m));
	printf("function() returned %i.\n", function());
	Kaka p = (Kaka){.a = 0, .b = 0, };

	Kakakak q = (Kakakak){0, 0, };

	printf("p.a is %i and p.b is %i.\n", p.a, p.b);
	printf("q[0] is %i and q[1] is %i.\n\n", q._0, q._1);
	char* mem = malloc((sizeof(char))*10);

	printf("Enter something: ");
	scanf("%s", mem);
	printf("First letter of what you entered is %c.\n", mem[0]);
	printf("Second letter of what you entered is %c.\n", (mem+1)[0]);
	printf("You entered %s.\n\nRandom stuff below...\n", mem);
	free(mem);
	{
		int x = 0;
		if(num){
			printf("str is \"%s\" and num is %i.\n", str, num);
			x ? printf("hehe x is not xero.\n\n") : printf("hehe x is zero.\n\n");
		} else {
		}
	}
	{
		int i = 10;
		while(i){
			printf("i is %i and num is %i\n", i, num);
			i--;
		}
	}
	{
		int p = 0;
		switch(num){
		case 90:
			printf("num is 90. p is %i.\n", cast(p, int));
			break;
		default:
			printf("num is not 90. p is %i.\n", p);
		}
	}
	return 0;
}
