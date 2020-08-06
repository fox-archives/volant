#include <stdio.h>
#include "gc.h"

#define malloc(x) GC_MALLOC(x)
#define free(x) GC_FREE(x)

typedef struct {
	int a;
	int b;
} Kaka;

int main(){
	char* str = "hehehehe";
	int num = (10+10)*10;

	char* mem = malloc((sizeof(char))*10);

	Kaka p = (Kaka){.a = 0, .b = 0, };

	printf("p.a is %i and p.b is %i.\n", p.a, p.b);
	printf("Enter something: ");
	scanf("%s", mem);
	printf("First letter of what you entered is %c.\n", mem[0]);
	printf("Second letter of what you entered is %c.\n", *(mem+1));
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
			printf("num is 90. p is %i.\n", p);
			break;
		default:
			printf("num is not 90. p is %i.\n", p);
		}
	}
	printf("\nhaha eternal heap allocater goes brr..\n");
	{
		long i = 0;
		while(1){
			malloc((sizeof(char))*100);
			if((i%10000)==0){
				printf("\rallocated %li bytes", i*10000);
			} else {
			}
			++i;
		}
	}
	return 0;
}
