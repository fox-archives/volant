#include <stdio.h>
#include <stdlib.h>

int main(){
	char* str = "hehehehe";
	int num = 100+(20+(100+(10+10)));

	char* mem = malloc((sizeof(char))*10);

	printf("Exter something: ");
	scanf("%s", mem);
	printf("You entered %s.\nRandom stuff below...\n", mem);
	free(mem);
	{
		int x = 0;
		if(num){
			printf("str is \"%s\" and num is %i.\n", str, num);
		} else if(x){
			printf("hehe x is not xero.\n");
		} else {
			printf("hehe x is zero.\n");
		}
	}
	{
		int i = 10;
		while(i){
			printf("i is %i and num is %i\n", i, num);
			--i;
		}
	}
	{
		int p = 0;
		switch(num){
		case 90:
			printf("num is 90.\n");
			break;
		default:
			printf("num is not 90.\n");
		}
	}
	return 0;
}
