#include <stdio.h>
#include "gc.h"

#define malloc(x) GC_MALLOC(x)
#define free(x) GC_FREE(x)
