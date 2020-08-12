#include "internal/default.h"
static i32 v_main_main();
int main() {
	return v_main_main();
}
typedef u8 (v_main_u8);
typedef u16 (v_main_u16);
typedef u32 (v_main_u32);
typedef u64 (v_main_u64);
typedef i8 (v_main_i8);
typedef i16 (v_main_i16);
typedef i32 (v_main_i32);
typedef i64 (v_main_i64);
typedef void (v_main_void);
#include "std.vo.h"

#include "test2.vo.h"

static v_main_i32 (v_main_main)(v_main_void){
	v_main_printf("%i\n", v_test_test__vo_x);
	v_main_printf("%i\n", v_test_std_vo_a);
};
