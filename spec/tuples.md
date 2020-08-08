# Tuples

```go
tuple Kaka {u8, u16, u32};

kaka := (Kaka){0, 0, 0};

a, b, c	:= kaka; 				// error: expected 3 values, got 1.
a, b, c	:= ..kaka; 				// success
a, b 	:= ..kaka; 				// error: expected 3 variables, got 2;
a, b, _ := ..kaka;				// success
a, b 	:= kaka[0], kaka[1];	// success
```