
# Pointers

```
u8Ptr: &u8; // pointer to a u8

var: u8 = 0;
u8Ptr = &var; // `varPtr` holds the memory address of `var` and is an pointer to var.
```

The underlying value from a pointer can be accessed with the `*` operator.
```
var: u8 = 0;
ptr: &u8 = &var;

var2: u8 = *ptr;
```

Pointers are allowed to mutate the value they are refering to unless the pointer or the underlying value is declared using the `const` keyword.

Pointer maths is not allowed.