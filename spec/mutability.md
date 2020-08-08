# Mutability

Varuables in [lang name] are mutable by default but their size is always fixed. For creating immutable variables, `const` keyword can be used before the type to tell the compiler for not allowing any mutability for the variable.
The varaibles declared using the `const` keyowrd have to be initialized when declared.
```
variable: const u8; // error: constant variables must be initialized when declared
```
```
vraiable: const u8 = 5;
variable = 6; // error: variable is immutable
```  

The `const` keyword when used with arrays makes all its elements immutable.
```
array: const [2]u8 = [0, 1];
array[0] = 1; // error: array is immutable
```

Functions are always immutable.