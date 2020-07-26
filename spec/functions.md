# Functions

Functions in [lang name] are expressions and can be used as ordinary values.

## Declarating a function
Functions are declared like this, where the first parentheses represents the types of function argumnets and the second represents the types of return vales. Functions ca take as many arguments as they need and can return as any values as they need.
```
function: func(u8, u16, i8)(u32, float) = (a: u8, b: u16, c: i8)(u32, float){
    // code goes here
}
```
You can also implictly declare a function without specifying the type
```
function := (a: u8, b: u16, c: i8)(u32, float){
    // code goes here
}
```

For functions that return just one value or do not return anything the second parentheses can be omitted,
```
function1 := (a: u8, b: u16) u8 {
   // code goes here 
}
function2 := (a: u8, b: u16) {
   // code goes here 
}
```