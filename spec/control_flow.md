
# if-else statements

```go
main := func(){
    i: u16 = 0;
    
    if i == 0 {
        print("haha i is zero")
    } else {
        print("i is not zero")
    }

    if x := 0; x < 100 {
        print("haha x is less than 100")
    } else {
        print("haha x is less than 100")
    }
};
```

# loops

```go
main := func(){
    for i: u32 = 0; i < 10; ++i {
        print("haha i'll be printed 10 times")
    }
    
    for x: u32 = 0; x++ < 10 {
        print("haha I'll be printed 10 times too")
    }
    
    x: u32 = 0
    for x++ < 10 {
        print("haha I'll be printed 10 times too")
    }

    for {
        print("haha I'll be printed forever")
    }
}
```

# switch 
```go
main := func() {
    switch x: int = 0; x {
    case 0:
        print("haha I will be printed")
        break;
    default:
        print("haha I wont be printed")
    }
    
    x: int = 0

    switch {
    case x == 0:
        print("haha I will be printed")
        break;
    default:
        print("haha I wont be printed")
    }
}
```