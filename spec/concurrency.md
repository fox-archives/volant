# Concurrency

## Work Functions
Work functions are functions which are supposed to be executed in separate threads.

Work functions are declared using the `work` keyoword and can be executed in a separate thread asynchronously using the `queue` keyword.

```
workFunction: work func() u8 = work () u8 {
    // some stuff
} 
main := async () {
    task := queue workFunction(); // returns a promise that resolves to the return value of `workFunction`
    await task;
}
```

It is not compulsary to use work functions with the `queue` keyword, they can also be used as ordinary synchronous functions.
```
workFunction := work () u8 {
    // some stuff
}

main := async () {
    // use workFunction asynchronously in separate thread
    await queue workFunction();

    // use workFunction synchronously in current thread
    workFunction();
}
```

Work functions can only call work functions in them. They aren't allowed to interact with the uvloop in any way, i.e, they cannot perform asynchronous actions, use the `queue` keyword, or promises. They are just straight forward synchrohous functions.