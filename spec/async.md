# Asynchrnous Programming

Asynchronous control flow and event management is managed using libuv.
`async`/`await` keywords are used to manage the state of an async task.

### Creating an async function
Async functions are declared using the `async` keyword
```
asyncFunction: async func(u16) u8 = async (arg: u16) u8 {
    // async funtion
} 
```

### The `await` keyword
The await keyword is used to wait for an asynchronous task to complete. It is valid only inside async functions.

```
function := async (u16) u8 {
    promise := anyAsyncTask();
    await promise;
    return 1;
}
function2 := async (u16) u8 {
    await function();
    return 1;
}
```