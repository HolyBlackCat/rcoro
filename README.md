# ~ rcoro ~

Coroutines, reimplemented using macros.

* [Stackless](https://stackoverflow.com/a/28989543/2752075)<sup>1</sup>, like C++20 coroutines.
* Can be used **without heap allocation**<sup>2</sup>. TODO LINK HERE TO INTRODUCTION
* **Copyable** - a paused coroutine can be copied with all its stack variables.
* **Reflectable** TODO LINK HERE - examine values of individual variables in a paused coroutine.
* **Serializable**<sup>3</sup> TODO LINK HERE - dump coroutine state to a file or transfer it over network.
* Header-only, written in pure standard C++. We imitate true coroutines with copious use of macros and `goto`.

<sup>1</sup> Can pause aka "yield" only directly from the coroutine body, not from a function it calls.

<sup>2</sup> Unlike C++20 coroutines, which are allocated on the heap, unless the compiler optimizes that away.

<sup>3</sup> Hook up your preferred serialization method, [see examples](TODO LINK).

## Example usage

### A minimal coroutine
```cpp
#include <rcoro.hpp>

int main()
{
    auto c = RCORO({
        std::cout << "Hello\n";
        RC_YIELD(); // Yield, aka pause the coroutine.
        std::cout << "world!\n";
    });

    c(); // Hello
    std::cout << "...pause...\n";
    c(); // world!

    c.rewind(); // Rearm the coroutine.
    c(); // Hello
    auto c2 = c; // Copy the state...
    c(); // world!
    c2(); // world!
}
```
As you can see, our coroutines are lambda-like and unnamed by default. Each has a unique type, like a lambda.

See [wrapping coroutines in functions](TODO LINK) for how to use them as normal non-lambda functions.

### Variables
Variables must be declared with `RC_VAR(...);`. Failing to use this macro causes a compilation error. The macro is unnecessary if the variable isn't in scope at any `RC_YIELD()` calls.
```cpp
auto c = RCORO({
    // Declare a variable that should be saved into the coroutine state.
    RC_VAR(i, 3); // `int i = 5;`, the type is deduced from the initializer.

    while (i > 0)
    {
        std::cout << i << '\n';
        i--;
        RC_YIELD();
    }
});

while (c)
{
    std::cout << "...\n";
    c();
}

// Prints:
// ...
// 3
// ...
// 2
// ...
// 1
// ...
```
It's not possible to create an uninitialized variable. Use `RC_VAR(name, type{});` to only specify the type, this will zero the variable by default.

`RC_VAR` must appear as a separate statement (i.e. a separate line), you can't do `for (RC_VAR(...); ...; ...)` or `if (RC_VAR(...))`.

### `for` loops

You might want to use `RC_VAR` as a loop counter, but `for (RC_VAR(...); ...; ...)` doesn't compile, since `RC_VAR` can only appear on a separate line.

You could do following, but the variable remains visible below the loop:
```cpp
RC_VAR(i, int{});
for (i = 0; i < 10; i++)
{...}
```

We provide a macro that solves this:
```cpp
RC_FOR((i, 0); i < 10; i++) // for (int i = 0; i < 10; i++)
{...}
```
This is equivalent to the previous snippet, but the variable isn't visible below the loop, as it should be.

Example:
```cpp
auto c = RCORO({
    RC_FOR((i, 0); i < 3; i++)
    {
        std::cout << i << '\n';
        RC_YIELD();
    }
});

while (c)
{
    std::cout << "...\n";
    c();
}

// Prints:
// ...
// 0
// ...
// 1
// ...
// 2
// ...
```

Under the hood, `RC_FOR((i, 0); i < 3; i++) {...}` expands to:
```cpp
RC_WITH_VAR(i, 0)
for (; i < 3; i++)
{...}
```
`RC_WITH_VAR` is a form of `RC_VAR` that creates a variable only visible in the next statement. You can use it to replace `if (type var = ...; cond)`. You can stack several of those per statement.

### Generating values

You can return values from a coroutine:

```cpp
auto x = RCORO({
    RC_YIELD(1);
    RC_YIELD(2);
    return 3;
});

std::cout << x() << '\n'; // 1
std::cout << x() << '\n'; // 2
std::cout << x() << '\n'; // 3
```

The type is deduced automatically. The type must be the same in every `RC_YIELD` and `return`, otherwise you get a compilation error.

Coroutines always returns by value.

### Passing parameters

You can pass parameters to coroutines.

Unlike C++20 coroutines, the arguments must be passed to every call:

```cpp
auto x = RCORO((int x)
{
    while (true)
        RC_YIELD(x * 10);
});

std::cout << x(1) << '\n'; // 10
std::cout << x(2) << '\n'; // 20
std::cout << x(3) << '\n'; // 30
```

### Passing coroutines to functions

There is `rcoro::view<...>`, which is similar to `std::function<...>`, but merely stores a pointer to a coroutine, rather than owning a copy. Think `std::string_view` for coroutines.

Example:
```cpp
void run(rcoro::view<void()> c)
{
    while (c)
    {
        c();
        std::cout << "...";
    }
}

int main()
{
    auto c = RCORO({
        std::cout << 1;
        RC_YIELD();
        std::cout << 2;
        RC_YIELD();
        std::cout << 3;
    });

    run(c); // 1...2...3
}
```

Like `std::function`, `rcoro::view` is flexible in the parameter type handling. E.g. `rcoro::view<std::string>` can accept a coroutine taking `std::string_view`, and so on.

### Returning coroutines from functions and passing them around

There is `rcoro::any<...>`, which is exactly like `std::function` for coroutines. Unlike `rcoro::view`, it owns a copy of the coroutine.

```cpp
rcoro::any<void()> foo()
{
    return RCORO({
        std::cout << "Hello\n";
        RC_YIELD();
        std::cout << "world!\n";
    });
}

int main()
{
    rcoro::any<void()> c = foo();
    c(); // Hello
    c(); // world!
}
```

This lets you return coroutines from functions. (`auto` would work too, but it forces the function body to be visible at the call site, which is bad for compilation times).

For class members, you'd want to add `MyClass &self` as a coroutine parameter.

`rcoro::any` allocates the coroutine on the heap.

Like in `rcoro::view<...>`, coroutine parameter types must be specified in `<...>`.

There is also `std::any_noncopyable<>`, which supports coroutines with non-copyable variables in them (it supports absolutely any coroutines), and in turn can't be copied (like [`std::move_only_function`](https://en.cppreference.com/w/cpp/utility/functional/move_only_function)).
