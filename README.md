# ✨ rcoro ✨

Copyable, serializable coroutines, implemented with macros.

* [Stackless](https://stackoverflow.com/a/28989543/2752075)<sup>1</sup>, like C++20 coroutines.
* Can be used [**without heap allocation**](#memory-layout)<sup>2</sup>.
* **Copyable** - a paused coroutine can be copied with all its stack variables.
* [**Reflectable**](#inspecting-coroutine-variables) - examine values of individual variables in a paused coroutine.
* [**Serializable**](TODO LINK HERE)<sup>3</sup> - dump coroutine state to a file or transfer it over network.
* Unlike any other macro-coroutine library I know of, we allow variables to be declared anywhere, not only at the beginning of the coroutine. Variable lifetimes are tracked individually.
* Header-only, written in pure standard C++. We imitate true coroutines with copious use of macros and `goto`.

<sup>1 — Can pause aka "yield" only directly from the coroutine body, not from a function it calls.</sup><br/>
<sup>2 — Unlike C++20 coroutines, which are allocated on the heap, unless the compiler optimizes that away.</sup><br/>
<sup>3 — Hook up your preferred serialization method, [see examples](TODO LINK).</sup>

<details><summary><b>Table of contents</b></summary>
<p>

* [Minimal example](#minimal-example)
* [Usage](#usage)
* [Introduction](#introduction)
  * [A minimal coroutine](#a-minimal-coroutine)
  * [Variables](#variables)
  * [`for` loops](#for-loops)
  * [Generating values](#generating-values)
  * [Passing parameters](#passing-parameters)
  * [Storing coroutines in variables](#storing-coroutines-in-variables)
  * [Passing coroutines to functions](#passing-coroutines-to-functions)
  * [Debug information](#debug-information)
  * [Inspecting coroutine variables](#inspecting-coroutine-variables)
* [Reference](#reference)
  * [Coroutine state](#coroutine-state)
  * [Memory layout](#memory-layout)
  * [Syntax](#syntax)
    * [`RCORO(...)` macro](#rcoro-macro)
    * [`RC_VAR(name, init);`](#rc_varname-init)
    * [`RC_WITH_VAR(name, init)`](#rc_with_varname-init)
    * [`RC_FOR((name, init); cond; step)`](#rc_forname-init-cond-step)
    * [`return`](#return)
    * [`RC_YIELD(...)`](#rc_yield)
    * [`RC_YIELD_NAMED("name", ...)`](#rc_yield_namedname-)

</p>
</details>

<!-- To regenerate the table of contents, run:
grep -E '^##+ ' README.md | sed -E -e 's/^## /* /g' -e 's/^### /  * /g' -e 's/^#### /    * /g' | gawk '{$0 = gensub(/( *\* )(.*)/,"\\1[\\2]","g") "(#" gensub(/[^-_a-z0-9]/,"","g",gensub(/ /,"-","g",tolower(gensub(/ *\* /,"",1,$0)))) ")"; print}'
-->


## Minimal example

Obligatory fibonacci generator.

```cpp
auto fib = RCORO({
    RC_VAR(a, 0); // int a = 0;
    RC_VAR(b, 1); // int b = 1;

    RC_YIELD(a); // Return `a` and pause.

    while (true)
    {
        RC_YIELD(b); // Return `b` and pause.

        int tmp = a;
        a = b;
        b += tmp;
    }

    return -1; // Unreachable, but some compilers warn otherwise.
});

for (int i = 0; i < 5; i++)
    std::cout << fib() << '\n'; // 0 1 1 2 3

// If you feel fancy:
std::copy_n(fib.begin(), 5, std::ostream_iterator<int>(std::cout, "\n")); // 5 8 13 21 34
```

## Usage

Header-only. Just clone, add `include/` to the include path, and `#include <rcoro.hpp>`.

Supported compilers are: GCC 10+, Clang 13+, and latest MSVC.

Clang and GCC are recommended, since they're better at optimizing away the coroutine internals, with GCC being slightly behind.

Must use C++20 or newer. MSVC users must use [`/Zc:preprocessor`](https://learn.microsoft.com/en-US/cpp/build/reference/zc-preprocessor?view=msvc-170).

[`macro_sequence_for`](https://github.com/HolyBlackCat/macro_sequence_for) is a dependency. It's also header-only, clone it as well and add `macro_sequence_for/include/` to the include path.

## Introduction

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
As you can see, our coroutines are lambda-like and are unnamed by default. Each has a unique type, like a lambda.

See [*storing coroutines in variables*](#storing-coroutines-in-variables) for how to use them as normal non-lambda functions.

### Variables
Variables must be declared with `RC_VAR(...);`. Failing to use this macro causes a compilation error. The macro is unnecessary if the variable isn't in scope at any of `RC_YIELD()` calls.
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

`RC_VAR` must appear as a separate statement (roughly, a separate line), you can't do `for (RC_VAR(...); ...; ...)` or `if (RC_VAR(...))`.

### `for` loops

You might want to use `RC_VAR` as a loop counter, but `for (RC_VAR(...); ...; ...)` doesn't compile, since `RC_VAR` can only appear on a separate line.

And placing `RC_VAR` before the loop is undersirable, since the variable will still exist after the loop.

We provide a special macro for sane `for` loops:
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
auto c = RCORO({
    RC_YIELD(1);
    RC_YIELD(2);
    return 3;
});

std::cout << c() << '\n'; // 1
std::cout << c() << '\n'; // 2
std::cout << c() << '\n'; // 3
```

The type is deduced automatically. The type must be the same in every `RC_YIELD` and `return`, otherwise you get a compilation error.

`return`, with or without a value, finishes the coroutine.

Coroutines always returns by value.

Coroutines have `.begin()` and `.end()`, making them usable in `for` loops:
```cpp
auto c = RCORO({
    RC_YIELD(1);
    RC_YIELD(2);
    return 3;
});

for (auto x : c)
    std::cout << x << '\n'; // 1 2 3
```

### Passing parameters

You can pass parameters to coroutines.

Unlike C++20 coroutines, the arguments must be passed to every call:

```cpp
auto c = RCORO((int c)
{
    while (true)
        RC_YIELD(c * 10);

    return 0; // Unreachable, but some compilers warn otherwise.
});

std::cout << c(1) << '\n'; // 10
std::cout << c(2) << '\n'; // 20
std::cout << c(3) << '\n'; // 30
```

### Storing coroutines in variables

`rcoro::any<...>` to coroutines is what `std::function` is to lambdas. Example:

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

This pattern is useful for naming coroutines.

Returning `auto` would work too, but it forces the function body to be visible at the call site, which is bad for compilation times.

`std::function` also works with coroutines, but it lacks some coroutine-specific methods, like `.finished()`, `.begin()`, `.end()`.

`rcoro::any` always allocates the coroutine on the heap.

There is also `std::any_noncopyable<>`, which supports coroutines with non-copyable variables in them, and in turn can't be copied (like [`std::move_only_function`](https://en.cppreference.com/w/cpp/utility/functional/move_only_function)).

### Passing coroutines to functions

Using `rcoro::any` to pass a coroutine to a function is wasteful, because it forces a heap allocation.

There is `rcoro::view<...>`, which is similar, but merely stores a pointer to an existing coroutine. Think `std::string_view` for coroutines.

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
### Debug information

A coroutine can be printed with `<<` to an `std::ostream` to get some debug information.

You can also print `rcoro::debug_info<decltype(c)>` to get more information about the coroutine type.

### Inspecting coroutine variables

You can inspect variables of a paused coroutine:
```cpp
auto c = RCORO({
    RC_FOR((i, 0); i < 3; i++)
        RC_YIELD(i);

    RC_FOR((j, 0); j < 2; j++)
        RC_YIELD(j);

    return -1; // The final value.
});

while (c)
{
    std::cout << "--> " << c() << '\n';

    if (c.var_exists<"i">())
        std::cout << "i=" << c.var<"i">();
    else
        std::cout << "i=dead";
    std::cout << ' ';
    if (c.var_exists<"j">())
        std::cout << "j=" << c.var<"j">();
    else
        std::cout << "j=dead";
    std::cout << '\n';
}

// --> 0
// i=0 j=dead
// --> 1
// i=1 j=dead
// --> 2
// i=2 j=dead
// --> 0
// i=dead j=0
// --> 1
// i=dead j=1
// --> -1
// i=dead j=dead
```

Of course, it's also possible to list alive variables:
```cpp
auto c = RCORO({
    RC_FOR((i, 0); i < 3; i++)
        RC_YIELD(i);

    RC_FOR((j, 0); j < 2; j++)
        RC_YIELD(j);

    return -1; // The final value.
});

while (c)
{
    std::cout << "--> " << c() << '\n';

    c.for_each_alive_var([&](auto i)
    {
        std::cout << rcoro::var_name<decltype(c)>(i.value) << '=' << c.var<i.value>() << ' ';
        return false;
    });
    std::cout << '\n';
}

// --> 0
// i=0
// --> 1
// i=1
// --> 2
// i=2
// --> 0
// j=0
// --> 1
// j=1
// --> -1
```

## Reference

### Coroutine state

A coroutine can be in three states:

* Currently running (`.busy()`)
* Finished (`.finished()`), inspect `.finish_reason()` for why it's finished.
* Paused (otherwise)
  * At the beginning, as returned from `RCORO()`. Indicated by `.yield_point() == 0`.
  * At `RC_YIELD()`, indicated by `.yield_point() > 0`.

State | `.busy()` | `.finished()` | `.finish_reason()` | `.yield_point()`
---|---|---|---|---
Currently running|✅`true`|❌`false`|`not_finished`|`0`..`N-1`
Just returned from `RCORO()`, or after `.rewind()`|❌`false`|❌`false`|`not_finished`|`0`
Paused at `RC_YIELD()`|❌`false`|❌`false`|`not_finished`|`1`..`N-1`
Finished normally or via `return`|❌`false`|✅`true`|`success`|`0`
Finished via exception|❌`false`|✅`true`|`exception`|`0`
After `.reset()`, or default-constructed, or moved-from|❌`false`|✅`true`|`reset`|`0`
Null `any<...>` or `any_noncopyable<...>` or `view<...>`|❌`false`|✅`true`|`null`|`0`

`operator bool` returns `!finished()`.

As you can see, `.finish_reason() != not_finished` if and only if `.finished() == true`.

Also `.finished()` implies `.yield_point() == 0`. Normally `0` is used for the coroutines paused at the very beginning, but it's also reused for finished coroutines.

Calling almost any other method on a `.busy()` coroutine throws. Trying to destroy or copy/move it terminates the program (don't want to forego `noexcept` just to report this error).

Calling `operator()` throws if the coroutine is `.busy()` or `.finished()`.

### Memory layout

Each coroutine object stores two extra ints (the current `RC_YIELD` point, and a state enum), and all `RC_VAR` variables by value.

Storage for different variables can overlap, if they don't exist at the same time. If a variable isn't visible at any `RC_YIELD` point, it's not stored in the coroutine object at all.

`any<...>`, `any_noncopyable<...>`, `view<...>` all occupy two pointers: to the target object and to a vtable.

`any<...>` and `any_noncopyable<...>` always allocate on the heap, they don't have embedded storage like `std::function` commonly does.

### Syntax

#### `RCORO(...)` macro

Defines a coroutine. All other macros are only usable inside of `RCORO(...)`.

Usage:
* `RCORO({...})` — without parameters.
* `RCORO((...){...})` — with parameters.

  The parameters can have default arguments, but they can't contain `auto`, and can't be followed by anything (no `-> return_type`, no `noexcept`, etc).

<sup>Strictly speaking, the braces are not necessary, but A: Clang-format doesn't work otherwise, and B: if you omit braces and parameters, the body can't start with `(`, which would then be considered a parameter list.</sup>

You can't refer to outside variables in the coroutine body, as if in a non-capturing lambda. Pass them as parameters, or store in `RC_VAR`s set from outside using `.var<"...">() = ...;`.

`RCORO(...)` returns an object of type `rcoro::specific_coro<T>`, where `T` is a unique opaque type.

The resulting object is copyable and movable, if all the variables are.

`RCORO(...)` can't appear inside of `decltype(...)` (GCC rejects this, but Clang and MSVC accept).

#### `RC_VAR(name, init);`

The type is deduced from the initializer, and is never a reference. Any types are supported, even non-movable ones.

This macro only makes sense if the variable lifetime overlaps a `RC_YIELD` point. Otherwise it's equivalent to a simple local variable, and the macro just wastes compilation time (it doesn't waste memory though, it won't be stored in the coroutine object, but rather on the stack).

While (another) `RCORO(...)` can be a variable initializer, it's not recommended, as the build time cost of the nested coroutine doubles. It's better to declare the second coroutine outside of the first one.

#### `RC_WITH_VAR(name, init)`

A variant of `RC_VAR` that creates a variable only visible at the next statement.

Example:
```cpp
RC_WITH_VAR(x, 1) // Note, no `;`.
if (x == 1)
    RC_YIELD(x);
if (x == 1) // Error, `x` is already dead here.
    RC_YIELD(x);
```

`RC_WITH_VAR` can be followed by braces. Following it by `;` is pointless, as it destroys the variable immediately.

Several `RC_WITH_VAR` can be stacked.

#### `RC_FOR((name, init); cond; step)`

A sane `for` loop.

Can be followed by braces or by a single body line.

Exactly equivalent to `RC_WITH_VAR(name, init); for(; cond; step)`.

#### `return`

`return` immediately stops the coroutine, and makes it `.finished()`.

You can return a value, but then all `RC_YIELD`s and all other `return`s must return a value of the same type.

In this regard, `return` is like a form of `RC_YIELD()` that also finishes the coroutine.

#### `RC_YIELD(...)`

Pauses the coroutine.

The parameter is optional. If specified, it's returned from the coroutine.

All `RC_YIELD`s in a coroutine must return the same type, or all must not return anything.

All `return`s must also return the same type, or nothing.

#### `RC_YIELD_NAMED("name", ...)`

Same as `RC_YIELD`, but this yield point is named. Good for serialization/deserialization, if you don't want to just store the incremental index of a `RC_YIELD` point.

The plain `RC_YIELD` uses `""` as the name, and so does the implicit yield point at the beginnning of a coroutine.

The second parameter is the return value. It is optional, like in `RC_YIELD`.

Check `rcoro::yield_names_are_unique<decltype(coro)>` to see if all yield names are unique. Since the implicit first yield point uses `""` as the name, this requires all other yields to have non-empty names.
