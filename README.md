# ✨ rcoro ✨

Copyable, serializable coroutines with reflection, implemented using macros.

[![tests badge](https://github.com/HolyBlackCat/rcoro/actions/workflows/tests.yml/badge.svg?branch=master)](https://github.com/HolyBlackCat/rcoro/actions?query=branch%3Amaster)<br/>
<kbd>[try on gcc.godbolt.org][1]</kbd>

* [Stackless](https://stackoverflow.com/a/28989543/2752075)<sup>1</sup>, like C++20 coroutines.
* Can be used [**without heap allocation**](#memory-layout)<sup>2</sup>.
* **Copyable** - a paused coroutine can be copied with all its stack variables.
* [**Reflectable**](#inspecting-coroutine-variables) - examine values of individual variables in a paused coroutine.
* [**Serializable**](#serialization--deserialization)<sup>3</sup> - dump coroutine state to a file or transfer it over network.
* Unlike any other macro-coroutine library I know of, this one allows variables to be declared anywhere, not only at the beginning of the coroutine. Variable lifetimes are tracked individually, at compile-time. Storage of dead variables is reused for other variables, and so on.
* Exception-safe, everything is guarded with RAII, etc.
* Header-only, written in pure standard C++. Imitates true coroutines with copious use of macros and `goto`.

Read about [curious implementation details.](/docs/cool_tricks.md)

<sup>1 — Can pause aka "yield" only directly from the coroutine body, not from a function it calls.</sup><br/>
<sup>2 — Unlike C++20 coroutines, which are normally allocated on the heap, unless the compiler optimizes that away.</sup><br/>
<sup>3 — Must bring your own serializer, [see examples](#serialization--deserialization).</sup>

<details><summary><b>Table of contents</b></summary>
<p>

* [Minimal example](#minimal-example)
* [Usage](#usage)
  * [Using debugger](#using-debugger)
  * [Debug info](#debug-info)
* [Introduction](#introduction)
  * [A minimal coroutine](#a-minimal-coroutine)
  * [Variables](#variables)
  * [`for` loops](#for-loops)
  * [Generating values](#generating-values)
  * [Passing parameters](#passing-parameters)
  * [Storing coroutines in variables](#storing-coroutines-in-variables)
  * [Passing coroutines to functions](#passing-coroutines-to-functions)
  * [Inspecting coroutine variables](#inspecting-coroutine-variables)
* [Reference](#reference)
  * [Macro summary](#macro-summary)
  * [Coroutine state](#coroutine-state)
  * [Memory layout](#memory-layout)
  * [More details on macros](#more-details-on-macros)
    * [`RCORO({...})` macro](#the-rcoro-macro)
    * [`RC_VAR(name, init);`](#rc_varname-init)
    * [`RC_WITH_VAR(name, init)`](#rc_with_varname-init)
    * [`RC_FOR((name, init); cond; step)`](#rc_forname-init-cond-step)
    * [`return`](#return)
    * [`RC_YIELD(...)`](#rc_yield)
    * [`RC_YIELD_NAMED("name", ...)`](#rc_yield_namedname-)
  * [Type traits](#type-traits)
* [Serialization & deserialization](#serialization--deserialization)
  * [Basic serialization](#basic-serialization)
  * [Basic deserialization](#basic-deserialization)
  * [Serializing and deserializing `rcoro::any<...>`](#serializing-and-deserializing-rcoroany)

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

Supported compilers are: GCC 10+ (tested up to 13), Clang 14+ (tested up to 17), and the latest MSVC (last tested on 19.38). Clang and GCC are recommended, since they're better at optimizing away the coroutine internals, with GCC being slightly behind.

Must use C++20 or newer. MSVC users must use [`/Zc:preprocessor`](https://learn.microsoft.com/en-US/cpp/build/reference/zc-preprocessor?view=msvc-170).

[`macro_sequence_for`](https://github.com/HolyBlackCat/macro_sequence_for) is a dependency. It's also header-only, clone it as well and add `macro_sequence_for/include/` to the include path.

If you're using a compiler version I didn't test for, consider building and running `tests.cpp` first.

### Using debugger

Line number information is lost in macro expansion, so debuggers are unable to place breakpoints in coroutines, nor step in them line by line. This can be temporarily fixed by expanding the `RCORO(...)` macro (easiest to do with VSCode+Clangd, position the cursor on `RCORO` and hit <kbd>Shift</kbd><kbd>Enter</kbd>->`Expand macro`).

### Debug info

Coroutines can be printed with `<<` to `std::ostream` to see the variable values and the current yield point.

Also print `rcoro::debug_info<decltype(c)>` to get complete information about the coroutine type.

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

See [*storing coroutines in variables*](#storing-coroutines-in-variables) for how to wrap them in normal functions.

You can't refer to outside variables in the coroutine body, as if in a non-capturing lambda. Pass them as parameters, or store in `RC_VAR`s set from outside using `.var<"...">() = ...;`.

### Variables
Variables must be declared with `RC_VAR(...);`. Failing to use this macro causes a compilation error. The macro is unnecessary and wastes compilation time if the variable isn't in scope at any of `RC_YIELD()` calls.
```cpp
auto c = RCORO({
    // Declare a variable that should be saved into the coroutine state.
    RC_VAR(i, 3); // `int i = 3;`, the type is deduced from the initializer.

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
It's not possible to create an uninitialized variable. Use `RC_VAR(name, type{});` to specify just the type, this will zero the variable.

`RC_VAR` must appear as a separate statement (roughly, a separate line), so `for (RC_VAR(...); ...; ...)` is illegal, and so is `if (RC_VAR(...))`.

### `for` loops

Again, `for (RC_VAR(...); ...; ...)` is illegal, `RC_VAR` can only appear on a separate line.

And placing `RC_VAR` before the loop is undesirable, since the variable will still exist after the loop.

There is a separate macro for sane `for` loops:
```cpp
RC_FOR((i, 0); i < 10; i++) // for (int i = 0; i < 10; i++)
{...}
// `i` doesn't exist after the loop.
```

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

Coroutines can return values:

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

The return type is deduced automatically. The type must be the same in every `RC_YIELD` and `return`, otherwise you get a compilation error.

`return` finishes the coroutine (in addition to returning the value, if any).

Coroutines always return by value.

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
The iterator type is `rcoro::iterator<ReturnType>`, it's not unique per coroutine.

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

### Macro summary

Element | Meaning
---|---
`RCORO({...})`<br/>`RCORO((...){...})` | Coroutine without and with parameters.
`RC_VAR(name, init);` | Declare a variable, as if by `auto name(init);`.<br/>
`RC_WITH_VAR(name, init)`<br/>(note, no `;`) | Same, but the variable is only visible in the next statement.
`RC_FOR((name, init); cond; step)`<br/>`{...}` | A `for` loop with an `RC_VAR` variable. Expands to<br/>`RC_WITH_VAR(name, init)`<br/>` for (; cond; step) {...}`.
`RC_YIELD()`<br/>`RC_YIELD(value)` | Pause the coroutine, without or with a return value.
`RC_YIELD_NAMED("name")`<br/>`RC_YIELD_NAMED("name", value)` | Same, but the yield point is named.<br/>The unnamed version uses `""` as the name.

### Coroutine state

A coroutine can be in three states:

* Currently running (`.busy()`)
* Finished (`.finished()`), inspect `.finish_reason()` for why it's finished.
* Paused (otherwise)
  * At the beginning, as returned from `RCORO()`. Indicated by `.yield_point() == 0`.
  * At `RC_YIELD()`, indicated by `.yield_point() > 0`.

State | `.busy()` | `.finished()` | `.finish_reason()` | `.yield_point()`
---|---|---|---|---
Currently running|🟢`true`|❌`false`|`not_finished`|`0`..`N-1`
Just returned from `RCORO()`,<br/>or after `.rewind()`|❌`false`|❌`false`|`not_finished`|`0`
Paused at `RC_YIELD()`|❌`false`|❌`false`|`not_finished`|`1`..`N-1`
Finished normally<br/>or via `return`|❌`false`|🟢`true`|`success`|`0`
Finished via exception|❌`false`|🟢`true`|`exception`|`0`
After `.reset()`, or<br/>default-constructed,<br/>or moved-from|❌`false`|🟢`true`|`reset`|`0`
Null `any<...>`, or<br/>`any_noncopyable<...>`,<br/>or `view<...>`|❌`false`|🟢`true`|`null`|`0`

`operator bool` returns `!finished()`.

As you can see, `.finish_reason() != not_finished` if and only if `.finished() == true`.

Also `.finished()` implies `.yield_point() == 0`. Normally `0` is used for the coroutines paused at the very beginning, but it's also reused for finished coroutines.

Calling almost any other method on a `.busy()` coroutine throws. Trying to destroy or copy/move it terminates the program (don't want to forego `noexcept` just to report this error).

Calling `operator()` throws if the coroutine is `.busy()` or `.finished()`.

### Memory layout

The object returned from `RCORO({...})` doesn't perform any heap allocations.

We calculate the required storage size at compile-time, and store all necessary variables in a single byte array.

Each coroutine object also stores two extra ints (the current `RC_YIELD` point index, and a state enum).

Storage for different variables can overlap, if they don't exist at the same time. If a variable isn't visible at any `RC_YIELD` point, it's not stored in the coroutine object at all.

`any<...>`, `any_noncopyable<...>`, `view<...>` all occupy two pointers: to the target object and to a vtable.

`any<...>` and `any_noncopyable<...>` always allocate on the heap, they don't even have embedded storage like `std::function` commonly does.

### More details on macros

#### The `RCORO({...})` macro

All other macros are only usable inside of `RCORO(...)`.

Strictly speaking, the braces are not necessary, but they look cool, and Clang-format doesn't work otherwise, and without braces the body can't start with `(`, or it will be confused for a parameter list.

Parameters can have default arguments.

`RCORO(...)` returns an object of type `rcoro::specific_coro<T>`, where `T` is a unique opaque type.

The resulting object is copyable and movable, if all the variables are.

`RCORO(...)` can't appear inside of `decltype(...)` (GCC rejects this, but Clang and MSVC accept).

#### `RC_VAR(name, init);`

Must be a separate statement. That is, can appear inside of `{...}` after `for`/`if`, but can't be used as `for (RC_VAR(...); ...; ...)` or `if (RC_VAR(...))`.

The type is deduced from the initializer, and is never a reference. Any types are supported, even non-movable ones.

Only makes sense if the variable lifetime overlaps a `RC_YIELD` point. Otherwise it's equivalent to a simple local variable, and the macro just wastes compilation time (it doesn't waste memory though, it won't be stored in the coroutine object, but rather on the stack).

While (another) `RCORO(...)` can be a variable initializer, it's not recommended, as the build time cost of the nested coroutine doubles. It's better to declare the second coroutine outside of the first one.

#### `RC_WITH_VAR(name, init)`

Creates a variable only visible at the next statement. Example:
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

Exactly equivalent to `RC_WITH_VAR(name, init); for(; cond; step)`.

#### `return`

`return` immediately stops the coroutine, and makes it `.finished()`.

You can return a value, but then all `RC_YIELD`s and all other `return`s must return a value of the same type.

In this regard, `return` is like a form of `RC_YIELD()` that also finishes the coroutine.

#### `RC_YIELD(...)`

The parameter is optional. If specified, it's returned from the coroutine.

All `RC_YIELD`s in a coroutine must return the same type, or all must not return anything.

All `return`s must also return the same type, or nothing.

#### `RC_YIELD_NAMED("name", ...)`

Good for serialization/deserialization, if you don't want to just store the incremental index of a `RC_YIELD` point.

The plain `RC_YIELD` uses `""` as the name, and so does the implicit yield point at the beginnning of a coroutine.

The second parameter is the return value. It is optional, like in `RC_YIELD`.

Check `rcoro::yield_names_are_unique<decltype(coro)>` to see if all yield names are unique. Since the implicit first yield point uses `""` as the name, this requires all other yields to have non-empty names.

### Type traits

`namespace rcoro` contains numerous type traits to inspect the type returned by `RCORO(...)`:

* Return type — `return_type<T>`
* Variables:
  * Count — `num_vars<T>`
  * Types — `var_type<T>`
  * Names — `var_name<T>(i)` and others
    * Mapping names back to indices — `var_index<T>("name")` and others
    * Check for uniqueness — `var_names_are_unique_per_yield<T>`
  * What storage is allocated — `frame_size<T>`, `frame_alignment<T>`, `frame_is_trivially_copyable<T>`, `var_offset<T, i>`, `var_lifetime_overlaps_var<T, i, j>`
* Yield points:
  * Count — `num_yields<T>`
  * Names — `yield_name<T>(i)` and others
    * Mapping names back to indices — `yield_index<T>("name")` and others
    * Check for uniqueness — `yield_names_are_unique<T>`
  * Relation to variables:
    * Which variables exist here — `yield_vars<T>(i)`, `var_lifetime_overlaps_yield<T>(i, j)`, and others

## Serialization & deserialization

### Basic serialization

The general algorithm is as follows:

* If `.busy()`, fail.
* Serialize enum `.finish_reason()`.<br/>
  Alternatively, just a boolean `.finished()`, if you don't care about the precise finish reason.

* If `.finish_reason() != not_finished` (same as `.finished() == true`), stop.

* Serialize int `.yield_point()` or string `.yield_point_name()`.<br/>
  If you use the string, `static_assert` `yield_names_are_unique`.<br/>
  Make sure you can handle an empty string, returned for `.yield_point() == 0`.

* Serialize variables using [`.for_each_alive_var`](#inspecting-coroutine-variables).<br/>
  Either include variable names or don't.<br/>
  If you do, `static_assert` `var_names_are_unique_per_yield`.

  You can include the variable count, if you want to validate it - `rcoro::yield_vars<T>(coro.yield_point()).size()` (this matches the number of iterations of `.for_each_alive_var`).

  * Alternatively, you can serialize raw bytes instead of separate variables, if all your variable types are trivial.

    `static_assert` `frame_is_trivially_copyable<T>`, and serialize `frame_size<T>` bytes from `.frame_storage()`.

Generally, if you know the storage format is stable, don't serialize variable/yield names. (E.g. when doing networking between applications known to have the exact same version.) And if the format can change, serialize the names, which will then allow you to tolerate minor coroutine changes, and/or fix the data to account for major changes.

A toy example is below. I wouldn't use text `std::ostream` in the real world, and would recommend binary, JSON, etc. This does save both yield and variable names.

<details><summary><b>Example: <code>serialize()</code></b></summary>
<p>

```cpp
template <rcoro::specific_coro_type T>
std::string serialize(const T &c)
{
    std::ostringstream ss;
    ss << int(c.finish_reason()) << '\n';
    if (!c.finished())
    {
        // Saving yield names: yes, hence assert.
        static_assert(rcoro::yield_names_are_unique<T>);
        // Quotes to support empty strings, since 0th name is always empty.
        ss << std::quoted(c.yield_point_name()) << '\n';

        c.for_each_alive_var([&](auto i)
        {
            static_assert(rcoro::var_names_are_unique_per_yield<T>);
            ss << rcoro::var_name<T>(i.value) << ' ' << c.template var<i.value>() << '\n';
            return false;
        });
    }
    return std::move(ss).str();
}

int main()
{
    auto fib = RCORO({
        RC_VAR(a, 0);
        RC_VAR(b, 1);

        RC_YIELD_NAMED("before_loop", a);

        while (true)
        {
            RC_YIELD_NAMED("in_loop", b);

            int tmp = a;
            a = b;
            b += tmp;
        }

        return -1;
    });

    // Run for a bit.
    fib(); fib(); fib(); fib();

    std::cout << serialize(fib) << '\n';
}
```
</p></details>

I used a modified fibonacci example from the beginning, with named yield points.
```cpp
0         // `.finish_reason() == 0`, aka `not_finished`.
"in_loop" // `.yield_point_name()` for the second `RC_YIELD` in the code.
a 1       // a=1
b 2       // b=2
```

### Basic deserialization

The algorithm is as follows:

* If the target coroutine is `.busy()`, fail.

* Read enum `rcoro::finish_reason`, or a boolean (depending on how you serialized it, see above). If you used a boolean, cast it to enum using `rcoro::finish_reason(boolean)`.

* If you got `finish_reason == not_finished` (or, false boolean), read the yield position: either an integer or a string (depending on how you serialized it, see above). Convert string to integer using `yield_index<T>("name")`.

  And if `finish_reason != not_finished` (or, true boolean), set yield position to `0`.

* Read variables. You have three options:

  * If you didn't serialize variable names, use `.load_ordered()`. It will tell you which and how many variables to read.
  * If you did serialize variable names, use `.load_unordered()`. You must know when to stop reading (know how many variables you have serialized).
  * If you serialized raw bytes, use `.load_raw_bytes()`.

All those functions are robust, and will throw if you do something wrong. You don't have to do any extra input validation, if you're fine with getting an exception. You can throw from the callbacks, and they will never leak objects.

All functions return `true` on success, and `false` if you aborted the load (each function uses a different way of aborting).

All functions accept `rcoro::finish_reason` and `int yield_index`. The latter must be zero if `finish_reason != not_finished`.

* `.load_raw_bytes` is easy:

  ```cpp
  c.load_raw_bytes(finish_reason, yield_index, []
  {
      // Load the bytes back into `c.frame_storage()`.
      return true; // Return false to cancel.
  }
  ```

  The lambda you pass is called once.

* `.load_ordered` is slightly more complicated:

  ```cpp
  c.load_ordered(finish_reason, yield_index, [](auto index, auto construct)
  {
      // Read a variable of type `rcoro::var_type<decltype(c), index.value>`.
      // Pass it to `construct(...)`.
      // To abort, just don't call `construct()`.
  });
  ```
  The lambda is called repeatedly for every variable that needs to be loaded.

* Lastly, `.load_unordered` will be demonstrated with a full example, matching our toy serializer above:

<details><summary><b>Example: <code>deserialize()</code></b></summary>
<p>

```cpp
template <typename T>
void deserialize(std::string source, T &coro)
{
    std::istringstream ss(std::move(source));

    // Read finish reason.
    int fin_reason_int = -1;
    if (!(ss >> fin_reason_int))
        throw std::runtime_error("bad finish reason");
    rcoro::finish_reason fin_reason = rcoro::finish_reason(fin_reason_int);

    // Read yield index.
    int yield_point = 0; // Must default to `0`.
    if (fin_reason == rcoro::finish_reason::not_finished)
    {
        std::string name;
        if (!(ss >> std::quoted(name)))
            throw std::runtime_error("bad yield point");
        yield_point = rcoro::yield_index<T>(name); // Throws on bad name.
    }

    coro.load_unordered(fin_reason, yield_point, [&](auto var)
        {
            // This lambda is called once.

            // Read next variable name, stop if no more variables.
            std::string var_name;
            while (ss >> var_name)
            {
                // Determine variable index. This automatically throws if the name is wrong.
                // Don't use `var_index()` without `..._at_yield`, as it doesn't understand duplicate variable names even when the variables don't coexist.
                int var_index = rcoro::var_index_at_yield<T>(yield_point, var_name);

                // This calls the second lambda.
                // Can add any arbitrary arguments, they will be passed to the second lambda.
                var(var_index);
            }

            return true; // This checks that all variables are loaded, throws on failure. Return false to abort without those checks.
        },
        [&](auto var_index, auto construct)
        {
            // This lambda loads a single variable, it's called when the first one calls `var()`.

            // Here we know the desired type.
            rcoro::var_type<T, var_index.value> var;
            ss >> var;
            if (!ss)
                // Or just return without calling `construct`, and somehow notify the first lambda that it should fail.
                throw std::runtime_error("bad variable");

            construct(std::move(var));
        }
    );
}

int main()
{
    auto fib = RCORO({
        RC_VAR(a, 0);
        RC_VAR(b, 1);

        RC_YIELD_NAMED("before_loop", a);

        while (true)
        {
            RC_YIELD_NAMED("in_loop", b);

            int tmp = a;
            a = b;
            b += tmp;
        }

        return -1;
    });

    std::string source = R"(
        0
        "in_loop"
        a 1
        b 2
    )";

    deserialize(std::move(source), fib);

    std::cout << fib() << '\n'; // 3, which is the next number.
}
```

</p>
</details>

Here we use the same coroutine, and the string created by the previous example.

The first lambda is called once. It's responsible for reading the variable name, then passing it (as index) to its parameter `var()`.

The second lambda is responsible for loading a single variable, and it's called whenever the first lambda calls `var()`.

### Serializing and deserializing `rcoro::any<...>`

With `any`, things get more complicated. Firstly, it lacks \[de\]serialization methods, because those are templates (for the same reason you can't template virtual functions).

Luckily, `any` is extensible, and you can extend it with *specific* (not templated) \[de\]serialization methods.

We will create `my_any<...>` that extends `any<...>` with what we need.

This is quite verbose, but it exactly mimics how `any` itself extends `any_noncopyable` (with copyability), which in turn extends yet another internal class. So you can always look into the library code for inspiration.

<details><summary><b>Example: unfinished <code>any</code> [de]serialization</b></summary>
<p>

```cpp
// Paste `serialize()` and `deserialize()` from the previous examples here.

// This stores function pointers to any functions we need to support.
// We inherit from an existing class, because there are some internal functions here too.
template <typename R, typename ...P>
struct basic_my_any_vtable : rcoro::type_erasure_bits::basic_any_vtable<R, P...>
{
    // Our function pointers.
    std::string (*save)(const void *) = nullptr;
    void (*load)(void *, std::string) = nullptr;

    template <typename T>
    constexpr void fill()
    {
        // Forward to the parent method.
        rcoro::type_erasure_bits::basic_any_vtable<R, P...>::template fill<T>();
        // Call the `serialize()`/`deserialize()` methods from the previous examples.
        save = [](const void *c){return serialize(*static_cast<const T *>(c));};
        load = [](void *c, std::string s){deserialize(std::move(s), *static_cast<T *>(c));};
    }
};

// An intermediate class. Not strictly necessary, but will be convenient
// if you decide to *further* extend your own `any` variant in yet another class.
template <typename Derived, typename Vtable, typename R, typename ...P>
class basic_my_any : public rcoro::type_erasure_bits::basic_any<Derived, Vtable, R, P...>
{
    using base = rcoro::type_erasure_bits::basic_any<Derived, Vtable, R, P...>;

  public:
    using base::base;

    // Our user-facing methods.
    std::string save() const
    {
        // `save()` is defined in our `basic_my_any_vtable`.
        return this->vptr->save(this->memory);
    }
    void load(std::string value) const
    {
        // `load()` is defined in our `basic_my_any_vtable`.
        return this->vptr->load(std::move(value), this->memory);
    }
};

// Finally, the actual class we're making.
template <rcoro::func_type T>
class my_any;
template <typename R, typename ...P>
class my_any<R(P...)> : public basic_my_any<my_any<R(P...)>, basic_my_any_vtable<R, P...>, R, P...>
{
    using base = basic_my_any<my_any<R(P...)>, basic_my_any_vtable<R, P...>, R, P...>;

  public:
    using base::base;

    my_any(const my_any &) = default;
    my_any(my_any &&) = default;
    // Unsure why this is needed, but without this, we get `nothrow_copy_assignable == true`, which is wrong. Looks like a Clang bug.
    my_any &operator=(const my_any &) noexcept(false) = default;
    my_any &operator=(my_any &&) = default;
};
```

</p>
</details>

Now, `my_any<...>` is for all purposes equivalent to `rcoro::any<...>`, but with two extra functions, `save()` and `load()`.

This is sweet, but for `.load()` to do its thing, you must first somehow load the correct coroutine **type** into `my_any`, since `.load()` can only adjust state of an existing coroutine.

This is equivalent to the problem of \[de\]serializing a derived class, while only working with a pointer to its base.

How do we fix this? We need to invent *names* for specific coroutines and store them in `my_any`. We need to serialize the name, and when deserializing, use the name to construct the correct coroutine type first, before performing the rest of deserialization.

The syntax then becomes `my_any<void()> x = my_coro<"name">(RCORO({...}));`.

For safety, it might be a good idea to instead do `my_any<"category", void()> x = make_my_any<"category", "name">(RCORO({...}));`, to split coroutine names into groups, limiting which coroutine can be loaded where, to guard against malevolent input. This is left as an exercise to the reader.

We press onward.

<details><summary><b>Example: <code>any</code> [de]serialization</b></summary>
<p>

```cpp
// Paste `serialize()` and `deserialize()` from the previous examples here.

// A return type for `my_coro()`.
template <typename T, rcoro::const_string Name>
struct my_coro_type
{
    T value; // The coroutine.
    using coro_type = T;
    static constexpr std::string_view name = Name.view();
};
// This function consturcts `my_coro_type` with a specific name.
template <rcoro::const_string Name, typename T>
[[nodiscard]] my_coro_type<std::remove_cvref_t<T>, Name> my_coro(T &&coro)
{
    return {std::forward<T>(coro)};
}

// This stores maps, mapping coroutine names to functions to construct them.
// This is templated, to group coroutines by signatures.
// We use `Vtable` as a template parameter (ultimately `basic_my_any_vtable<...>`),
//   instead of directly the return type and parameter types,
//   because if somebody futher extends your `my_any` class, they'll need separate
//   maps, using their vtable class, not yours.
// We use a function instead of a global variable, because variables are initialized
//   too late, while we need it to be initialized on demand, when we first register
//   our coroutines here.
template <typename Vtable>
auto &my_coro_map()
{
    // This requires some understanding how `rcoro::any` works. It contains two pointers:
    // `void *memory`, pointing to the allocated object, and `Vtable *vptr`, pointing to what we call a vtable.
    // In our example, `vptr` points to `basic_my_any_vtable`.
    static std::map<std::string_view, void (*)(const Vtable *&vptr, void *&memory)> ret;
    return ret;
}

// Instantiating this variable registers a coroutine type,
// and the registration happens when the program starts,
// regardless of where and when the variable is used.
// `MyCoroType` is `my_coro_type<...>`.
template <typename MyCoroType, typename Vtable>
const std::nullptr_t register_my_coro = []{
    auto lambda = [](const Vtable *&vptr, void *&memory)
    {
        // The `my_coro_type<T, Name>` is an extra parameter that goes straight to `basic_my_any_vtable::fill<...>()`,
        // see below.
        vptr = &rcoro::type_erasure_bits::vtable_storage<typename MyCoroType::coro_type, Vtable, MyCoroType>;
        memory = new typename MyCoroType::coro_type;
    };
    // Insert into the map.
    bool ok = my_coro_map<Vtable>().try_emplace(MyCoroType::name, lambda).second;
    // Check for duplicate names.
    // This can give you false positives if you have multiple DLLs, and register
    // coroutine in a header. Fixing this is left as an exercise to the reader,
    // or just don't do that, since coroutines are expensive to compile and don't belong in headers.
    if (!ok)
        throw std::runtime_error("Duplicate coroutine name: " + std::string(MyCoroType::name));

    return nullptr; // Have to return *something*.
}();

// This stores function pointers to any functions we need to support.
// We inherit from an existing class, because there are some internal functions here too.
template <typename R, typename ...P>
struct basic_my_any_vtable : rcoro::type_erasure_bits::basic_any_vtable<R, P...>
{
    // Our function pointers.
    std::string (*save)(const void *) = nullptr;
    void (*load)(void *, std::string) = nullptr;

    // `Wrapper` is a custom parameter we later pass to `vtable_storage<...>`.
    template <typename T, typename Wrapper>
    constexpr void fill()
    {
        rcoro::type_erasure_bits::basic_any_vtable<R, P...>::template fill<T>();
        save = [](const void *c)
        {
            // Now we include the type name here.
            std::string ret(Wrapper::name);
            ret += '\n';
            ret += serialize(*static_cast<const T *>(c));
            return ret;
        };
        load = [](void *c, std::string s)
        {
            // This stays unchanged, the name is processed elsewhere.
            deserialize(std::move(s), *static_cast<T *>(c));
        };
    }
};

// An intermediate class. Not strictly necessary, but will be convenient
// if you decide to *further* extend your own `any` variant in yet another class.
template <typename Derived, typename Vtable, typename R, typename ...P>
class basic_my_any : public rcoro::type_erasure_bits::basic_any<Derived, Vtable, R, P...>
{
    using base = rcoro::type_erasure_bits::basic_any<Derived, Vtable, R, P...>;

    // We no longer inherit the constructors with `using base::base`,
    // since we want to limit our object to only be created with `my_coro<"name">(...)`.

  public:
    constexpr basic_my_any() {}

    template <typename T, rcoro::const_string Name>
    constexpr basic_my_any(my_coro_type<T, Name> &&source)
    {
        // Here we pass an extra argument `my_coro_type<T, Name>`, which goes
        // straight to `basic_my_any_vtable::fill<...>()`.
        this->vptr = &rcoro::type_erasure_bits::vtable_storage<T, Vtable, my_coro_type<T, Name>>;
        this->memory = new T(std::move(source.value));

        // Poke `register_my_coro` to force it to run the registration code when the program starts.
        [[maybe_unused]] auto dummy = register_my_coro<my_coro_type<T, Name>, Vtable>;
    }

    std::string save() const
    {
        // `save()` is defined in our `basic_my_any_vtable`.
        return this->vptr->save(this->memory);
    }
    // This is now static.
    static Derived load(std::string value)
    {
        // Extract the name from `value`.
        auto pos = value.find_first_of('\n');
        if (pos == std::string::npos)
            throw std::runtime_error("Expected newline after the coroutine name.");
        std::string name = value.substr(0, pos); // Extract the name.
        value = std::move(value).substr(pos + 1); // Remove the name from the string.

        Derived ret; // `my_any<R(P...)> ret;`, unless somebody further extends your class.

        // Load the correct type using our function map.
        my_coro_map<Vtable>().at(name)(ret.vptr, ret.memory);
        // Lastly, deserialize the coroutine.
        ret.vptr->load(ret.memory, std::move(value));

        return ret; // Boom, we're done.
    }
};

// Finally, the actual class we're making.
template <rcoro::func_type T>
class my_any;
template <typename R, typename ...P>
class my_any<R(P...)> : public basic_my_any<my_any<R(P...)>, basic_my_any_vtable<R, P...>, R, P...>
{
    using base = basic_my_any<my_any<R(P...)>, basic_my_any_vtable<R, P...>, R, P...>;

  public:
    using base::base;

    my_any(const my_any &) = default;
    my_any(my_any &&) = default;
    // Unsure why this is needed, but without this, we get `nothrow_copy_assignable == true`, which is wrong. Looks like a Clang bug.
    my_any &operator=(const my_any &) noexcept(false) = default;
    my_any &operator=(my_any &&) = default;
};
```

</p>
</details>

And the usage:
```cpp
int main()
{
    my_any<int()> x = my_coro<"fib">(RCORO({
        RC_VAR(a, 0);
        RC_VAR(b, 1);

        RC_YIELD_NAMED("before_loop", a);

        while (true)
        {
            RC_YIELD_NAMED("in_loop", b);

            int tmp = a;
            a = b;
            b += tmp;
        }

        return -1;
    }));

    x(); x(); x(); x(); // Run for a bit.

    std::cout << x.save() << '\n';

    // This prints:
    //     fib      <- Note the coroutine name getting added here.
    //     0
    //     "in_loop"
    //     a 1
    //     b 2

    // Now create some unrelated variable
    my_any<int()> y = my_any<int()>::load(R"(fib
        0
        "in_loop"
        a 1
        b 2
    )");

    std::cout << y() << '\n'; // 3
    // Viola!
}
```

<!-- Godbolt link: -->

  [1]: https://gcc.godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:c%2B%2B,selection:(endColumn:5,endLineNumber:9,positionColumn:5,positionLineNumber:9,selectionStartColumn:5,selectionStartLineNumber:9,startColumn:5,startLineNumber:9),source:'%23include+%3Chttps://raw.githubusercontent.com/HolyBlackCat/macro_sequence_for/master/include/macro_sequence_for.h%3E%0A%23include+%3Chttps://raw.githubusercontent.com/HolyBlackCat/rcoro/master/include/rcoro.hpp%3E%0A%0A%23include+%3Ccstdio%3E%0A%23include+%3Ciostream%3E%0A%0Aint+main()%0A%7B%0A++++auto+fib+%3D+RCORO(%7B%0A++++++++RC_VAR(a,+0)%3B+//+int+a+%3D+0%3B%0A++++++++RC_VAR(b,+1)%3B+//+int+b+%3D+1%3B%0A%0A++++++++RC_YIELD(a)%3B+//+Return+%60a%60+and+pause.%0A%0A++++++++while+(true)%0A++++++++%7B%0A++++++++++++RC_YIELD(b)%3B+//+Return+%60b%60+and+pause.%0A%0A++++++++++++int+tmp+%3D+a%3B%0A++++++++++++a+%3D+b%3B%0A++++++++++++b+%2B%3D+tmp%3B%0A++++++++%7D%0A%0A++++++++return+-1%3B+//+Unreachable,+but+some+compilers+warn+otherwise.%0A++++%7D)%3B%0A%0A++++for+(int+i+%3D+0%3B+i+%3C+5%3B+i%2B%2B)%0A++++++++std::printf(%22%25d%5Cn%22,+fib())%3B+//+0+1+1+2+3%0A%7D%0A'),l:'5',n:'0',o:'C%2B%2B+source+%231',t:'0')),k:49.22570909891783,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((g:!((h:compiler,i:(compiler:clang1701,filters:(b:'0',binary:'1',binaryObject:'1',commentOnly:'0',debugCalls:'1',demangle:'0',directives:'0',execute:'0',intel:'0',libraryCode:'1',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:2,lang:c%2B%2B,libs:!(),options:'-std%3Dc%2B%2B23+-pedantic-errors+-Wall+-Wextra+-Wdeprecated+-O2+-DNDEBUG',overrides:!(),paneName:Clang,selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1),l:'5',n:'0',o:Clang,t:'0')),header:(),l:'4',m:46.40234948604993,n:'0',o:'',s:0,t:'0'),(g:!((h:output,i:(compilerName:'x86-64+clang+15.0.0',editorid:1,fontScale:13,fontUsePx:'0',j:2,paneName:'Clang+output',wrap:'0'),l:'5',n:'0',o:'Clang+output',t:'0')),header:(),l:'4',m:53.59765051395007,n:'0',o:'',s:0,t:'0')),k:50.77429090108217,l:'3',n:'0',o:'',t:'0')),l:'2',n:'0',o:'',t:'0')),version:4
