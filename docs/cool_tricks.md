This is a list of cool metaprogramming tricks used in this library.

  * [The very basic idea](#the-very-basic-idea)
  * [Unbounded preprocessor loops without boilerplate](#unbounded-preprocessor-loops-without-boilerplate)
  * [`goto` jumps over variables](#goto-jumps-over-variables)
  * [Friend injection](#friend-injection)
  * [Tracking variable lifetime at compile-time](#tracking-variable-lifetime-at-compile-time)
  * [Instantiating coroutine body twice](#instantiating-coroutine-body-twice)
  * [Handling types defined locally](#handling-types-defined-locally)

<!-- To regenerate the table of contents, run:
grep -E '^##+ ' docs/cool_tricks.md | sed -E -e 's/^## /* /g' -e 's/^### /  * /g' -e 's/^#### /    * /g' | gawk '{$0 = gensub(/( *\* )(.*)/,"\\1[\\2]","g") "(#" gensub(/[^-_a-z0-9]/,"","g",gensub(/ /,"-","g",tolower(gensub(/ *\* /,"",1,$0)))) ")"; print}'
-->


### The very basic idea

As you probably already aware, the basic idea behind this and other similar libraries is:

`RC_YIELD` expands to a `return;`, then a `goto` label. When resuming a coroutine, we `goto` to the last point we stopped at (by saving its index before returning, etc).

The interesting stuff comes next.


### Unbounded preprocessor loops without boilerplate

`RCORO` needs to loop several times over the coroutine body (e.g. to [track variable lifetime](#tracking-variable-lifetime-at-compile-time)). Normally this requires a bunch of boilerplate macros, limiting the max number of iterations (e.g. Boost.Preprocessor does that).

The solution is abstracted away into a library, see [`macro_sequence_for`](https://github.com/HolyBlackCat/macro_sequence_for) for code and explanation.


### `goto` jumps over variables

We use `goto` to jump to `RC_YIELD()` when resuming a coroutine. `goto` can't normally jump over variable initialization (this is a compilation error).

This is the reason why other similar libraries only allow you to declare variables in a separate struct at the beginning of a coroutine. If a variable is declared anywhere between the beginning and `RC_YIELD`, you can't `goto` to said `RC_YIELD` from the beginning of the function.

We solve this by jumping to each variable in order. Let's say there are two `RC_VAR`s before an `RC_YIELD` we're paused at. To resume from this point, we jump right before the first variable, initialize it, then immediately jump right before the second one, initialize it, then jump to the desired `RC_YIELD`.

For example:

```cpp
auto x = RCORO({
    std::cout << "...\n";    // (1)
    RC_VAR(a, 1);            // (2)
    std::cout << "Hello,\n"; // (3)
    RC_VAR(b, 2);            // (4)
    std::cout << "world!\n"; // (5)
    RC_YIELD();              // (6)
});
```

To resume from (6), we first jump directly to (2), create the variable, then jump to (4), create that varaible, then jump to (6) and continue the normal execution.

Before you say this is dumb and wasteful, consider this: all our `RC_VAR`s are simple references into the byte array we use for storage. The offsets into the array are [determined at compile-time](#instantiating-coroutine-body-twice).

The references shouldn't even exist at runtime; the compiler should access the byte array directly at the hardcoded offset, so the chained jumps should be optimized away entirely, and we should be jumping directly to the right address.


### Friend injection

This is probably considered vanilla at this point.

The problem: you have a variable (`RC_VAR`) deep inside of a coroutine. How do you get its type?

Sure, there is `decltype(...)`, but how do you pull the type out from the braces enclosing the coroutine body?

```cpp
auto lambda = []{
    auto var = 42;
    using type = decltype(var); // int
};

// How do I get `type` here?
```

Answer: friend injection!

```cpp
template <typename T>
struct Tag {using type = T;};

template <typename Key>
struct Reader
{
    friend auto foo(Reader<Key>);
};

template <typename Key, typename Value>
struct Writer
{
    friend auto foo(Reader<Key>) {return Tag<Value>{};}
};


struct MyKey {};
struct MyKey2 {};

auto lambda = []{
    auto var = 42;
    using type = decltype(var); // int
    (void)Writer<MyKey, type> {}; // Instantiate `Writer`.

    auto var2 = 42.f;
    using type2 = decltype(var); // float
    (void)Writer<MyKey2, type2> {}; // Instantiate `Writer`.
};

using type = typename decltype(foo(Reader<MyKey>{}))::type; // int
using type = typename decltype(foo(Reader<MyKey2>{}))::type; // float
```
Instantiating `Writer` generates `foo()` in the enclosing (global) namespace. That's what `friend` function *definitions* (as opposed to declarations) do.

The resulting function can't be called directly, only via [ADL](https://en.cppreference.com/w/cpp/language/adl).

That's what `Reader` is for. Since it's in the same (global) namespace as the function, *and* is a parameter of the function, *and* adds it as a friend, `foo(Reader<MyKey>{})` is able to find the newly defined function.

`typename Key` can be anything. This becomes a compile-time map, with types as keys.

We use this to track not only variable types, but also [their lifetimes](#tracking-variable-lifetime-at-compile-time).


### Tracking variable lifetime at compile-time

We need to know two things:

* Which variables can coexist. If two of them don't, we can reuse the storage between them.
* Which variables are visible at each `RC_YIELD`. Only those variables need to be processed: serialized, or destroyed when the coroutine dies, or copied when it's copied, etc.

And we want to know all that at compile-time.

Solution: next to each `RC_VAR(MyVar, init);`, generate `constexpr bool MyVar_visible = true;`. And the very beginning of the lambda, add `constexpr bool MyVar_visible = false;`.

For example, given
```cpp
auto x = RCORO({
    // A
    RC_VAR(a, 1);
    // B
    RC_VAR(b, 2);
    // C
    RC_YIELD();
});
```
we produce something like
```cpp
auto x = []{
    constexpr bool a_visible = false;
    constexpr bool b_visible = false;
    {
        // A
        constexpr bool a_visible = true;
        int a = 1;
        // B
        constexpr bool b_visible = true;
        int b = 2;
        // C
        RC_YIELD();
    }
};
```
Now, at A, `a_visible == false && b_visible == false`.

At B, `a_visible == true && b_visible == false`.

At C, `a_visible == true && b_visible == true`.

For each variable and yield point, we generate a list of markers for preceding variables, e.g. `std::array<bool, 2>{a_visible, b_visible}` at the final yield.

Next we plop this array into a [friend function](#friend-injection) to extract it from the lambda.


### Instantiating coroutine body twice

We instantiate the coroutine body twice, by putting it into a template lambda:

```cpp
struct A {};
struct B {};

auto x = [](auto param)
{
    // ...
};

if (false)
    x(A{}); // First instantiation. Never actually called at runtime.

x(B{}); // Second instantiation.
```

The first instantiation is the dummy one: the variables are replaced with placeholders, and so on. It collects the variable types and [lifetime information](#tracking-variable-lifetime-at-compile-time) using [friend injection](#friend-injection). This version is never called at runtime.

Then we use this knowledge to calculate the total required storage size (decide which variables don't coexist and thus can share storage, decide on the individual variable offsets in the resulting byte array). Since we know the storage size at compile-time, we can avoid heap allocation.

Then we instantiate it again. This time the variable offsets get baked into the code. This copy is what gets called at runtime.

### Handling types defined locally

If a new type is created in the coroutine (a lambda, another coroutine, or a new `struct`), the type ends up being different in [the two instantiations](#instantiating-coroutine-body-twice).

Even though it's for most purposes the same type, the two types are actually incompatible, can't be initialized with one another, and so on.

This causes issues, because the variable types are collected during the first instantiation, and are used in the second one. This initially caused `RC_VAR`s with those types to not work at all.

The solution: collect the types twice, once per instantiation.

In the first set of types we only care about `sizeof` and `alignof` to calculate the required storage, and few other insignificant properties.

The second set of types is what's actually used in most places, reported through reflection, etc. There are `static_assert`s to make sure the types have the same size and alignment, if the user somehow manages to change them.
