// #include <iostream>

#include <rcoro.hpp>

#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>

// Test boolean condition.
#define ASSERT(...) \
    do \
    { \
        if (!bool(__VA_ARGS__)) \
        { \
            ::std::cout << "Assertion failed: " #__VA_ARGS__ "\n"; \
            ::std::exit(1); \
        } \
    } \
    while (false)

namespace test_detail
{
    template <typename F>
    void expect_throw(std::string_view expr, std::string_view pattern, F &&func)
    {
        try
        {
            std::forward<F>(func)();
        }
        catch (std::exception &e)
        {
            std::string_view error = e.what();
            if (std::search(error.begin(), error.end(), pattern.begin(), pattern.end(), [](unsigned char a, unsigned char b){return std::tolower(a) == std::tolower(b);}) == error.end())
            {
                std::cout << "Expected exception mentioning `" << pattern << "`, but got `" << error << "` from expression `" << expr << "`.\n";
            }
            return;
        }

        std::cout << "Expression didn't throw: " << expr << '\n';
    }
}

#define THROWS(pattern, ...) ::test_detail::expect_throw(#__VA_ARGS__, pattern, [&]{(void)(__VA_ARGS__);})

int main()
{
    // * Silence GCC warnings.
    // * Deserialization from string.
    // * Tests (lifetime, exception recovery, rule-of-five)
    // * Passing parameters.
    // * Serialization-deserialization tests.
    // * Strip `constexpr` that will never happen?
    // * Constexpr tests. (mostly `constinit`-ness?)
    // * Noexcept tests for copy/move operations.
    // * CI
    // * Test that we include all necessary headers.
    // * Add dummy variables `rcoro` and `std` to check that we prefix everything with `::`.
    // * Test GCC and MSVC.
    // * Optimized assignments between the same yield points.

    { // Basic (mostly) static checks.
        { // Empty coroutine.
            auto x = RCORO();
            using tag = decltype(x)::tag;
            static_assert(rcoro::frame_size<tag> == 0);
            static_assert(rcoro::frame_alignment<tag> == 1);
            static_assert(rcoro::num_vars<tag> == 0);
            THROWS("unknown", rcoro::var_index<tag>("?"));
            static_assert(rcoro::var_index_or_negative<tag>("?") == rcoro::unknown_name);
            static_assert(rcoro::num_yields<tag> == 1);
            static_assert(rcoro::yield_name<tag>(0) == "");
            static_assert(rcoro::yield_name_const<tag, 0>.view() == "");
            static_assert(rcoro::yield_index<tag>("") == 0);
            THROWS("unknown", rcoro::yield_index<tag>("?"));
            static_assert(rcoro::yield_index_or_negative<tag>("") == 0);
            static_assert(rcoro::yield_index_or_negative<tag>("?") == rcoro::unknown_name);
            static_assert(rcoro::yield_index_const<tag, ""> == 0);
            static_assert(rcoro::yields_uniquely_named<tag>);
            static_assert(rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
        }

        { // Single yield point.
            auto x = RCORO(RC_YIELD("y"););
            using tag = decltype(x)::tag;
            static_assert(rcoro::frame_size<tag> == 0);
            static_assert(rcoro::frame_alignment<tag> == 1);
            static_assert(rcoro::num_vars<tag> == 0);
            THROWS("unknown", rcoro::var_index<tag>("?"));
            static_assert(rcoro::var_index_or_negative<tag>("?") == rcoro::unknown_name);
            static_assert(rcoro::num_yields<tag> == 2);
            static_assert(rcoro::yield_name<tag>(0) == "");
            static_assert(rcoro::yield_name<tag>(1) == "y");
            static_assert(rcoro::yield_name_const<tag, 0>.view() == "");
            static_assert(rcoro::yield_name_const<tag, 1>.view() == "y");
            static_assert(rcoro::yield_index<tag>("") == 0);
            static_assert(rcoro::yield_index<tag>("y") == 1);
            THROWS("unknown", rcoro::yield_index<tag>("?"));
            static_assert(rcoro::yield_index_or_negative<tag>("") == 0);
            static_assert(rcoro::yield_index_or_negative<tag>("y") == 1);
            static_assert(rcoro::yield_index_or_negative<tag>("?") == rcoro::unknown_name);
            static_assert(rcoro::yield_index_const<tag, ""> == 0);
            static_assert(rcoro::yield_index_const<tag, "y"> == 1);
            static_assert(rcoro::yields_uniquely_named<tag>);
            static_assert(rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
            static_assert(rcoro::yield_vars<tag, 1> == std::array<int, 0>{});

            { // Single unnamed yield point.
                auto x = RCORO(RC_YIELD(););
                using tag = decltype(x)::tag;
                static_assert(rcoro::num_yields<tag> == 2);
                static_assert(rcoro::yield_name<tag>(0) == "");
                static_assert(rcoro::yield_name<tag>(1) == "");
                static_assert(rcoro::yield_name_const<tag, 0>.view() == "");
                static_assert(rcoro::yield_name_const<tag, 1>.view() == "");
                THROWS("ambiguous", rcoro::yield_index<tag>(""));
                THROWS("unknown", rcoro::yield_index<tag>("?"));
                static_assert(rcoro::yield_index_or_negative<tag>("") == rcoro::ambiguous_name);
                static_assert(rcoro::yield_index_or_negative<tag>("?") == rcoro::unknown_name);
                static_assert(!rcoro::yields_uniquely_named<tag>);
                static_assert(rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
                static_assert(rcoro::yield_vars<tag, 1> == std::array<int, 0>{});
            }
        }

        { // Single variable.
            auto x = RCORO(RC_VAR(a, 42); (void)a;);
            using tag = decltype(x)::tag;
            static_assert(rcoro::num_vars<tag> == 1);
            static_assert(rcoro::var_index<tag>("a") == 0);
            THROWS("unknown", rcoro::var_index<tag>("?"));
            static_assert(rcoro::var_index_or_negative<tag>("a") == 0);
            static_assert(rcoro::var_index_or_negative<tag>("?") == rcoro::unknown_name);
            static_assert(rcoro::num_yields<tag> == 1);
            static_assert(rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
        }

        { // Single variable visible from a single yield point.
            auto x = RCORO(RC_VAR(a, 42); (void)a; RC_YIELD(););
            using tag = decltype(x)::tag;
            static_assert(rcoro::num_vars<tag> == 1);
            static_assert(rcoro::num_yields<tag> == 2);
            static_assert(rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
            static_assert(rcoro::yield_vars<tag, 1> == std::array<int, 1>{0});
        }
    }

#if 0
    // int rcoro;
    // int detail;
    // int std;
    auto x = RCORO(
        ::std::cout << 1 << '\n';
        RC_YIELD("2");
        ::std::cout << 2 << '\n';
        RC_YIELD("1");
        ::std::cout << 3 << '\n';
        {
            RC_VAR(unreachable, 0);
            (void)unreachable;
        }
        RC_FOR((i,0); i < 5; i++)
        {
            RC_VAR(j, 'a');
            RC_YIELD("3");
            ::std::cout << i * 10 << '\n';
            RC_VAR(k, short{});
            RC_VAR(l, int{});
            (void)j;
            (void)k;
            (void)l;
        }
    );

    // using tag = decltype(x)::tag;

    // bool finished = false;
    // std::cin >> finished;
    // std::string yield;
    // if (!finished)
    // {
    //     std::getline(std::cin, yield); // Flush.
    //     std::getline(std::cin, yield);
    // }
    // bool ok = x.load(rcoro::finish_reason(finished), rcoro::yield_index<tag>(yield), [](auto index, auto construct)
    // {
    //     std::cout << rcoro::var_name_const<tag, index.value>.view() << ": ";
    //     rcoro::var_type<tag, index.value> var;
    //     std::cin >> var;
    //     if (var)
    //         construct(var);
    // });
    // std::cout << "ok=" << ok << '\n';

    // std::cout << rcoro::yield_points_uniquely_named<tag> << '\n';


    // ::std::cout << "{{\n";
    // ::std::cout << x;
    // ::std::cout << "}}\n";
    // x.for_each_alive_var([&](auto index)
    // {
    //     std::cout << rcoro::var_name_const<tag, index.value>.view() << ' ';
    // });
    // std::cout << '\n';

    if (x)
    {
        ::std::cout << "-\n";
        while (x.resume())
        {
            // if (x.var_exists<"i">() && x.var<"i">() == 2)
            //     x.var<"i">() = 3;
            std::cout << "---\n";
            // ::std::cout << "{{\n";
            // ::std::cout << x;
            // ::std::cout << "}}\n";
            // x.for_each_alive_var([&](auto index)
            // {
            //     std::cout << rcoro::var_name_const<tag, index.value>.view() << ' ';
            // });
            // std::cout << '\n';
        }
    }

    // ::std::cout << "{{\n";
    // ::std::cout << x;
    // ::std::cout << "}}\n";
    // x.for_each_alive_var([&](auto index)
    // {
    //     std::cout << rcoro::var_name_const<tag, index.value>.view() << ' ';
    // });
    // std::cout << '\n';

    // std::cout << rcoro::debug_info<decltype(x)::tag> << '\n';
#endif
    std::cout << "OK\n";
}
