// #include <iostream>

#include <rcoro.hpp>

#include <iostream>
#include <iomanip>

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
