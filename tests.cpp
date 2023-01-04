// #include <iostream>

#include <rcoro.hpp>

#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <map>
#include <typeindex>
#include <typeinfo>

#define FAIL(...) FAIL_AT(__FILE__, __LINE__, __VA_ARGS__)

#define FAIL_AT(file, line, ...) \
    do \
    { \
        ::std::cout << file << ":" << line << ": " << __VA_ARGS__ << "\n"; \
        ::std::exit(1); \
    } \
    while (false)

// Test boolean condition.
#define ASSERT(...) \
    do \
    { \
        if (!bool(__VA_ARGS__)) \
            FAIL("Assertion failed: " #__VA_ARGS__); \
    } \
    while (false)

namespace test_detail
{
    template <typename F>
    void expect_throw(const char* file, int line, std::string_view expr, std::string_view pattern, F &&func)
    {
        try
        {
            std::forward<F>(func)();
        }
        catch (std::exception &e)
        {
            std::string_view error = e.what();
            if (std::search(error.begin(), error.end(), pattern.begin(), pattern.end(), [](unsigned char a, unsigned char b){return std::tolower(a) == std::tolower(b);}) == error.end())
                FAIL_AT(file, line, "Expected exception mentioning `" << pattern << "`, but got `" << error << "` from expression `" << expr << "`.");
            return;
        }

        FAIL_AT(file, line, ": Expression didn't throw: " << expr);
    }
}

#define THROWS(pattern, ...) ::test_detail::expect_throw(__FILE__, __LINE__, #__VA_ARGS__, pattern, [&]{(void)(__VA_ARGS__);})

template <typename T>
class A;

namespace test_detail
{
    std::string *a_log = nullptr;

    struct Entry
    {
        std::type_index type;
        std::string type_name;
        bool moved_from = false;
    };

    std::map<void *, Entry> a_instances;

    template <typename T>
    void add_instance(void *target, bool moved_from)
    {
        if (a_instances.contains(target))
        {
            std::cout << *a_log;
            FAIL("Instance already exists at " << std::uintptr_t(target) << ".");
        }
        if (std::uintptr_t(target) % alignof(T) != 0)
        {
            std::cout << *a_log;
            FAIL("Attempt to create an instance at a misaligned address " << std::uintptr_t(target) << ". Must be aligned to " << alignof(T) << ".");
        }
        a_instances.try_emplace(target, Entry{.type = typeid(T), .type_name = std::string(rcoro::detail::type_name<T>()), .moved_from = moved_from});
    }

    // Returns true if the instance is moved-from.
    template <typename T>
    bool check_instance(void *target, bool allow_moved_from)
    {
        auto it = a_instances.find(target);
        if (it == a_instances.end())
        {
            std::cout << *a_log;
            FAIL("No instance at " << std::uintptr_t(target));
        }
        if (it->second.type != typeid(T))
        {
            std::cout << *a_log;
            FAIL("Instance at " << std::uintptr_t(target) << " has the wrong type. Expected `" << rcoro::detail::type_name<T>() << "` but got `" << it->second.type_name << "`.");
        }
        if (!allow_moved_from && it->second.moved_from)
        {
            std::cout << *a_log;
            FAIL("Instance at " << std::uintptr_t(target) << " is in moved-from state.");
        }
        return it->second.moved_from;
    }
}

template <typename T>
class A
{
    T value{};

  public:
    A(T new_value)
    {
        *test_detail::a_log += std::string(rcoro::detail::type_name<A<T>>()) + "::A(" + std::to_string(new_value) + ")";
        *test_detail::a_log += "   @ " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, false);
        value = new_value;
    }
    A(const A &other)
    {
        *test_detail::a_log += std::string(rcoro::detail::type_name<A<T>>()) + "::A(const A & = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   @ " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, test_detail::check_instance<T>(&other, true));
        value = other.value;
    }
    A(A &&other) noexcept
    {
        *test_detail::a_log += std::string(rcoro::detail::type_name<A<T>>()) + "::A(A && = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   @ " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, test_detail::check_instance<T>(&other, true));
        test_detail::a_instances.at(&other).moved_from = true;
        value = std::move(other.value);
    }
    A &operator=(const A &other)
    {
        *test_detail::a_log += std::string(rcoro::detail::type_name<A<T>>()) + "::operator=(const A & = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   @ " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::check_instance<T>(&other, true);
        test_detail::a_instances.at(this).moved_from = test_detail::a_instances.at(&other).moved_from;
        value = other.value;
        return *this;
    }
    A &operator=(A &&other) noexcept
    {
        *test_detail::a_log += std::string(rcoro::detail::type_name<A<T>>()) + "::operator=(A && = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   @ " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::check_instance<T>(&other, true);
        test_detail::a_instances.at(this).moved_from = test_detail::a_instances.at(&other).moved_from;
        test_detail::a_instances.at(&other).moved_from = true;
        value = std::move(other.value);
        return *this;
    }
    ~A()
    {
        *test_detail::a_log += std::string(rcoro::detail::type_name<A<T>>()) + "::~A(" + std::to_string(value) + ")";
        *test_detail::a_log += "   @ " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::a_instances.erase(this);
    }

    operator T() noexcept
    {
        test_detail::check_instance<T>(this, false);
        return value;
    }
};

class Expect
{
    std::string log, expected_log;

  public:
    Expect(std::string expected_log) : expected_log(expected_log)
    {
        test_detail::a_log = &log;
    }
    Expect(const Expect &) = delete;
    Expect &operator=(const Expect &) = delete;
    ~Expect()
    {
        if (log != expected_log)
            FAIL("---- Got log:\n" << log << "---- but expected log:\n" << expected_log << "----");
        test_detail::a_log = nullptr;
    }
};

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
            THROWS("out of range", rcoro::var_name<tag>(0));
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
            THROWS("out of range", rcoro::var_name<tag>(0));
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
            static_assert(rcoro::var_name<tag>(0) == "a");
            THROWS("out of range", rcoro::var_name<tag>(1));
            static_assert(rcoro::num_yields<tag> == 1);
            static_assert(rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
        }

        { // Single variable visible from a single yield point.
            auto x = RCORO(RC_VAR(a, 42); (void)a; RC_YIELD(););
            using tag = decltype(x)::tag;
            static_assert(rcoro::frame_size<tag> == sizeof(int));
            static_assert(rcoro::frame_alignment<tag> == alignof(int));
            static_assert(rcoro::num_vars<tag> == 1);
            static_assert(rcoro::num_yields<tag> == 2);
            static_assert(rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
            static_assert(rcoro::yield_vars<tag, 1> == std::array<int, 1>{0});
        }
    }

    { // A simple coroutine.
        static auto x = RCORO({
            {
                RC_VAR(a, A(10));
                (void)a;
            }
            {
                RC_VAR(b, A(20));
                (void)b;
                RC_YIELD("f");
            }

            RC_FOR((c, A(char{})); c < 5; c = c+1)
            {
                RC_WITH_VAR(d,A(short(30)))
                {
                    (void)d;
                    RC_YIELD("g");
                }

                RC_VAR(e, A(40));
                (void)e;
                RC_YIELD("h");
            }

            RC_YIELD("i");
        });
        using tag = decltype(x)::tag;
        static_assert(rcoro::frame_size<tag> == sizeof(int) * 2);
        static_assert(rcoro::frame_alignment<tag> == alignof(int));
        static_assert(rcoro::num_vars<tag> == 5);
        static_assert(rcoro::var_offset<tag, 0> == 0);
        static_assert(rcoro::var_offset<tag, 1> == 0);
        static_assert(rcoro::var_offset<tag, 2> == 0);
        static_assert(rcoro::var_offset<tag, 3> == sizeof(short));
        static_assert(rcoro::var_offset<tag, 4> == sizeof(int));
        static_assert( rcoro::var_lifetime_overlaps_var<tag, 0, 0> && !rcoro::var_lifetime_overlaps_var<tag, 1, 0> && !rcoro::var_lifetime_overlaps_var<tag, 2, 0> && !rcoro::var_lifetime_overlaps_var<tag, 3, 0> && !rcoro::var_lifetime_overlaps_var<tag, 4, 0>);
        static_assert(!rcoro::var_lifetime_overlaps_var<tag, 0, 1> &&  rcoro::var_lifetime_overlaps_var<tag, 1, 1> && !rcoro::var_lifetime_overlaps_var<tag, 2, 1> && !rcoro::var_lifetime_overlaps_var<tag, 3, 1> && !rcoro::var_lifetime_overlaps_var<tag, 4, 1>);
        static_assert(!rcoro::var_lifetime_overlaps_var<tag, 0, 2> && !rcoro::var_lifetime_overlaps_var<tag, 1, 2> &&  rcoro::var_lifetime_overlaps_var<tag, 2, 2> &&  rcoro::var_lifetime_overlaps_var<tag, 3, 2> &&  rcoro::var_lifetime_overlaps_var<tag, 4, 2>);
        static_assert(!rcoro::var_lifetime_overlaps_var<tag, 0, 3> && !rcoro::var_lifetime_overlaps_var<tag, 1, 3> &&  rcoro::var_lifetime_overlaps_var<tag, 2, 3> &&  rcoro::var_lifetime_overlaps_var<tag, 3, 3> && !rcoro::var_lifetime_overlaps_var<tag, 4, 3>);
        static_assert(!rcoro::var_lifetime_overlaps_var<tag, 0, 4> && !rcoro::var_lifetime_overlaps_var<tag, 1, 4> &&  rcoro::var_lifetime_overlaps_var<tag, 2, 4> && !rcoro::var_lifetime_overlaps_var<tag, 3, 4> &&  rcoro::var_lifetime_overlaps_var<tag, 4, 4>);
        static_assert(!rcoro::var_lifetime_overlaps_yield<tag, 0, 0> && !rcoro::var_lifetime_overlaps_yield<tag, 1, 0> && !rcoro::var_lifetime_overlaps_yield<tag, 2, 0> && !rcoro::var_lifetime_overlaps_yield<tag, 3, 0> && !rcoro::var_lifetime_overlaps_yield<tag, 4, 0> && rcoro::yield_vars<tag, 0> == std::array<int, 0>{});
        static_assert(!rcoro::var_lifetime_overlaps_yield<tag, 0, 1> &&  rcoro::var_lifetime_overlaps_yield<tag, 1, 1> && !rcoro::var_lifetime_overlaps_yield<tag, 2, 1> && !rcoro::var_lifetime_overlaps_yield<tag, 3, 1> && !rcoro::var_lifetime_overlaps_yield<tag, 4, 1> && rcoro::yield_vars<tag, 1> == std::array{1});
        static_assert(!rcoro::var_lifetime_overlaps_yield<tag, 0, 2> && !rcoro::var_lifetime_overlaps_yield<tag, 1, 2> &&  rcoro::var_lifetime_overlaps_yield<tag, 2, 2> &&  rcoro::var_lifetime_overlaps_yield<tag, 3, 2> && !rcoro::var_lifetime_overlaps_yield<tag, 4, 2> && rcoro::yield_vars<tag, 2> == std::array{2,3});
        static_assert(!rcoro::var_lifetime_overlaps_yield<tag, 0, 3> && !rcoro::var_lifetime_overlaps_yield<tag, 1, 3> &&  rcoro::var_lifetime_overlaps_yield<tag, 2, 3> && !rcoro::var_lifetime_overlaps_yield<tag, 3, 3> &&  rcoro::var_lifetime_overlaps_yield<tag, 4, 3> && rcoro::yield_vars<tag, 3> == std::array{2,4});

        Expect ex("");

        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::not_finished && x.yield_point() == 0 && x.yield_point_name() == ""  && !x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && x());
        *test_detail::a_log += "---\n";
        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::not_finished && x.yield_point() == 1 && x.yield_point_name() == "f" && !x.var_exists<"a">() &&  x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && x());
        *test_detail::a_log += "---\n";
        for (int i = 0; i < 5; i++)
        {
            ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::not_finished && x.yield_point() == 2 && x.yield_point_name() == "g" && !x.var_exists<"a">() && !x.var_exists<"b">() &&  x.var_exists<"c">() &&  x.var_exists<"d">() && !x.var_exists<"e">() && x());
            *test_detail::a_log += "---\n";
            ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::not_finished && x.yield_point() == 3 && x.yield_point_name() == "h" && !x.var_exists<"a">() && !x.var_exists<"b">() &&  x.var_exists<"c">() && !x.var_exists<"d">() &&  x.var_exists<"e">() && x());
            *test_detail::a_log += "---\n";
        }
        ASSERT(!x && x.finished() && !x.busy() && x.finish_reason() == rcoro::success);
        *test_detail::a_log += "---\n";
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
