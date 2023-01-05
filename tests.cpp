// #include <iostream>

#include <rcoro.hpp>

#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>
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

    std::string string_replace(std::string_view source, std::string_view a, std::string_view b)
    {
        std::string ret;

        std::size_t cur_pos;
        std::size_t last_pos = 0;

        while ((cur_pos = source.find(a, last_pos)) != std::string::npos)
        {
            ret.append(source, last_pos, cur_pos - last_pos);
            ret += b;
            last_pos = cur_pos + a.size();
        }

        ret += source.substr(last_pos);
        return ret;
    }

    template <typename T>
    std::string get_type_name()
    {
        std::string ret(rcoro::detail::type_name<T>());
        ret = string_replace(ret, "short int", "short"); // GCC spells it as `short int`, breaking my test cases.
        ret = string_replace(ret, "long int", "long"); // ^
        return ret;
    }

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
        a_instances.try_emplace(target, Entry{.type = typeid(T), .type_name = std::string(get_type_name<T>()), .moved_from = moved_from});
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
            FAIL("Instance at " << std::uintptr_t(target) << " has the wrong type. Expected `" << get_type_name<T>() << "` but got `" << it->second.type_name << "`.");
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
        *test_detail::a_log += std::string(test_detail::get_type_name<A<T>>()) + "::A(" + std::to_string(new_value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, false);
        value = new_value;
    }
    A(const A &other)
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A<T>>()) + "::A(const A & = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, test_detail::check_instance<T>(&other, true));
        value = other.value;
    }
    A(A &&other) noexcept
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A<T>>()) + "::A(A && = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, test_detail::check_instance<T>(&other, true));
        test_detail::a_instances.at(&other).moved_from = true;
        value = std::move(other.value);
    }
    A &operator=(const A &other)
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A<T>>()) + "::operator=(const A & = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::check_instance<T>(&other, true);
        test_detail::a_instances.at(this).moved_from = test_detail::a_instances.at(&other).moved_from;
        value = other.value;
        return *this;
    }
    A &operator=(A &&other) noexcept
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A<T>>()) + "::operator=(A && = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::check_instance<T>(&other, true);
        test_detail::a_instances.at(this).moved_from = test_detail::a_instances.at(&other).moved_from;
        test_detail::a_instances.at(&other).moved_from = true;
        value = std::move(other.value);
        return *this;
    }
    ~A()
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A<T>>()) + "::~A(" + std::to_string(value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(this)) + "\n";
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
    std::string log, expected_log, expected_log_raw;

  public:
    Expect(std::string new_log)
    {
        std::istringstream ss(std::move(new_log));
        std::string line;
        while (std::getline(ss, line))
        {
            auto start = line.find_first_not_of(' ');
            if (start == std::string::npos)
                continue;
            std::string_view line_view = std::string_view(line).substr(start);
            expected_log_raw += line_view;
            expected_log_raw += '\n';
            line_view = line_view.substr(0, line_view.find_last_of('#'));
            auto end = line_view.find_last_not_of(' ');
            if (end == std::string::npos)
                continue;
            line_view = line_view.substr(0, end + 1);
            expected_log += line_view;
            expected_log += '\n';
        }

        test_detail::a_log = &log;
    }
    Expect(const Expect &) = delete;
    Expect &operator=(const Expect &) = delete;
    ~Expect()
    {
        test_detail::a_log = nullptr;

        // Strip comments from `log`.
        std::string stripped_log;
        std::istringstream ss(log);
        std::string line;
        while (std::getline(ss, line))
        {
            auto end = line.find_last_of('#');
            if (end == std::string::npos)
            {
                stripped_log += line;
                stripped_log += '\n';
            }
            else
            {
                std::string_view line_view = std::string_view(line).substr(0, end);
                end = line_view.find_last_not_of(' ');
                if (end != std::string::npos)
                {
                    stripped_log += line_view.substr(0, end + 1);
                    stripped_log += '\n';
                }
            }
        }

        // Compare line-wise.
        std::istringstream ss_log(stripped_log);
        std::istringstream ss_expected(expected_log);
        int row = 1;
        bool fail = true;
        while (true)
        {
            std::string line_log, line_expected;
            bool log_ok(std::getline(ss_log, line_log));
            bool expected_ok(std::getline(ss_expected, line_expected));
            if (log_ok > expected_ok)
            {
                std::cout << "Expected less log. Line " << row << " wasn't expected: " << line_log << '\n';
                break;
            }
            if (log_ok < expected_ok)
            {
                std::cout << "Expected more log. Missing line " << row << ": " << line_expected << '\n';
                break;
            }
            if (!log_ok && !expected_ok)
            {
                fail = false;
                break;
            }
            if (line_log != line_expected)
            {
                std::cout << "Mismatch at line " << row << ".\nExpected: " << line_expected << "\nBut got: " << line_log << "\n";
                break;
            }

            row++;
        }

        if (fail)
            FAIL("---- Got log:\n" << log << "---- but expected log:\n" << expected_log_raw << "----");

        // Check for leaks.
        if (!test_detail::a_instances.empty())
            FAIL("Found leaks!");
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

            RC_FOR((c, A(char{})); c < 3; c = c+1)
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

        Expect ex(R"(
            A<int>::A(10)                # `a` created and destroyed immediately.
            A<int>::~A(10)               # ^
            A<int>::A(20)                # `b` created.
            ...                          # Pause at `f`.
            A<int>::~A(20)               # `b` destroyed.
            A<char>::A(0)                # Loop counter `c` created.
            A<short>::A(30)              # {{{ `d` created.
            ...                          # Pause at `g`.
            A<short>::~A(30)             # `d` destroyed.
            A<int>::A(40)                # `e` created.
            ...                          # Pause at `h`.
            A<int>::~A(40)               # `d` destroyed. }}}
            A<char>::A(1)                # Increment `c`: 0 -> 1.
            A<char>::operator=(A && = 1) # ^
            A<char>::~A(1)               # ^
            A<short>::A(30)              # {{{
            ...
            A<short>::~A(30)
            A<int>::A(40)
            ...
            A<int>::~A(40)               # }}}
            A<char>::A(2)
            A<char>::operator=(A && = 2)
            A<char>::~A(2)
            A<short>::A(30)              # {{{
            ...
            A<short>::~A(30)
            A<int>::A(40)
            ...
            A<int>::~A(40)               # }}}
            A<char>::A(3)                # The loop counter is incremented the last time.
            A<char>::operator=(A && = 3) # ^
            A<char>::~A(3)               # ^
            A<char>::~A(3)               # The loop counter dies.
            ...                          # Pause at `i`.
        )");

        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 0 && x.yield_point_name() == ""  && !x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && x());
        *test_detail::a_log += "...\n";
        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 1 && x.yield_point_name() == "f" && !x.var_exists<"a">() &&  x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && x());
        *test_detail::a_log += "...\n";
        for (int i = 0; i < 3; i++)
        {
            ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 2 && x.yield_point_name() == "g" && !x.var_exists<"a">() && !x.var_exists<"b">() &&  x.var_exists<"c">() &&  x.var_exists<"d">() && !x.var_exists<"e">() && x());
            *test_detail::a_log += "...\n";
            ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 3 && x.yield_point_name() == "h" && !x.var_exists<"a">() && !x.var_exists<"b">() &&  x.var_exists<"c">() && !x.var_exists<"d">() &&  x.var_exists<"e">() && x());
            *test_detail::a_log += "...\n";
        }
        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 4 && x.yield_point_name() == "i" && !x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && !x());
        ASSERT(!x && x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::success);
    }

    { // Reset and rewind.
        { // Stateless.
            auto x = RCORO({
                RC_YIELD();
                RC_YIELD("y");
            });
            ASSERT(!x.busy() && !x.finished() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 0 && x.yield_point_name() == "");
            x()();
            ASSERT(!x.busy() && !x.finished() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 2 && x.yield_point_name() == "y");
            x.reset();
            ASSERT(!x.busy() &&  x.finished() && x.finish_reason() == rcoro::finish_reason::reset        && x.yield_point() == 0 && x.yield_point_name() == "");
            x.rewind();
            ASSERT(!x.busy() && !x.finished() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 0 && x.yield_point_name() == "");
            x()()();
            ASSERT(!x.busy() &&  x.finished() && x.finish_reason() == rcoro::finish_reason::success      && x.yield_point() == 0 && x.yield_point_name() == "");
            decltype(x) y;
            ASSERT(!y.busy() &&  y.finished() && y.finish_reason() == rcoro::finish_reason::reset        && y.yield_point() == 0 && y.yield_point_name() == "");

            // Some type checks.
            static_assert(std::is_same_v<decltype(x.reset()), decltype(x) &>);
            static_assert(std::is_same_v<decltype(x.rewind()), decltype(x) &>);

            auto &&a = decltype(x){}.reset(); // Evaluated context, to force an instantiation.
            static_assert(std::is_same_v<decltype(a), decltype(x) &&>);
            auto &&b = decltype(x){}.rewind(); // Evaluated context, to force an instantiation.
            static_assert(std::is_same_v<decltype(b), decltype(x) &&>);
        }

        { // Stateful.
            auto x = RCORO({
                {
                    RC_VAR(unused, A(1)); // Skip index `0` to catch more bugs.
                    (void)unused;
                }

                RC_VAR(a, A(short(2)));
                (void)a;
                RC_VAR(b, A(long(3)));
                (void)b;
                RC_YIELD();
            });

            Expect ex(R"(
                ...
                A<int>::A(1)
                A<int>::~A(1)
                A<short>::A(2)
                A<long>::A(3)
                ...
                A<long>::~A(3)
                A<short>::~A(2)
                ...
                ...
                A<int>::A(1)
                A<int>::~A(1)
                A<short>::A(2)
                A<long>::A(3)
                ...
                A<long>::~A(3)
                A<short>::~A(2)
            )");

            *test_detail::a_log += "...\n";
            x();
            *test_detail::a_log += "...\n";
            x.reset();
            *test_detail::a_log += "...\n";
            x.rewind();
            *test_detail::a_log += "...\n";
            x();
            *test_detail::a_log += "...\n";
            x.rewind();
        }
    }

    { // User exceptions.
        { // Body throws.
            auto x = RCORO({
                {
                    RC_VAR(unused, A(1)); // Skip index `0` to catch more bugs.
                    (void)unused;
                }

                RC_VAR(a, A(short(2)));
                (void)a;
                RC_VAR(b, A(long(3)));
                (void)b;

                *test_detail::a_log += "throw!\n";
                throw 42;
            });

            Expect ex(R"(
                # Get in position.
                A<int>::A(1)
                A<int>::~A(1)
                A<short>::A(2)
                A<long>::A(3)
                throw!
                # Recover.
                A<long>::~A(3)
                A<short>::~A(2)
            )");

            try
            {
                x();
            }
            catch (int value)
            {
                ASSERT(value == 42);
            }

            ASSERT(!x.busy() && x.finished() && x.finish_reason() == rcoro::finish_reason::exception && x.yield_point() == 0 && x.yield_point_name() == "");
        }
    }

    std::cout << "OK\n";
}
