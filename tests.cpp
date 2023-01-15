#ifndef DETAIL_RCORO_HPP_ // Make this godbolt-friendly.
#include <rcoro.hpp>
#endif

#include <cstdlib>
#include <cstring>
#include <functional>
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

    std::map<const void *, Entry> a_instances;

    template <typename T>
    void add_instance(const void *target, bool moved_from)
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
    bool check_instance(const void *target, bool allow_moved_from)
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
    using type = T;

    template <typename U> requires std::is_same_v<T, U>
    A(U new_value)
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A>()) + "::A(" + std::to_string(new_value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, false);
        value = new_value;
    }
    A(const A &other)
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A>()) + "::A(const A & = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, test_detail::check_instance<T>(&other, true));
        value = other.value;
    }
    A(A &&other) noexcept
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A>()) + "::A(A && = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::add_instance<T>(this, test_detail::check_instance<T>(&other, true));
        test_detail::a_instances.at(&other).moved_from = true;
        value = std::move(other.value);
        other.value *= -1; // This indicates moved-from-ness.
    }
    A &operator=(const A &other)
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A>()) + "::operator=(const A & = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::check_instance<T>(&other, true);
        test_detail::a_instances.at(this).moved_from = test_detail::a_instances.at(&other).moved_from;
        value = other.value;
        return *this;
    }
    A &operator=(A &&other) noexcept
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A>()) + "::operator=(A && = " + std::to_string(other.value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(&other)) + " -> " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::check_instance<T>(&other, true);
        test_detail::a_instances.at(this).moved_from = test_detail::a_instances.at(&other).moved_from;
        test_detail::a_instances.at(&other).moved_from = true;
        value = std::move(other.value);
        other.value *= -1; // This indicates moved-from-ness.
        return *this;
    }
    ~A()
    {
        *test_detail::a_log += std::string(test_detail::get_type_name<A>()) + "::~A(" + std::to_string(value) + ")";
        *test_detail::a_log += "   # " + std::to_string(std::uintptr_t(this)) + "\n";
        test_detail::check_instance<T>(this, true);
        test_detail::a_instances.erase(this);
    }

    explicit operator T() const noexcept
    {
        test_detail::check_instance<T>(this, false);
        return value;
    }

    template <typename U> requires(rcoro::detail::Printable<T, U> && !std::is_same_v<T, char>)
    friend U &operator<<(U &s, const A &a) {return s << T(a);}
};
template <typename T> A(T) -> A<T>;

enum class ops
{
    none = 0,
    copy_ctor           = 0b00'00000001,
    nothrow_copy_ctor   = 0b00'00000011,
    move_ctor           = 0b00'00000100,
    nothrow_move_ctor   = 0b00'00001100,
    copy_assign         = 0b00'00010000,
    nothrow_copy_assign = 0b00'00110000,
    move_assign         = 0b00'01000000,
    nothrow_move_assign = 0b00'11000000,
    copy_ctor_throws    = 0b01'00000001,
    move_ctor_throws    = 0b10'00000100,
};
[[nodiscard]] constexpr ops operator&(ops a, ops b) {return ops(int(a) & int(b));}
[[nodiscard]] constexpr ops operator|(ops a, ops b) {return ops(int(a) | int(b));}
[[nodiscard]] constexpr ops operator~(ops a) {return ops(~int(a));}

// Need a helper base because of a GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=108335
template <typename T, ops O>
struct B_base : A<T>
{
    using A<T>::A;
    constexpr B_base(const B_base &other) noexcept((O & ops::nothrow_copy_ctor) == ops::nothrow_copy_ctor) requires(bool(O & ops::copy_ctor))
        : A<T>((O & ops::copy_ctor_throws) == ops::copy_ctor_throws ? throw(void(*test_detail::a_log += "throw!\n"), 42) : other) {}
    constexpr B_base(B_base &&other) noexcept((O & ops::nothrow_move_ctor) == ops::nothrow_move_ctor) requires(bool(O & ops::move_ctor))
        : A<T>((O & ops::move_ctor_throws) == ops::move_ctor_throws ? throw(void(*test_detail::a_log += "throw!\n"), 42) : std::move(other)) {}
    constexpr B_base &operator=(const B_base &other) noexcept((O & ops::nothrow_copy_assign) == ops::nothrow_copy_assign) requires(bool(O & ops::copy_assign)) {static_cast<A<T> &>(*this) = other; return *this;}
    constexpr B_base &operator=(B_base &&other) noexcept((O & ops::nothrow_move_assign) == ops::nothrow_move_assign) requires(bool(O & ops::move_assign)) {static_cast<A<T> &>(*this) = std::move(other); return *this;}
};
template <typename T, ops O>
struct B : B_base<T, O>
{
    using B_base<T, O>::B_base;
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
            std::string_view line_view = line;
            if (auto begin = line_view.find_first_not_of(' '); begin != std::string::npos)
                line_view = line_view.substr(begin);

            auto end = line_view.find_last_of('#');
            if (end == std::string::npos)
            {
                stripped_log += line_view;
                stripped_log += '\n';
            }
            else
            {
                line_view = line_view.substr(0, end);
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
    // * Any's copy ctor should SFINAE reject non-copyable coros.
    // * Test `any[_noncopyable]` rule of five.
    // * Test `any` like `any_noncopyable`
    // * Test: copy throws when: 1. constructing wrapper, 2. copying wrapper
    // * Test any[_noncopyable]: move ops are nothrow, copy ops are not

    // * CI
    // * Test that we include all necessary headers.
    // * Test MSVC.
    // * Possible improvements:
    //   * <=> == for specific_coro and all the type-erasure wrappers.
    //   * Optimized assignments between the same yield points? (use assignment instead of reconstruction)
    //   * Ignore variables that aren't visible on any yield points.
    //   * `.var` and `.var_exists` should disambiguate variable names based on yield index.
    //   * Overaligned heap allocation?

    // Make sure our macros prefix everything with `::`.
    [[maybe_unused]] int std, rcoro, detail;

    { // Basic (mostly) static checks.
        { // Stuff.
            { // `finish_reason` values.
                // This lets user cast booleans to `finish_reason`, which is convenient for serialization.
                static_assert(int(rcoro::finish_reason::not_finished) == 0);
                static_assert(int(rcoro::finish_reason::reset) == 1);
            }
        }

        { // Empty coroutine.
            auto x = RCORO();
            using X = decltype(x);
            static_assert(rcoro::frame_size<X> == 0);
            static_assert(rcoro::frame_alignment<X> == 1);
            static_assert(rcoro::num_vars<X> == 0);
            THROWS("unknown", rcoro::var_index<X>("?"));
            THROWS("out of range", rcoro::var_name<X>(0));
            static_assert(rcoro::var_index_or_negative<X>("?") == rcoro::unknown_name);
            static_assert(rcoro::num_yields<X> == 1);
            static_assert(rcoro::yield_name<X>(0) == "");
            static_assert(rcoro::yield_name_const<X, 0>.view() == "");
            static_assert(rcoro::yield_index<X>("") == 0);
            THROWS("unknown", rcoro::yield_index<X>("?"));
            static_assert(rcoro::yield_index_or_negative<X>("") == 0);
            static_assert(rcoro::yield_index_or_negative<X>("?") == rcoro::unknown_name);
            static_assert(rcoro::yield_index_const<X, ""> == 0);
            static_assert(rcoro::yields_names_are_unique<X>);
            static_assert(rcoro::yield_vars<X, 0>.empty());
            THROWS("variable index is out of range", rcoro::var_lifetime_overlaps_yield<X>(0, 0));

            static_assert(rcoro::var_names_are_unique_per_yield<X>);
        }

        { // Single yield point.
            auto x = RCORO(RC_YIELD("y"););
            using X = decltype(x);
            static_assert(rcoro::frame_size<X> == 0);
            static_assert(rcoro::frame_alignment<X> == 1);
            static_assert(rcoro::num_vars<X> == 0);
            THROWS("unknown", rcoro::var_index<X>("?"));
            THROWS("out of range", rcoro::var_name<X>(0));
            static_assert(rcoro::var_index_or_negative<X>("?") == rcoro::unknown_name);
            static_assert(rcoro::num_yields<X> == 2);
            static_assert(rcoro::yield_name<X>(0) == "");
            static_assert(rcoro::yield_name<X>(1) == "y");
            static_assert(rcoro::yield_name_const<X, 0>.view() == "");
            static_assert(rcoro::yield_name_const<X, 1>.view() == "y");
            static_assert(rcoro::yield_index<X>("") == 0);
            static_assert(rcoro::yield_index<X>("y") == 1);
            THROWS("unknown", rcoro::yield_index<X>("?"));
            static_assert(rcoro::yield_index_or_negative<X>("") == 0);
            static_assert(rcoro::yield_index_or_negative<X>("y") == 1);
            static_assert(rcoro::yield_index_or_negative<X>("?") == rcoro::unknown_name);
            static_assert(rcoro::yield_index_const<X, ""> == 0);
            static_assert(rcoro::yield_index_const<X, "y"> == 1);
            static_assert(rcoro::yields_names_are_unique<X>);
            static_assert(rcoro::yield_vars<X, 0>.empty());
            static_assert(rcoro::yield_vars<X, 1>.empty());
            THROWS("variable index is out of range", rcoro::var_lifetime_overlaps_yield<X>(0, 0));
            THROWS("variable index is out of range", rcoro::var_lifetime_overlaps_yield<X>(1, 0));

            static_assert(rcoro::var_names_are_unique_per_yield<X>);

            { // Single unnamed yield point.
                auto x = RCORO(RC_YIELD(););
                using X = decltype(x);
                static_assert(rcoro::num_yields<X> == 2);
                static_assert(rcoro::yield_name<X>(0) == "");
                static_assert(rcoro::yield_name<X>(1) == "");
                static_assert(rcoro::yield_name_const<X, 0>.view() == "");
                static_assert(rcoro::yield_name_const<X, 1>.view() == "");
                THROWS("ambiguous", rcoro::yield_index<X>(""));
                THROWS("unknown", rcoro::yield_index<X>("?"));
                static_assert(rcoro::yield_index_or_negative<X>("") == rcoro::ambiguous_name);
                static_assert(rcoro::yield_index_or_negative<X>("?") == rcoro::unknown_name);
                static_assert(!rcoro::yields_names_are_unique<X>);
                static_assert(rcoro::yield_vars<X, 0>.empty());
                static_assert(rcoro::yield_vars<X, 1>.empty());
                static_assert(rcoro::var_names_are_unique_per_yield<X>);
            }
        }

        { // Single variable.
            auto x = RCORO(RC_VAR(a, 42); (void)a;);
            using X = decltype(x);
            static_assert(rcoro::num_vars<X> == 1);
            static_assert(rcoro::var_index<X>("a") == 0);
            THROWS("unknown", rcoro::var_index<X>("?"));
            static_assert(rcoro::var_index_or_negative<X>("a") == 0);
            static_assert(rcoro::var_index_or_negative<X>("?") == rcoro::unknown_name);
            static_assert(rcoro::var_name<X>(0) == "a");
            THROWS("out of range", rcoro::var_name<X>(1));
            static_assert(rcoro::num_yields<X> == 1);
            static_assert(rcoro::yield_vars<X, 0>.empty());
            ASSERT(!rcoro::var_lifetime_overlaps_yield<X>(0, 0));

            THROWS("unknown", rcoro::var_index_at_yield<X>(0, "a"));
            static_assert(rcoro::var_index_at_yield_or_negative<X>(0, "a") == rcoro::unknown_name);

            static_assert(rcoro::var_names_are_unique_per_yield<X>);
        }

        { // Single variable visible from a single yield point.
            auto x = RCORO(RC_VAR(a, 42); (void)a; RC_YIELD(););
            using X = decltype(x);
            static_assert(rcoro::frame_size<X> == sizeof(int));
            static_assert(rcoro::frame_alignment<X> == alignof(int));
            static_assert(rcoro::num_vars<X> == 1);
            static_assert(rcoro::num_yields<X> == 2);
            static_assert(rcoro::yield_vars<X, 0>.empty());
            static_assert(rcoro::yield_vars<X, 1> == std::array{0});
            ASSERT(!rcoro::var_lifetime_overlaps_yield<X>(0, 0));
            ASSERT( rcoro::var_lifetime_overlaps_yield<X>(0, 1));

            THROWS("unknown", rcoro::var_index_at_yield<X>(0, "a"));
            ASSERT(rcoro::var_index_at_yield<X>(1, "a") == 0);

            static_assert(rcoro::var_names_are_unique_per_yield<X>);
        }

        { // Ambiguous variable name.
            auto x = RCORO({
                RC_VAR(a, 1);
                (void)a;
                {
                    RC_VAR(a, 2);
                    (void)a;
                    RC_YIELD();
                }
            });
            using X = decltype(x);
            static_assert(rcoro::num_vars<X> == 2);
            THROWS("ambiguous", rcoro::var_index<X>("a"));
            THROWS("unknown", rcoro::var_index<X>("?"));
            static_assert(rcoro::var_index_or_negative<X>("a") == rcoro::ambiguous_name);
            static_assert(rcoro::var_index_or_negative<X>("?") == rcoro::unknown_name);
            static_assert(rcoro::var_name<X>(0) == "a");
            static_assert(rcoro::var_name<X>(1) == "a");

            THROWS("unknown", rcoro::var_index_at_yield<X>(0, "a"));
            THROWS("ambiguous", rcoro::var_index_at_yield<X>(1, "a"));
            THROWS("unknown", rcoro::var_index_at_yield<X>(1, "?"));

            static_assert(!rcoro::var_names_are_unique_per_yield<X>);
        }

        { // `frame_is_trivially_copyable`
            { // Empty coroutine.
                auto x = RCORO();
                static_assert(rcoro::frame_is_trivially_copyable<decltype(x)>);
            }

            { // Trivially copyable variable.
                auto x = RCORO(RC_VAR(a, int(42)); (void)a; RC_YIELD(););
                static_assert(rcoro::frame_is_trivially_copyable<decltype(x)>);
            }

            { // Non-trivially copyable variable.
                auto x = RCORO(RC_VAR(a, A<int>(42)); (void)a; RC_YIELD(););
                static_assert(!rcoro::frame_is_trivially_copyable<decltype(x)>);
            }

            { // Non-trivially copyable variable, but not visible at any yield points.
                auto x = RCORO(RC_VAR(a, A<int>(42)); (void)a;);
                // Currently those are taken into account, but we can change it later.
                static_assert(!rcoro::frame_is_trivially_copyable<decltype(x)>);
            }
        }
    }

    { // A simple coroutine.
        auto x = RCORO({
            {
                RC_VAR(a, A(10));
                (void)a;
            }
            {
                RC_VAR(b, A(20));
                (void)b;
                RC_YIELD("f");
            }

            RC_FOR((c, A(char{})); char(c) < 3; c = char(char(c)+1))
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
        using X = decltype(x);
        static_assert(rcoro::frame_size<X> == sizeof(int) * 2);
        static_assert(rcoro::frame_alignment<X> == alignof(int));
        static_assert(rcoro::num_vars<X> == 5);
        static_assert(rcoro::var_offset<X, 0> == 0);
        static_assert(rcoro::var_offset<X, 1> == 0);
        static_assert(rcoro::var_offset<X, 2> == 0);
        static_assert(rcoro::var_offset<X, 3> == sizeof(short));
        static_assert(rcoro::var_offset<X, 4> == sizeof(int));
        static_assert( rcoro::var_lifetime_overlaps_var<X, 0, 0> && !rcoro::var_lifetime_overlaps_var<X, 1, 0> && !rcoro::var_lifetime_overlaps_var<X, 2, 0> && !rcoro::var_lifetime_overlaps_var<X, 3, 0> && !rcoro::var_lifetime_overlaps_var<X, 4, 0>);
        static_assert(!rcoro::var_lifetime_overlaps_var<X, 0, 1> &&  rcoro::var_lifetime_overlaps_var<X, 1, 1> && !rcoro::var_lifetime_overlaps_var<X, 2, 1> && !rcoro::var_lifetime_overlaps_var<X, 3, 1> && !rcoro::var_lifetime_overlaps_var<X, 4, 1>);
        static_assert(!rcoro::var_lifetime_overlaps_var<X, 0, 2> && !rcoro::var_lifetime_overlaps_var<X, 1, 2> &&  rcoro::var_lifetime_overlaps_var<X, 2, 2> &&  rcoro::var_lifetime_overlaps_var<X, 3, 2> &&  rcoro::var_lifetime_overlaps_var<X, 4, 2>);
        static_assert(!rcoro::var_lifetime_overlaps_var<X, 0, 3> && !rcoro::var_lifetime_overlaps_var<X, 1, 3> &&  rcoro::var_lifetime_overlaps_var<X, 2, 3> &&  rcoro::var_lifetime_overlaps_var<X, 3, 3> && !rcoro::var_lifetime_overlaps_var<X, 4, 3>);
        static_assert(!rcoro::var_lifetime_overlaps_var<X, 0, 4> && !rcoro::var_lifetime_overlaps_var<X, 1, 4> &&  rcoro::var_lifetime_overlaps_var<X, 2, 4> && !rcoro::var_lifetime_overlaps_var<X, 3, 4> &&  rcoro::var_lifetime_overlaps_var<X, 4, 4>);
        static_assert(!rcoro::var_lifetime_overlaps_yield_const<X, 0, 0> && !rcoro::var_lifetime_overlaps_yield_const<X, 1, 0> && !rcoro::var_lifetime_overlaps_yield_const<X, 2, 0> && !rcoro::var_lifetime_overlaps_yield_const<X, 3, 0> && !rcoro::var_lifetime_overlaps_yield_const<X, 4, 0> && rcoro::yield_vars<X, 0>.empty());
        static_assert(!rcoro::var_lifetime_overlaps_yield_const<X, 0, 1> &&  rcoro::var_lifetime_overlaps_yield_const<X, 1, 1> && !rcoro::var_lifetime_overlaps_yield_const<X, 2, 1> && !rcoro::var_lifetime_overlaps_yield_const<X, 3, 1> && !rcoro::var_lifetime_overlaps_yield_const<X, 4, 1> && rcoro::yield_vars<X, 1> == std::array{1});
        static_assert(!rcoro::var_lifetime_overlaps_yield_const<X, 0, 2> && !rcoro::var_lifetime_overlaps_yield_const<X, 1, 2> &&  rcoro::var_lifetime_overlaps_yield_const<X, 2, 2> &&  rcoro::var_lifetime_overlaps_yield_const<X, 3, 2> && !rcoro::var_lifetime_overlaps_yield_const<X, 4, 2> && rcoro::yield_vars<X, 2> == std::array{2,3});
        static_assert(!rcoro::var_lifetime_overlaps_yield_const<X, 0, 3> && !rcoro::var_lifetime_overlaps_yield_const<X, 1, 3> &&  rcoro::var_lifetime_overlaps_yield_const<X, 2, 3> && !rcoro::var_lifetime_overlaps_yield_const<X, 3, 3> &&  rcoro::var_lifetime_overlaps_yield_const<X, 4, 3> && rcoro::yield_vars<X, 3> == std::array{2,4});
        THROWS("variable index is out of range", rcoro::var_lifetime_overlaps_yield<X>(-1, 0));
        THROWS("variable index is out of range", rcoro::var_lifetime_overlaps_yield<X>(rcoro::num_vars<X>, 0));
        THROWS("yield point index is out of range", rcoro::var_lifetime_overlaps_yield<X>(0, -1));
        THROWS("yield point index is out of range", rcoro::var_lifetime_overlaps_yield<X>(0, rcoro::num_yields<X>));
        ASSERT(!rcoro::var_lifetime_overlaps_yield<X>(0, 0) && !rcoro::var_lifetime_overlaps_yield<X>(1, 0) && !rcoro::var_lifetime_overlaps_yield<X>(2, 0) && !rcoro::var_lifetime_overlaps_yield<X>(3, 0) && !rcoro::var_lifetime_overlaps_yield<X>(4, 0));
        ASSERT(!rcoro::var_lifetime_overlaps_yield<X>(0, 1) &&  rcoro::var_lifetime_overlaps_yield<X>(1, 1) && !rcoro::var_lifetime_overlaps_yield<X>(2, 1) && !rcoro::var_lifetime_overlaps_yield<X>(3, 1) && !rcoro::var_lifetime_overlaps_yield<X>(4, 1));
        ASSERT(!rcoro::var_lifetime_overlaps_yield<X>(0, 2) && !rcoro::var_lifetime_overlaps_yield<X>(1, 2) &&  rcoro::var_lifetime_overlaps_yield<X>(2, 2) &&  rcoro::var_lifetime_overlaps_yield<X>(3, 2) && !rcoro::var_lifetime_overlaps_yield<X>(4, 2));
        ASSERT(!rcoro::var_lifetime_overlaps_yield<X>(0, 3) && !rcoro::var_lifetime_overlaps_yield<X>(1, 3) &&  rcoro::var_lifetime_overlaps_yield<X>(2, 3) && !rcoro::var_lifetime_overlaps_yield<X>(3, 3) &&  rcoro::var_lifetime_overlaps_yield<X>(4, 3));

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
            A<char>::~A(-1)              # ^
            A<short>::A(30)              # {{{
            ...
            A<short>::~A(30)
            A<int>::A(40)
            ...
            A<int>::~A(40)               # }}}
            A<char>::A(2)
            A<char>::operator=(A && = 2)
            A<char>::~A(-2)
            A<short>::A(30)              # {{{
            ...
            A<short>::~A(30)
            A<int>::A(40)
            ...
            A<int>::~A(40)               # }}}
            A<char>::A(3)                # The loop counter is incremented the last time.
            A<char>::operator=(A && = 3) # ^
            A<char>::~A(-3)              # ^
            A<char>::~A(3)               # The loop counter dies.
            ...                          # Pause at `i`.
        )");

        std::string state_dump;

        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 0 && x.yield_point_name() == ""  && !x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && x());
        *test_detail::a_log += "...\n";
        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 1 && x.yield_point_name() == "f" && !x.var_exists<"a">() &&  x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && x());
        *test_detail::a_log += "...\n";
        for (int i = 0; i < 3; i++)
        {
            ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 2 && x.yield_point_name() == "g" && !x.var_exists<"a">() && !x.var_exists<"b">() &&  x.var_exists<"c">() &&  x.var_exists<"d">() && !x.var_exists<"e">() && x());
            *test_detail::a_log += "...\n";

            if (i == 1)
            {
                std::ostringstream ss;
                ss << x;
                state_dump = ss.str();
            }

            ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 3 && x.yield_point_name() == "h" && !x.var_exists<"a">() && !x.var_exists<"b">() &&  x.var_exists<"c">() && !x.var_exists<"d">() &&  x.var_exists<"e">() && x());
            *test_detail::a_log += "...\n";
        }
        ASSERT(x && !x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::not_finished && x.yield_point() == 4 && x.yield_point_name() == "i" && !x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">() && !x.var_exists<"d">() && !x.var_exists<"e">() && !x());
        ASSERT(!x && x.finished() && !x.busy() && x.finish_reason() == rcoro::finish_reason::success);

        { // Debug info text.
            { // Static.
                std::ostringstream expected_ss;
                expected_ss <<
R"(copying: ctor=yes assign=yes
moving: ctor=yes(nothrow) assign=yes(nothrow)
frame: size=)" << rcoro::frame_size<X> << R"( align=)" << rcoro::frame_alignment<X> << R"(
5 variables:
  0. a, A<int>
      offset=)" << rcoro::var_offset<X, 0> << R"(, size=)" << sizeof(rcoro::var_type<X, 0>) << R"(, align=)" << alignof(rcoro::var_type<X, 0>) << R"(
  1. b, A<int>
      offset=)" << rcoro::var_offset<X, 1> << R"(, size=)" << sizeof(rcoro::var_type<X, 1>) << R"(, align=)" << alignof(rcoro::var_type<X, 1>) << R"(
  2. c, A<char>
      offset=)" << rcoro::var_offset<X, 2> << R"(, size=)" << sizeof(rcoro::var_type<X, 2>) << R"(, align=)" << alignof(rcoro::var_type<X, 2>) << R"(
  3. d, A<short>
      offset=)" << rcoro::var_offset<X, 3> << R"(, size=)" << sizeof(rcoro::var_type<X, 3>) << R"(, align=)" << alignof(rcoro::var_type<X, 3>) << R"(
      visible_vars: 2.c
  4. e, A<int>
      offset=)" << rcoro::var_offset<X, 4> << R"(, size=)" << sizeof(rcoro::var_type<X, 4>) << R"(, align=)" << alignof(rcoro::var_type<X, 4>) << R"(
      visible_vars: 2.c
5 yields:
  0. ``
  1. `f`, visible_vars: 1.b
  2. `g`, visible_vars: 2.c, 3.d
  3. `h`, visible_vars: 2.c, 4.e
  4. `i`)";

                std::ostringstream log_ss;
                log_ss << rcoro::debug_info<X>;
                std::string log = log_ss.str();
                log = test_detail::string_replace(log, "short int", "short"); // GCC needs this, because it spells the type as `short int`.

                if (log != expected_ss.str())
                {
                    std::cout << log << '\n';
                    FAIL("Debug output mismatch (static)!");
                }
            }

            { // Dynamic.
                std::string expected =
R"(yield_point = 3, `h`
  0. a - dead
  1. b - dead
  2. c - alive but not printable
  3. d - dead
  4. e = 40)";

                if (expected != state_dump)
                {
                    std::cout << state_dump;
                    FAIL("Debug output mismatch (dynamic)!");
                }
            }
        }
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

    { // Parameter passing.
        auto x = RCORO((int x, int &y, float &&z)
        {
            ASSERT(x == 1 && y == 2 && z == 2.5f);
            RC_YIELD();
            ASSERT(x == 3 && y == 4 && z == 4.5f);
        }
        (void)1, (void)2; // While we're at it, test that top-level commas are parsed correctly.
        );
        int y = 2;
        x(1, y, 2.5f);
        y = 4;
        x(3, y, 4.5f);
    }

    { // Constexpr-ness.
        auto prototype = RCORO( // This isn't constexpr merely because we have `goto` inside. This should be fixed in C++23.
            RC_VAR(a, 42);
            (void)a;
            RC_YIELD();
        );
        constexpr auto x = []{
            auto x = decltype(prototype){};
            auto y(x);
            x = y;
            auto z(std::move(x));
            x = std::move(z);
            x.reset();
            x.rewind();
            ASSERT(!x.var_exists<"a">());
            ASSERT(!x.busy());
            ASSERT(!x.finished());
            ASSERT(x.finish_reason() == rcoro::finish_reason::not_finished);
            ASSERT(x.yield_point() == 0);
            ASSERT(x.yield_point_name() == "");
            return x;
        }();

        #if 0 // This might never happen, because `goto` is still not allowed in constexpr as of C++23 (allowed only if not executed).
        { // Execution.
            { // Without variables.
                constexpr int sum = []{
                    int sum = 0;

                    auto x = RCORO((int &value)
                    {
                        value = 1;
                        RC_YIELD();
                        value = 2;
                        RC_YIELD();
                        value = 3;
                        RC_YIELD();
                    });
                    for (int value{}; x(value);)
                        sum += value;

                    return sum;
                }();
                static_assert(sum == 6);
            }
        }
        #endif
    }

    { // Local types as variables.
        { // Local structs as variables.
            auto x = RCORO({
                struct A {};
                RC_VAR(a, A{});
                (void)a;
            });
            x();
        }

        { // Lambdas as variables.
            auto x = RCORO((int &out)
            {
                RC_VAR(y, 42);
                RC_VAR(a, [&y]{return y;});
                RC_YIELD();
                out = a();
            });
            int out = 0;
            x(out);
            x(out);
            ASSERT(out == 42);
        }

        { // Nested coroutines.
            auto x = RCORO((int &f, int &g)
            {
                RC_VAR(y, RCORO((int &g)
                {
                    g = 1;
                    RC_YIELD();
                    g = 2;
                    RC_YIELD();
                }));
                f = 3;
                while (y(g)) RC_YIELD();
                y.rewind();
                f = 4;
                while (y(g)) RC_YIELD();
            });

            int f = 0, g = 0;
            x(f, g);
            ASSERT(f == 3 && g == 1);
            x(f, g);
            ASSERT(f == 3 && g == 2);
            x(f, g);
            ASSERT(f == 4 && g == 1);
            x(f, g);
            ASSERT(f == 4 && g == 2);
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

        { // Body throws before the first yield point.
            auto x = RCORO({
                *test_detail::a_log += "throw!\n";
                throw 42;

                {
                    RC_VAR(unused, A(1)); // Skip index `0` to catch more bugs.
                    (void)unused;
                }

                RC_VAR(a, A(short(2)));
                (void)a;
                RC_VAR(b, A(long(3)));
                (void)b;
            });

            Expect ex("throw!");

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

    { // Rule of five.
        enum class kind {start, yield, exception, _count};
        static constexpr const char *kind_names[] = {"start", "yield", "exception"};
        static_assert(std::size(kind_names) == std::size_t(kind::_count));

        auto lambda = [&](auto move, auto assign, kind source_kind, kind target_kind)
        {
            static bool should_throw = false;

            auto x = RCORO({
                // Throw if necessary.
                if (should_throw)
                {
                    should_throw = false;
                    throw 42;
                }

                {
                    RC_VAR(unused, A(1)); // Skip index `0` to catch more bugs.
                    (void)unused;
                }

                RC_VAR(a, A(short(2)));
                (void)a;
                RC_VAR(b, A(long(3)));
                (void)b;

                RC_YIELD();

                {
                    RC_VAR(d, A(int(4)));
                    (void)d;
                    RC_YIELD();
                }

                {
                    RC_VAR(e, A(int(5)));
                    (void)e;
                    RC_YIELD();
                }
            });

            Expect ex(std::string(R"(
                ... create source
            )")
            // Only if the source object is paused at a yield point, creating it involves constructing variables.
            + (source_kind == kind::yield ? R"(
                A<int>::A(1)
                A<int>::~A(1)
                A<short>::A(2)
                A<long>::A(3)
                A<int>::A(4)
            )" : "")
            // If we're assigning, we need to construct a target objecct.
            + (!assign.value ? "" : std::string(R"(
                ... create target  # only assignments have a target object
            )")
                // Only if the target object is paused at a yield point, creating it involves constructing variables.
                + (target_kind == kind::yield ? R"(
                    A<int>::A(1)
                    A<int>::~A(1)
                    A<short>::A(2)
                    A<long>::A(3)
                    A<int>::A(4)
                    A<int>::~A(4)
                    A<int>::A(5)
                )" : "")
            )
            + R"(
                ... act
            )"
            // If we're assigning, we need to destroy the existing state first. This produces output only if it's paused at a yield point.
            + (assign.value && target_kind == kind::yield ? R"(
                A<int>::~A(5) # destroy the assignment target
                A<long>::~A(3)
                A<short>::~A(2)
            )" : "")
            // If the source is not at a yield point, copying/moving it prints nothing.
            // If it's being copied, then we just copy the variables.
            // If it's being moved, we move the variables and destroy the originals.
            + (source_kind != kind::yield ? ""/*no copies or moves*/ : !move.value ? R"(
                A<short>::A(const A & = 2) # copy
                A<long>::A(const A & = 3)
                A<int>::A(const A & = 4)
            )" : R"(
                A<short>::A(A && = 2) # move
                A<long>::A(A && = 3)
                A<int>::A(A && = 4)
                A<int>::~A(-4) # destroy moved-from source
                A<long>::~A(-3)
                A<short>::~A(-2)
            )")
            // Destroy the target object. This produces output only if the source had state in the first place.
            + R"(
                ... destroy target
            )" + (source_kind == kind::yield ? R"(
                A<int>::~A(4)
                A<long>::~A(3)
                A<short>::~A(2)
            )" : "")
            // Destroy the source object. This produces output only if had the state in the first place and we didn't move it away.
            + R"(
                ... destroy source
            )" + (source_kind == kind::yield && !move.value ? R"(
                A<int>::~A(4) # destroy copy sources
                A<long>::~A(3)
                A<short>::~A(2)
            )" : ""));

            *test_detail::a_log += std::string("# ") + (move.value ? "move" : "copy") + " " + (assign.value ? "assign" : "construct") + "\n";
            *test_detail::a_log += std::string("# source is: ") + kind_names[int(source_kind)] + "\n";
            if constexpr (assign.value)
                *test_detail::a_log += std::string("# target is: ") + kind_names[int(target_kind)] + "\n";
            *test_detail::a_log += "... create source\n";
            decltype(x) source;
            source.rewind();
            switch (source_kind)
            {
              case kind::start:
                break;
              case kind::yield:
                source()();
                break;
              case kind::exception:
                should_throw = true;
                try {source();} catch (int) {}
              case kind::_count:
                break;
            }

            auto check_target = [point = source.yield_point(), reason = source.finish_reason()](const decltype(x) &target)
            {
                ASSERT(target.yield_point() == point);
                ASSERT(target.finish_reason() == reason);
            };

            {
                if constexpr (assign.value)
                {
                    *test_detail::a_log += "... create target\n";
                    decltype(x) target;
                    target.rewind();
                    switch (target_kind)
                    {
                      case kind::start:
                        break;
                      case kind::yield:
                        target()()();
                        break;
                      case kind::exception:
                        should_throw = true;
                        try {target();} catch (int) {}
                      case kind::_count:
                        break;
                    }

                    *test_detail::a_log += "... act\n";

                    if constexpr (move.value)
                        target = std::move(source);
                    else
                        target = source;
                    check_target(target);
                    *test_detail::a_log += "... destroy target\n";
                }
                else
                {
                    *test_detail::a_log += "... act\n";
                    if constexpr (move.value)
                    {
                        auto target = std::move(source);
                        check_target(target);
                        *test_detail::a_log += "... destroy target\n";
                    }
                    else
                    {
                        auto target = source;
                        check_target(target);
                        *test_detail::a_log += "... destroy target\n";
                    }
                }

                // Check that moved-from object is empty.
                if constexpr (move.value)
                {
                    ASSERT(source.finished() && source.finish_reason() == rcoro::finish_reason::reset && source.yield_point() == 0);
                }

                *test_detail::a_log += "... destroy source\n";
            }
        };

        for (int i = 0; i < int(kind::_count); i++)
        {
            lambda(std::false_type{}, std::false_type{}, kind(i), kind::_count); // Copy construct.
            lambda(std::true_type{}, std::false_type{}, kind(i), kind::_count); // Move construct.
            for (int j = 0; j < int(kind::_count); j++)
            {
                lambda(std::false_type{}, std::true_type{}, kind(i), kind(j)); // Copy assign.
                lambda(std::true_type{}, std::true_type{}, kind(i), kind(j)); // Move assign.
            }
        }
    }

    { // Rule of five: conditions and noexcept-ness.
        { // No vars.
            auto x = RCORO();
            static_assert(std::is_copy_constructible_v<decltype(x)> && std::is_nothrow_copy_constructible_v<decltype(x)>);
            static_assert(std::is_move_constructible_v<decltype(x)> && std::is_nothrow_move_constructible_v<decltype(x)>);
            static_assert(std::is_copy_assignable_v<decltype(x)> && std::is_nothrow_copy_assignable_v<decltype(x)>);
            static_assert(std::is_move_assignable_v<decltype(x)> && std::is_nothrow_move_assignable_v<decltype(x)>);
        }

        { // Immovable var.
            auto x = RCORO(RC_VAR(a, B<int, ops::none>(1)); (void)a;);
            static_assert(!std::is_copy_constructible_v<decltype(x)> && !std::is_nothrow_copy_constructible_v<decltype(x)>);
            static_assert(!std::is_move_constructible_v<decltype(x)> && !std::is_nothrow_move_constructible_v<decltype(x)>);
            static_assert(!std::is_copy_assignable_v<decltype(x)> && !std::is_nothrow_copy_assignable_v<decltype(x)>);
            static_assert(!std::is_move_assignable_v<decltype(x)> && !std::is_nothrow_move_assignable_v<decltype(x)>);
        }

        { // Only move-constructible var.
            auto x = RCORO(RC_VAR(a, B<int, ops::move_ctor>(1)); (void)a;);
            static_assert(!std::is_copy_constructible_v<decltype(x)> && !std::is_nothrow_copy_constructible_v<decltype(x)>);
            static_assert(std::is_move_constructible_v<decltype(x)> && !std::is_nothrow_move_constructible_v<decltype(x)>);
            static_assert(!std::is_copy_assignable_v<decltype(x)> && !std::is_nothrow_copy_assignable_v<decltype(x)>);
            static_assert(std::is_move_assignable_v<decltype(x)> && !std::is_nothrow_move_assignable_v<decltype(x)>);
        }

        { // Only nothrow-move-constructible var.
            auto x = RCORO(RC_VAR(a, B<int, ops::nothrow_move_ctor>(1)); (void)a;);
            static_assert(!std::is_copy_constructible_v<decltype(x)> && !std::is_nothrow_copy_constructible_v<decltype(x)>);
            static_assert(std::is_move_constructible_v<decltype(x)> && std::is_nothrow_move_constructible_v<decltype(x)>);
            static_assert(!std::is_copy_assignable_v<decltype(x)> && !std::is_nothrow_copy_assignable_v<decltype(x)>);
            static_assert(std::is_move_assignable_v<decltype(x)> && std::is_nothrow_move_assignable_v<decltype(x)>);
        }

        { // Copy- and move-constructible var.
            auto x = RCORO(RC_VAR(a, B<int, ops::copy_ctor | ops::move_ctor>(1)); (void)a;);
            static_assert(std::is_copy_constructible_v<decltype(x)> && !std::is_nothrow_copy_constructible_v<decltype(x)>);
            static_assert(std::is_move_constructible_v<decltype(x)> && !std::is_nothrow_move_constructible_v<decltype(x)>);
            static_assert(std::is_copy_assignable_v<decltype(x)> && !std::is_nothrow_copy_assignable_v<decltype(x)>);
            static_assert(std::is_move_assignable_v<decltype(x)> && !std::is_nothrow_move_assignable_v<decltype(x)>);
        }
    }

    { // Rule of five: the lack of `std::move_if_noexcept`.
        auto lambda = [](auto current_ops_param)
        {
            static constexpr ops current_ops = current_ops_param.value;
            auto x = RCORO(RC_VAR(a, B<int, current_ops>(1)); (void)a; RC_YIELD(););

            Expect ex(
                // Would use this if we used `std::move_if_noexcept`:
                // (current_ops & ops::nothrow_move_ctor) == ops::nothrow_move_ctor || !bool(current_ops & ops::copy_ctor)
                // Instead, a simple condition:
                bool(current_ops & ops::move_ctor)
            ? R"(
                A<int>::A(1)
                A<int>::A(A && = 1)
                A<int>::~A(-1) # move source is destroyed here, immediately
                ... destroying target
                A<int>::~A(1)
                ... destroying source
                # nothing here, already destroyed the source state
            )" : R"(
                A<int>::A(1)
                A<int>::A(const A & = 1)
                A<int>::~A(1) # move source is destroyed here, immediately, even if we're actually copying
                ... destroying target
                A<int>::~A(1)
                ... destroying source
                # nothing here, already destroyed the source state
            )");

            *test_detail::a_log += "# ";
            if ((current_ops & ops::nothrow_copy_ctor) == ops::nothrow_copy_ctor) *test_detail::a_log += "nothrow-copyable";
            else if (bool(current_ops & ops::copy_ctor)) *test_detail::a_log += "copyable";
            else *test_detail::a_log += "non-copyable";
            *test_detail::a_log += " + ";
            if ((current_ops & ops::nothrow_move_ctor) == ops::nothrow_move_ctor) *test_detail::a_log += "nothrow-movable";
            else if (bool(current_ops & ops::move_ctor)) *test_detail::a_log += "movable";
            else *test_detail::a_log += "non-movable";
            *test_detail::a_log += '\n';

            x();
            {
                auto y(std::move(x));
                *test_detail::a_log += "... destroying target\n";
            }
            *test_detail::a_log += "... destroying source\n";
        };

        // lambda(std::integral_constant<ops, ops::none              | ops::none             >{}); // Not movable -> no point in testing this.
        lambda(std::integral_constant<ops, ops::copy_ctor         | ops::none             >{});
        lambda(std::integral_constant<ops, ops::nothrow_copy_ctor | ops::none             >{});
        lambda(std::integral_constant<ops, ops::none              | ops::move_ctor        >{});
        lambda(std::integral_constant<ops, ops::copy_ctor         | ops::move_ctor        >{});
        lambda(std::integral_constant<ops, ops::nothrow_copy_ctor | ops::move_ctor        >{});
        lambda(std::integral_constant<ops, ops::none              | ops::nothrow_move_ctor>{});
        lambda(std::integral_constant<ops, ops::copy_ctor         | ops::nothrow_move_ctor>{});
        lambda(std::integral_constant<ops, ops::nothrow_copy_ctor | ops::nothrow_move_ctor>{});
    }

    { // Rule of five: exception handling.
        auto lambda = [&](auto move, auto assign)
        {
            auto x = RCORO({
                {
                    RC_VAR(unused, A(0)); // Skip index `0` to catch more bugs.
                    (void)unused;
                }

                RC_VAR(a, A(1)); (void)a;
                RC_VAR(b, A(2)); (void)b;
                RC_VAR(c, B<int, ops::copy_ctor_throws | ops::move_ctor_throws>(3)); (void)c;
                RC_VAR(d, A(4)); (void)d;
                RC_YIELD();
            });

            Expect ex(std::string(R"(
                ... create source
                A<int>::A(0)
                A<int>::~A(0)
                A<int>::A(1)
                A<int>::A(2)
                A<int>::A(3)
                A<int>::A(4)
                ... act
            )") + (!move.value ? R"(
                A<int>::A(const A & = 1)
                A<int>::A(const A & = 2)
            )" : R"(
                A<int>::A(A && = 1)
                A<int>::A(A && = 2)
            )") + R"(
                throw!
                A<int>::~A(2) # destroy newly constructed objects
                A<int>::~A(1) # ^
            )" + (!move.value ? std::string(assign.value ? "... destroy target" : "") + R"(
                ... destroy source
                A<int>::~A(4)
                A<int>::~A(3)
                A<int>::~A(2)
                A<int>::~A(1)
            )" : R"(
                # destroy early, while the move operation still runs.
                A<int>::~A(4)
                A<int>::~A(3)
                A<int>::~A(-2) # those are already moved-from
                A<int>::~A(-1) # ^
            )" + std::string(assign.value ? "... destroy target" : "") + R"(
                ... destroy source
                # nothing remains here
            )"));

            *test_detail::a_log += std::string("# ") + (move.value ? "move" : "copy") + " " + (assign.value ? "assign" : "construct") + "\n";

            *test_detail::a_log += "... create source\n";
            decltype(x) source;
            source.rewind()();

            {
                if constexpr (assign.value)
                {
                    decltype(x) target;
                    *test_detail::a_log += "... act\n";

                    try
                    {
                        if constexpr (move.value)
                            target = std::move(source);
                        else
                            target = source;
                    }
                    catch (int) {}

                    // ASSERT(target.finished() && target.finish_reason() == rcoro::finish_reason::reset && target.yield_point() == 0);
                    *test_detail::a_log += "... destroy target\n";
                }
                else
                {
                    *test_detail::a_log += "... act\n";
                    try
                    {
                        if constexpr (move.value)
                            auto target = std::move(source);
                        else
                            auto target = source;
                    }
                    catch (int) {}
                }

                // ASSERT(source.finished() && source.finish_reason() == rcoro::finish_reason::reset && source.yield_point() == 0);

                *test_detail::a_log += "... destroy source\n";
            }
        };

        lambda(std::false_type{}, std::false_type{}); // Copy construct.
        lambda(std::false_type{}, std::true_type{}); // Copy assign.
        lambda(std::true_type{}, std::false_type{}); // Move construct.
        lambda(std::true_type{}, std::true_type{}); // Move assign.
    }

    { // Busy coroutines.
        static std::function<void(int)> func;
        auto x = RCORO({
            func(0);
            RC_VAR(a, int(42));
            (void)a;
            RC_YIELD("y");
            func(1);
        });
        func = [&x](int yield_point)
        {
            (void)x.frame_storage(); // This shouldn't throw here.
            ASSERT(x.busy());
            ASSERT(x.yield_point() == yield_point);
            ASSERT(x.yield_point_name() == (yield_point == 0 ? "" : "y"));
            THROWS("busy", x.var_exists<"a">());
            THROWS("busy", x.var<"a">());
            THROWS("busy", x.reset());
            THROWS("busy", x.rewind());
            THROWS("busy", x.for_each_alive_var([](auto){return false;}));
        };
        x()();
    }

    { // Variable access.
        { // Basic checks.
            auto x = RCORO({
                {
                    RC_VAR(a, int{}); // Skip index 0 to catch more bugs.
                    (void)a;
                }

                RC_VAR(b, short(1));
                (void)b;
                RC_YIELD();
                RC_VAR(c, long(2));
                (void)c;
                RC_YIELD();
            });

            static_assert(std::is_same_v<decltype(              x .var<"a">()),       int   &>);
            static_assert(std::is_same_v<decltype(std::as_const(x).var<"a">()), const int   &>);
            static_assert(std::is_same_v<decltype(              x .var<"b">()),       short &>);
            static_assert(std::is_same_v<decltype(std::as_const(x).var<"b">()), const short &>);
            static_assert(std::is_same_v<decltype(              x .var<"c">()),       long  &>);
            static_assert(std::is_same_v<decltype(std::as_const(x).var<"c">()), const long  &>);

            std::string vars;

            ASSERT(!x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">());
            THROWS("doesn't exist", x.var<"a">());
            THROWS("doesn't exist", x.var<"b">());
            THROWS("doesn't exist", x.var<"c">());
            vars.clear();
            ASSERT(x.for_each_alive_var([&](auto i){vars += rcoro::var_name_const<decltype(x), i.value>.view(); return false;}) == false && vars == "");

            x();

            ASSERT(!x.var_exists<"a">() && x.var_exists<"b">() && !x.var_exists<"c">());
            ASSERT(x.var<"b">() == 1);
            x.var<"b">() += 10;
            ASSERT(x.var<"b">() == 11);
            THROWS("doesn't exist", x.var<"a">());
            THROWS("doesn't exist", x.var<"c">());
            vars.clear();
            ASSERT(x.for_each_alive_var([&](auto i){vars += rcoro::var_name_const<decltype(x), i.value>.view(); return false;}) == false && vars == "b");

            x();

            ASSERT(!x.var_exists<"a">() && x.var_exists<"b">() && x.var_exists<"c">());
            ASSERT(x.var<"b">() == 11);
            x.var<"b">() += 10;
            ASSERT(x.var<"b">() == 21);
            ASSERT(x.var<"c">() == 2);
            x.var<"c">() += 100;
            ASSERT(x.var<"c">() == 102);
            THROWS("doesn't exist", x.var<"a">());
            vars.clear();
            ASSERT(x.for_each_alive_var([&](auto i){vars += rcoro::var_name_const<decltype(x), i.value>.view(); return false;}) == false && vars == "bc");

            x();
            ASSERT(x.finished());

            ASSERT(!x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">());
            THROWS("doesn't exist", x.var<"a">());
            THROWS("doesn't exist", x.var<"b">());
            THROWS("doesn't exist", x.var<"c">());
            vars.clear();
            ASSERT(x.for_each_alive_var([&](auto i){vars += rcoro::var_name_const<decltype(x), i.value>.view(); return false;}) == false && vars == "");

            x.reset();

            ASSERT(!x.var_exists<"a">() && !x.var_exists<"b">() && !x.var_exists<"c">());
            THROWS("doesn't exist", x.var<"a">());
            THROWS("doesn't exist", x.var<"b">());
            THROWS("doesn't exist", x.var<"c">());
            vars.clear();
            ASSERT(x.for_each_alive_var([&](auto i){vars += rcoro::var_name_const<decltype(x), i.value>.view(); return false;}) == false && vars == "");
        }

        { // More `for_each_alive_var` tests.
            { // No variables.
                auto x = RCORO();
                ASSERT(!x.for_each_alive_var([](auto){FAIL("This shouldn't be called.");}));
            }

            { // Interrupting `for_each_alive_var`.
                auto x = RCORO({
                    RC_VAR(a, 1); (void)a;
                    RC_VAR(b, 2); (void)b;
                    RC_VAR(c, 3); (void)c;
                    RC_YIELD();
                });

                x();

                std::string vars;
                bool ret = x.for_each_alive_var([&](auto i)
                {
                    vars += rcoro::var_name_const<decltype(x), i.value>.view();
                    return i.value == 1;
                });
                ASSERT(ret);
                ASSERT(vars == "ab");
            }
        }
    }

    { // Deserialization.
        { // `load_raw_bytes_UNSAFE`.
            auto x = RCORO({
                RC_YIELD();
                {
                    RC_VAR(unused, A(0)); (void)unused; // Skip index 0, this helps catch bugs.
                }

                RC_VAR(a, A(short(1))); (void)a;
                RC_VAR(b, A(long(2))); (void)b;
                RC_VAR(c, A(int(3))); (void)c;
                RC_YIELD();
            });

            { // Failures.
                Expect ex(""); // Nothing here should create objects.

                // Finish reason too small.
                x.rewind()();
                THROWS("finish reason is out of range", x.load_raw_bytes_UNSAFE(rcoro::finish_reason(-1), 0, []{return false;}));
                ASSERT(x.finish_reason() == rcoro::finish_reason::reset);

                // Finish reason too big.
                x.rewind()();
                THROWS("finish reason is out of range", x.load_raw_bytes_UNSAFE(rcoro::finish_reason(rcoro::finish_reason::_count), 0, []{return false;}));
                ASSERT(x.finish_reason() == rcoro::finish_reason::reset);

                // Yield point too small.
                x.rewind()();
                THROWS("yield point index is out of range", x.load_raw_bytes_UNSAFE(rcoro::finish_reason::not_finished, -1, []{return false;}));
                ASSERT(x.finish_reason() == rcoro::finish_reason::reset);

                // Yield point too big.
                x.rewind()();
                THROWS("yield point index is out of range", x.load_raw_bytes_UNSAFE(rcoro::finish_reason::not_finished, rcoro::num_yields<decltype(x)>, []{return false;}));
                ASSERT(x.finish_reason() == rcoro::finish_reason::reset);

                // Yield point is not zero, even though we're finished.
                x.rewind()();
                THROWS("must be 0", x.load_raw_bytes_UNSAFE(rcoro::finish_reason::reset, 1, []{return false;}));
                ASSERT(x.finish_reason() == rcoro::finish_reason::reset);

                // Callback returns false.
                x.rewind()();
                ASSERT(!x.load_raw_bytes_UNSAFE(rcoro::finish_reason::not_finished, 1, []{return false;}));
                ASSERT(x.finish_reason() == rcoro::finish_reason::reset);

                // Callback throws.
                x.rewind()();
                THROWS("test!", x.load_raw_bytes_UNSAFE(rcoro::finish_reason::not_finished, 1, []{throw std::runtime_error("test!"); return false;}));
                ASSERT(x.finish_reason() == rcoro::finish_reason::reset);
            }

            { // Successful loads.
                { // Finished.
                    Expect ex("");

                    decltype(x) y(rcoro::rewind);
                    y();

                    y.load_raw_bytes_UNSAFE(rcoro::finish_reason::exception, 0, []{return true;});
                    ASSERT(y.finished());
                    ASSERT(y.finish_reason() == rcoro::finish_reason::exception);
                    ASSERT(y.yield_point() == 0);
                }

                { // Not started.
                    Expect ex("");

                    decltype(x) y(rcoro::rewind);
                    y();

                    y.rewind()();
                    y.load_raw_bytes_UNSAFE(rcoro::finish_reason::not_finished, 0, []{return true;});
                    ASSERT(!y.finished());
                    ASSERT(y.yield_point() == 0);
                }

                { // Started and have variables.
                    Expect ex(R"(
                        A<long>::A(2)
                        A<short>::A(1)
                        A<int>::A(3)
                        ... done loading
                        A<int>::~A(3)
                        A<long>::~A(2)
                        A<short>::~A(1)
                    )");

                    decltype(x) y(rcoro::rewind);
                    y();

                    y.rewind()();
                    y.load_raw_bytes_UNSAFE(rcoro::finish_reason::not_finished, 2, [&]
                    {
                        // Create out of order for test purposes, why not.
                        ::new((void *)((char *)y.frame_storage() + rcoro::var_offset<decltype(y), 2>)) A(long(2));
                        ::new((void *)((char *)y.frame_storage() + rcoro::var_offset<decltype(y), 1>)) A(short(1));
                        ::new((void *)((char *)y.frame_storage() + rcoro::var_offset<decltype(y), 3>)) A(int(3));
                        return true;
                    });
                    ASSERT(!y.finished());
                    ASSERT(y.yield_point() == 2);
                    *test_detail::a_log += "... done loading\n";
                }
            }
        }

        { // `load_raw_bytes`.
            // We already have a proper test for the `_UNSAFE` version, so we just do a bare minimum.
            auto x = RCORO((int &out)
            {
                {
                    RC_VAR(unused, int(0)); (void)unused; // Skip index 0, this helps catch bugs.
                }

                RC_VAR(x, 0);
                RC_YIELD();
                out = x * 10;

                RC_VAR(dummy, 42); (void)dummy;
                RC_YIELD();
            });

            ASSERT(x.load_raw_bytes(rcoro::finish_reason::not_finished, 1, [&]
            {
                int value = 42;
                std::memcpy((char *)x.frame_storage() + rcoro::var_offset<decltype(x), 1>, &value, sizeof(int));
                return true;
            }));

            int result = 0;
            x(result);
            ASSERT(result == 420);

            { // The same thing again, but make sure that we can load the variables before calling `load_raw_bytes`.
                x.rewind()(result)(result);
                int value = 43;
                std::memcpy((char *)x.frame_storage() + rcoro::var_offset<decltype(x), 1>, &value, sizeof(int));
                ASSERT(x.load_raw_bytes(rcoro::finish_reason::not_finished, 1, []{return true;}));

                result = 0;
                x(result);
                ASSERT(result == 430);
            }
        }

        { // `load`.
            auto x = RCORO({
                RC_YIELD();

                {
                    RC_VAR(unused, A(0)); (void)unused; // Skip index 0, this helps catch bugs.
                }

                RC_VAR(a, A(short(1))); (void)a;
                RC_VAR(b, A(long(2))); (void)b;
                RC_VAR(c, A(int(3))); (void)c;
                RC_YIELD();
            });

            { // Finish reason and yield index validation.
                // A minimal test, because we already use `load_raw_bytes_UNSAFE` under the hood.

                Expect ex("");

                decltype(x) y(rcoro::rewind);
                y();

                THROWS("finish reason is out of range", y.load(rcoro::finish_reason(-1), 0, [](auto, auto){}));
                ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset);
            }

            { // Successful loads.
                { // Yield point with variables.
                    Expect ex(R"(
                        ... 1
                        A<short>::A(10)
                        ... 2
                        A<long>::A(20)
                        ... 3
                        A<int>::A(30)
                        ... done
                        A<int>::~A(30)
                        A<long>::~A(20)
                        A<short>::~A(10)
                    )");

                    decltype(x) y;
                    ASSERT(y.load({}, 2, [](auto index, auto construct)
                    {
                        *test_detail::a_log += "... " + std::to_string(index.value) + '\n';

                        if constexpr (index.value == 1)
                            construct(short(10));
                        else if constexpr (index.value == 2)
                            construct(long(20));
                        else if constexpr (index.value == 3)
                            construct(int(30));
                        else
                            FAIL("Wrong variable index.");
                    }));
                    *test_detail::a_log += "... done\n";

                    ASSERT(!y.finished() && y.yield_point() == 2);
                    ASSERT(short(y.var<"a">()) == 10);
                    ASSERT(long(y.var<"b">()) == 20);
                    ASSERT(int(y.var<"c">()) == 30);
                }

                { // At initial yield point.
                    Expect ex("");
                    decltype(x) y;
                    ASSERT(y.load({}, 0, [](auto, auto)
                    {
                        FAIL("This shouldn't be called.");
                    }));

                    ASSERT(!y.finished() && y.yield_point() == 0);
                }

                { // Finished.
                    Expect ex("");
                    decltype(x) y;
                    y.load(rcoro::finish_reason::exception, 0, [](auto, auto)
                    {
                        FAIL("This shouldn't be called.");
                    });

                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::exception && y.yield_point() == 0);
                }

                { // Coroutine without variables.
                    auto z = RCORO(RC_YIELD(););

                    Expect ex("");
                    ASSERT(z.load(rcoro::finish_reason::not_finished, 1, [](auto, auto)
                    {
                        FAIL("This shouldn't be called.");
                    }));

                    ASSERT(!z.finished() && z.yield_point() == 1);
                }
            }

            { // Failures.
                { // Manual abort.
                    Expect ex(R"(
                        A<short>::A(10)
                        A<long>::A(20)
                        aborting!
                        A<long>::~A(20)
                        A<short>::~A(10)
                        ... done
                    )");

                    decltype(x) y;
                    ASSERT(!y.load({}, 2, [](auto index, auto construct)
                    {
                        if constexpr (index.value == 1)
                            construct(short(10));
                        else if constexpr (index.value == 2)
                            construct(long(20));
                        else if constexpr (index.value == 3)
                            *test_detail::a_log += "aborting!\n"; // Not calling `construct` to abort.
                        else
                            FAIL("Wrong variable index.");
                    }));
                    *test_detail::a_log += "... done\n";
                }

                { // Exception.
                    Expect ex(R"(
                        A<short>::A(10)
                        A<long>::A(20)
                        throw!
                        A<long>::~A(20)
                        A<short>::~A(10)
                        ... done
                    )");

                    decltype(x) y;
                    THROWS("test!", !y.load({}, 2, [](auto index, auto construct)
                    {
                        if constexpr (index.value == 1)
                            construct(short(10));
                        else if constexpr (index.value == 2)
                            construct(long(20));
                        else if constexpr (index.value == 3)
                        {
                            *test_detail::a_log += "throw!\n";
                            throw std::runtime_error("test!");
                        }
                        else
                            FAIL("Wrong variable index.");
                    }));
                    *test_detail::a_log += "... done\n";
                }

                { // Exception after calling `construct()`.
                    Expect ex(R"(
                        A<short>::A(10)
                        A<long>::A(20)
                        throw!
                        A<long>::~A(20)
                        A<short>::~A(10)
                        ... done
                    )");

                    decltype(x) y;
                    THROWS("test!", !y.load({}, 2, [](auto index, auto construct)
                    {
                        if constexpr (index.value == 1)
                            construct(short(10));
                        else if constexpr (index.value == 2)
                        {
                            construct(long(20));
                            *test_detail::a_log += "throw!\n";
                            throw std::runtime_error("test!");
                        }
                        else
                            FAIL("Wrong variable index.");
                    }));
                    *test_detail::a_log += "... done\n";
                }

                { // Calling `construct()` twice.
                    Expect ex(R"(
                        A<short>::A(10)
                        A<long>::A(20)
                        calling again...
                        A<long>::~A(20)
                        A<short>::~A(10)
                        ... done
                    )");

                    decltype(x) y;
                    THROWS("at most once", !y.load({}, 2, [](auto index, auto construct)
                    {
                        if constexpr (index.value == 1)
                            construct(short(10));
                        else if constexpr (index.value == 2)
                        {
                            construct(long(20));
                            *test_detail::a_log += "calling again...\n";
                            construct(long(200));
                        }
                        else
                            FAIL("Wrong variable index.");
                    }));
                    *test_detail::a_log += "... done\n";
                }
            }
        }

        { // `load_unordered`.
            auto x = RCORO({
                RC_YIELD();

                {
                    RC_VAR(unused, A(0)); (void)unused; // Skip index 0, this helps catch bugs.
                }

                RC_VAR(aa, A(short(1))); (void)aa;
                RC_VAR(bb, A(long(2))); (void)bb;
                RC_VAR(cc, A(int(3))); (void)cc;
                RC_YIELD();
            });

            { // Finish reason and yield index validation.
                // A minimal test, because we already use `load_raw_bytes_UNSAFE` under the hood.

                Expect ex("");

                decltype(x) y(rcoro::rewind);
                y();

                THROWS("finish reason is out of range", y.load_unordered(rcoro::finish_reason(-1), 0,
                    [](auto){FAIL("This shouldn't be called."); return true;},
                    [](auto, auto){FAIL("This shouldn't be called.");}
                ));
                ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset);
            }

            { // Successful loads.
                { // Yield point with variables.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        ... 3
                        A<int>::A(30)
                        ... done
                        A<int>::~A(30)
                        A<long>::~A(20)
                        A<short>::~A(10)
                    )");

                    decltype(x) y;
                    ASSERT(y.load_unordered({}, 2,
                        [](auto var)
                        {
                            // Can't use the correct variable types here, because this lambda must work for every yield point.
                            var(2, 20, "A<long>");
                            var(1, 10, "A<short>");
                            var(3, 30, "A<int>");
                            return true;
                        },
                        [](auto var_index, auto construct, auto value, std::string_view type_name)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            ASSERT(type_name == test_detail::get_type_name<var_type>());
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(!y.finished() && y.yield_point() == 2);
                    ASSERT(short(y.var<"aa">()) == 10);
                    ASSERT(long(y.var<"bb">()) == 20);
                    ASSERT(int(y.var<"cc">()) == 30);
                    *test_detail::a_log += "... done\n";
                }

                { // An initial yield point.
                    Expect ex("");

                    decltype(x) y;
                    ASSERT(y.load_unordered({}, 0,
                        [](auto)
                        {
                            return true;
                        },
                        [](auto, auto)
                        {
                            FAIL("This shouldn't be called.");
                        }
                    ));
                    ASSERT(!y.finished() && y.yield_point() == 0);
                }

                { // Finished.
                    Expect ex("");

                    decltype(x) y;
                    ASSERT(y.load_unordered(rcoro::finish_reason::exception, 0,
                        [](auto)
                        {
                            return true;
                        },
                        [](auto, auto)
                        {
                            FAIL("This shouldn't be called.");
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::exception && y.yield_point() == 0);
                }

                { // Coroutine without variables.
                    auto z = RCORO(RC_YIELD(););

                    Expect ex("");
                    ASSERT(z.load_unordered(rcoro::finish_reason::not_finished, 1,
                        [](auto)
                        {
                            return true;
                        },
                        [](auto, auto)
                        {
                            FAIL("This shouldn't be called.");
                        }
                    ));

                    ASSERT(!z.finished() && z.yield_point() == 1);
                }

                { // Coroutine with duplicate variable names, but unique per yield.
                    auto y = RCORO({
                        {
                            RC_VAR(unused, 0); // Skip index 0 to catch more bugs.
                            (void)unused;
                        }

                        RC_VAR(a, 1);
                        (void)a;

                        RC_FOR((i, 0); i < 3; i++)
                        {
                            (void)i;
                            RC_YIELD();
                        }

                        RC_FOR((i, 0); i < 5; i++)
                        {
                            (void)i;
                            RC_YIELD();
                        }
                    });

                    static_assert(rcoro::var_names_are_unique_per_yield<decltype(y)>);

                    for (int yield_index : {1, 2})
                    {
                        ASSERT(y.load_unordered({}, yield_index,
                            [&](auto var)
                            {
                                var(rcoro::var_index_at_yield<decltype(y)>(yield_index, "a"), 10);
                                var(rcoro::var_index_at_yield<decltype(y)>(yield_index, "i"), 20);
                                return true;
                            },
                            [](auto var_index, auto construct, int value)
                            {
                                (void)var_index;
                                construct(value);
                            }
                        ));

                        ASSERT(!y.finished() && y.yield_point() == yield_index);
                        ASSERT(!y.var_exists<"unused">() && y.var_exists<"a">() && y.var_exists<2>() == (yield_index == 1) && y.var_exists<3>() == (yield_index == 2));
                        ASSERT(y.var<"a">() == 10 && (yield_index == 1 ? y.var<2>() : y.var<3>()) == 20);
                    }
                }
            }

            { // Failures.
                { // Manual abort.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        aborting!
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    ASSERT(!y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            *test_detail::a_log += "aborting!\n";
                            return false;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }

                { // Excepion.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        throw!
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    THROWS("test!", y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            *test_detail::a_log += "throw!\n";
                            throw std::runtime_error("test!");
                            return true;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }

                { // Missing variables.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        stopping!
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    THROWS("variables are missing", y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            *test_detail::a_log += "stopping!\n";
                            return true;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }

                { // Calling construct twice.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        again!
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    THROWS("already loaded", y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            return true;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                            if constexpr (var_index.value == 1)
                            {
                                *test_detail::a_log += "again!\n";
                                construct(typename var_type::type(value));
                            }
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }

                { // Invalid variable index: too small.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        and...
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    THROWS("out of range", y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            *test_detail::a_log += "and...\n";
                            var(-1, 10);
                            return true;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }

                { // Invalid variable index: too large.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        and...
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    THROWS("out of range", y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            *test_detail::a_log += "and...\n";
                            var(rcoro::num_vars<decltype(x)>, 10);
                            return true;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }

                { // Invalid variable index: not for this yield point.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        and...
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    THROWS("doesn't exist at this yield point", y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            *test_detail::a_log += "and...\n";
                            var(0, 10);
                            return true;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }

                { // Invalid variable index: already loaded.
                    Expect ex(R"(
                        ... 2
                        A<long>::A(20)
                        ... 1
                        A<short>::A(10)
                        and...
                        A<short>::~A(10)
                        A<long>::~A(20)
                    )");

                    decltype(x) y;
                    THROWS("already loaded", y.load_unordered({}, 2,
                        [](auto var)
                        {
                            var(2, 20);
                            var(1, 10);
                            *test_detail::a_log += "and...\n";
                            var(1, 10);
                            return true;
                        },
                        [](auto var_index, auto construct, auto value)
                        {
                            *test_detail::a_log += "... " + std::to_string(var_index.value) + '\n';
                            using var_type = rcoro::var_type<decltype(x), var_index.value>;
                            construct(typename var_type::type(value));
                        }
                    ));
                    ASSERT(y.finished() && y.finish_reason() == rcoro::finish_reason::reset && y.yield_point() == 0);
                }
            }
        }
    }

    { // Type erasure.
        { // `view`.
            auto x = RCORO((int a, int &b, int &&c)
            {
                b = a + c;
            });

            rcoro::view<int, int &, int &&> y = x;
            y = std::move(x); // Test that rvalues work too.

            static_assert(std::is_same_v<decltype(          y .reset()), decltype(y) & >);
            static_assert(std::is_same_v<decltype(std::move(y).reset()), decltype(y) &&>);
            static_assert(std::is_same_v<decltype(          y .rewind()), decltype(y) & >);
            static_assert(std::is_same_v<decltype(std::move(y).rewind()), decltype(y) &&>);
            static_assert(std::is_same_v<decltype(          y (1, std::declval<int &>(), 2)), decltype(y) & >);
            static_assert(std::is_same_v<decltype(std::move(y)(1, std::declval<int &>(), 2)), decltype(y) &&>);

            ASSERT(y && !y.finished() && y.finish_reason() == rcoro::finish_reason::not_finished);
            int result = 0;
            ASSERT(!y(2, result, 3));
            ASSERT(result == 5);
            ASSERT(!y && y.finished() && y.finish_reason() == rcoro::finish_reason::success);

            y.rewind();
            ASSERT(y && !y.finished() && y.finish_reason() == rcoro::finish_reason::not_finished);

            y.reset();
            ASSERT(!y && y.finished() && y.finish_reason() == rcoro::finish_reason::reset);

            y = nullptr;
            ASSERT(!y && y.finished() && y.finish_reason() == rcoro::finish_reason::null);
            y.reset(); // Allowed.
            std::move(y).reset(); // Allowed.
            THROWS("null", y.rewind());
            THROWS("null", std::move(y).rewind());
            THROWS("null", y(2, result, 3));
            THROWS("null", std::move(y)(2, result, 3));
        }

        { // `any_noncopyable`
            Expect ex(R"(
                A<int>::A(0)                # i=0
                A<int>::A(1)                # i=1
                A<int>::operator=(A && = 1) # ^
                A<int>::~A(-1)              # ^
                ... wrap
                A<int>::A(const A & = 1)
                ... call wrapper
                A<int>::A(2)
                A<int>::operator=(A && = 2)
                A<int>::~A(-2)
                ... reassign wrapper
                A<int>::A(A && = 1) # copy into the by-value parameter
                A<int>::~A(-1)      # destroy source object
                A<int>::~A(2)       # destroy old value
                ... call wrapper
                A<int>::A(2)
                A<int>::operator=(A && = 2)
                A<int>::~A(-2)
                ... done
                A<int>::~A(2) # Wrapper dies. The original is already moved-from.
            )");

            auto x = RCORO((int *out = nullptr)
            {
                RC_FOR((i, A(0)); int(i) < 5; i = int(i) + 1)
                {
                    if (out)
                        *out = int(i) * 10;
                    RC_YIELD();
                }
            });

            x()();

            *test_detail::a_log += "... wrap\n";
            rcoro::any_noncopyable<int *> y = x;
            ASSERT(y && !y.busy() && !y.finished() && y.finish_reason() == rcoro::finish_reason::not_finished);
            *test_detail::a_log += "... call wrapper\n";
            int result = 0;
            y(&result);
            ASSERT(result == 20);
            *test_detail::a_log += "... reassign wrapper\n";
            y = std::move(x);
            *test_detail::a_log += "... call wrapper\n";
            result = 0;
            y(&result);
            ASSERT(result == 20);
            *test_detail::a_log += "... done\n";

            { // Null wrapper.
                rcoro::any_noncopyable<> z;
                ASSERT(!z && !z.busy() && z.finished() && z.finish_reason() == rcoro::finish_reason::null);
                z.reset(); // No-op.
                std::move(z).reset(); // No-op.
                ASSERT(!z && !z.busy() && z.finished() && z.finish_reason() == rcoro::finish_reason::null);
                THROWS("null", z.rewind());
                THROWS("null", std::move(z).rewind());
                THROWS("null", z());
                THROWS("null", std::move(z)());
            }
        }
    }

    std::cout << "OK\n";
}
