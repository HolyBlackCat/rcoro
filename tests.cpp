#include <rcoro.hpp>

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
    A(T new_value)
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

    explicit operator T() noexcept
    {
        test_detail::check_instance<T>(this, false);
        return value;
    }
};

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
    // * __restrict on variable refs?
    // * __builtin_assume on jump_to?
    // * remove condition on ::new by moving it before the label?

    // * Deserialization from string.
    // * Passing parameters.
    // * Serialization-deserialization tests.
    // * Strip `constexpr` that will never happen?
    // * Constexpr tests. (mostly `constinit`-ness?)
    // * Transition away from `::tag`, accept `coro<T>` directly?
    // * CI
    // * Test that we include all necessary headers.
    // * Test MSVC.
    // * Possible improvements:
    //   * Optimized assignments between the same yield points? (use assignment instead of reconstruction)
    //   * Noexcept checks and other stuff should ignore variables that aren't visible on any yield points.

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
            static_assert(rcoro::yields_names_are_unique<tag>);
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
            static_assert(rcoro::yields_names_are_unique<tag>);
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
                static_assert(!rcoro::yields_names_are_unique<tag>);
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

        { // `frame_is_trivially_copyable`
            { // Empty coroutine.
                auto x = RCORO();
                static_assert(rcoro::frame_is_trivially_copyable<decltype(x)::tag>);
            }

            { // Trivially copyable variable.
                auto x = RCORO(RC_VAR(a, int(42)); (void)a; RC_YIELD(););
                static_assert(rcoro::frame_is_trivially_copyable<decltype(x)::tag>);
            }

            { // Non-trivially copyable variable.
                auto x = RCORO(RC_VAR(a, A<int>(42)); (void)a; RC_YIELD(););
                static_assert(!rcoro::frame_is_trivially_copyable<decltype(x)::tag>);
            }

            { // Non-trivially copyable variable, but not visible at any yield points.
                auto x = RCORO(RC_VAR(a, A<int>(42)); (void)a;);
                // Currently those are taken into account, but we can change it later.
                static_assert(!rcoro::frame_is_trivially_copyable<decltype(x)::tag>);
            }
        }
    }

    { // A simple coroutine.
        auto x = RCORO({
            // 1,2,3;
            {
                RC_VAR(a, A(10));
                (void)a;
            }
            {
                RC_VAR(b, A(20));
                (void)b;
                RC_YIELD("f");
            }

            RC_FOR((c, A(char{})); char(c) < 3; c = char(c)+1)
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
        constexpr const char *kind_names[] = {"start", "yield", "exception"};
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
            constexpr ops current_ops = current_ops_param.value;
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

    std::cout << "OK\n";
}
