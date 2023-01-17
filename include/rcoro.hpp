// Copyright (c) 2022 Egor Mikhailov

// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.

// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:

// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#ifndef DETAIL_RCORO_HPP_
#define DETAIL_RCORO_HPP_

#include <algorithm>
#include <array>
#include <cstddef>
#include <exception>
#include <iosfwd>
#include <memory>
#include <new>
#include <stdexcept>
#include <string_view>
#include <string>
#include <type_traits>
#include <utility>

#ifndef MACRO_SEQUENCE_FOR_H_ // Make this godbolt-friendly.
#if __has_include("macro_sequence_for.h")
#include "macro_sequence_for.h"
#else
#error "Didn't find "macro_sequence_for.h". Install it from https://github.com/HolyBlackCat/macro_sequence_for"
#endif
#endif

// The version number: `major*10000 + minor*100 + patch`.
#define RCORO_VERSION 100

// An assertion macro. If not customized, uses the standard `assert()`.
#ifndef RCORO_ASSERT
#include <cassert>
#define RCORO_ASSERT(expr) assert(expr)
#endif

// Aborts the program.
#ifndef RCORO_ABORT
#define RCORO_ABORT(text) do {assert(false && text); std::terminate();} while(false)
#endif

// 'Restrict' qualifier.
#ifndef RCORO_RESTRICT
#define RCORO_RESTRICT __restrict // There's also `__restrict__`, but that doesn't work on MSVC.
#endif

// An assumption.
#ifndef RCORO_ASSUME
#if defined(__clang__)
#define RCORO_ASSUME(...) __builtin_assume(__VA_ARGS__)
#elif defined(_MSC_VER)
#define RCORO_ASSUME(...) __assume(__VA_ARGS__)
#else
#define RCORO_ASSUME(...) (__VA_ARGS__ ? void() : __builtin_unreachable())
#endif
#endif

// Silences GCC's silly `-Wnon-template-friend` warning.
#ifndef RCORO_TEMPLATE_FRIEND
#if defined(__GNUC__) && !defined(__clang__)
#define RCORO_TEMPLATE_FRIEND(...) \
    _Pragma("GCC diagnostic push") \
    _Pragma("GCC diagnostic ignored \"-Wnon-template-friend\"") \
    __VA_ARGS__ \
    _Pragma("GCC diagnostic pop")
#else
#define RCORO_TEMPLATE_FRIEND(...) __VA_ARGS__
#endif
#endif

namespace rcoro
{
    // A classical constexpr string.
    template <std::size_t N>
    struct const_string
    {
        char value[N];

        constexpr const_string(const char (&new_value)[N])
        {
            std::copy_n(new_value, N, value);
        }

        [[nodiscard]] constexpr std::string_view view() const
        {
            return std::string_view(value, N-1);
        }
    };

    // Some string-to-int functions below return those.
    constexpr int unknown_name = -1;
    constexpr int ambiguous_name = -2;

    // Why a coroutine finished executing.
    enum class finish_reason
    {
        // Don't reorder the first two, this allows casting booleans to `finish_reason`.
        not_finished = 0, // We're not actually finsihed.
        reset = 1, // The default finish reason for any coroutine that didn't finish naturally.
        success, // Finished normally.
        exception, // Finished because of an exception.
        _count [[maybe_unused]],
        null = _count, // Some wrappers report this when they're null. Since this is not a valid value elsewhere, it's `>= _count`.
        _extended_count [[maybe_unused]],
    };

    namespace detail
    {
        // The template argument of `specific_coro<T>` must satisfy this.
        template <typename T>
        concept ValidCoroTypes = requires
        {
            typename T::_rcoro_marker_t;
            typename T::_rcoro_frame_t;
            typename T::_rcoro_lambda_t;
        };
    }

    // See below.
    template <detail::ValidCoroTypes T>
    class specific_coro;

    namespace detail
    {
        // Returns an invalid reference.
        template <typename T>
        constexpr T &bad_ref()
        {
            #ifdef __clang__
            #pragma clang diagnostic push
            #pragma clang diagnostic ignored "-Wnull-dereference"
            #endif
            // If sanitizers complain about this, we'll have to come up with something else.
            return *(T *)sizeof(T);
            #ifdef __clang__
            #pragma clang diagnostic pop
            #endif
        }

        // Coroutine state.
        enum class State : std::underlying_type_t<finish_reason>
        {
            // Copy members from `finish_reason`.
            not_finished = std::underlying_type_t<finish_reason>(finish_reason::not_finished),
            reset        = std::underlying_type_t<finish_reason>(finish_reason::reset),
            success      = std::underlying_type_t<finish_reason>(finish_reason::success),
            exception    = std::underlying_type_t<finish_reason>(finish_reason::exception),
            // Our own members, for busy tasks.
            _busy = std::underlying_type_t<finish_reason>(finish_reason::_extended_count),
            running = _busy, // Currently running.
            pausing, // In the process of being paused.
        };

        // `specific_coro` adds this as a friend.
        struct SpecificCoroFriend
        {
            template <typename T>
            static State get_state(const T &coro)
            {
                return coro.frame.state;
            }
        };

        // A CRTP base for inspecting the `State` enum.
        template <typename Derived>
        class BasicStateInterface
        {
          public:
            // Examining coroutine state:

            // Each coroutine can be either finished or not finished.
            // If finished, it'll have a `finish_reason()` set, and will have `yield_point() == 0`.
            // If not finished, it can either be `busy()` (currently executing) or not, and will have `yield_point() >= 0 && yield_point() < num_yields<T>`.
            // Yield point 0 is special, it's automatically inserted at the beginning of a coroutine.

            // Same as `!finished()`.
            [[nodiscard]] explicit constexpr operator bool() const noexcept {return !finished();}

            // Returns true if the coroutine is currently running and can't be manipulated.
            [[nodiscard]] constexpr bool busy() const noexcept
            {
                return static_cast<const Derived *>(this)->_state_interface_enum() >= detail::State::_busy;
            }

            // Returns true if the coroutine has finished executing.
            [[nodiscard]] constexpr bool finished() const noexcept
            {
                return !busy() && static_cast<const Derived *>(this)->_state_interface_enum() != detail::State::not_finished;
            }

            // Returns the reason why the coroutine has finished executing, or `not_finished` if not actually finished.
            [[nodiscard]] constexpr rcoro::finish_reason finish_reason() const noexcept
            {
                return finished() ? rcoro::finish_reason(static_cast<const Derived *>(this)->_state_interface_enum()) : finish_reason::not_finished;
            }

            // Returns true if this coroutine can be resumed.
            // Same as `!finished() && !busy()`.
            [[nodiscard]] constexpr bool can_resume() const noexcept
            {
                return static_cast<const Derived *>(this)->_state_interface_enum() == detail::State::not_finished;
            }
        };

        // Get the template parameter of a `specific_coro<T>`.
        template <typename T> struct GetMarkerHelper {};
        template <typename T> struct GetMarkerHelper<specific_coro<T>> {using type = typename T::_rcoro_marker_t;};
        template <typename T> using GetMarker = typename GetMarkerHelper<T>::type;

        // Wraps a value into a type.
        template <auto N>
        struct ValueTag {static constexpr auto value = N;};

        // A constexpr for loop.
        template <auto N, typename F>
        constexpr void const_for(F &&func)
        {
            [&]<decltype(N) ...I>(std::integer_sequence<decltype(N), I...>){
                (void(func(std::integral_constant<decltype(N), I>{})), ...);
            }(std::make_integer_sequence<decltype(N), N>{});
        }
        template <auto N, typename F>
        constexpr void const_reverse_for(F &&func)
        {
            [&]<decltype(N) ...I>(std::integer_sequence<decltype(N), I...>){
                (void(func(std::integral_constant<decltype(N), N-I-1>{})), ...);
            }(std::make_integer_sequence<decltype(N), N>{});
        }
        template <auto N, typename F>
        constexpr bool const_any_of(F &&func)
        {
            return [&]<decltype(N) ...I>(std::integer_sequence<decltype(N), I...>){
                return (func(std::integral_constant<decltype(N), I>{}) || ...);
            }(std::make_integer_sequence<decltype(N), N>{});
        }

        // An internal helper for `with_const_index`.
        template <auto I, typename F>
        constexpr void with_const_index_helper(F &&func) {std::forward<F>(func)(std::integral_constant<decltype(I), I>{});}

        // Transforms a runtime index to a compile-time one. UB if out of bounds.
        // `func` is `void func(int i)`. It's called with the current yield point index, if any.
        template <auto N, typename F>
        constexpr void with_const_index(decltype(N) i, F &&func)
        {
            RCORO_ASSERT(i >= 0 && i < N);
            if constexpr (N > 0)
            {
                constexpr auto arr = []<decltype(N) ...I>(std::integer_sequence<decltype(N), I...>){
                    return std::array<void (*)(F &&), N>{with_const_index_helper<I, F>...};
                }(std::make_integer_sequence<decltype(N), N>{});
                arr[i](std::forward<F>(func));
            }
        }

        // A type list.
        template <typename ...P>
        struct TypeList {static constexpr int size = sizeof...(P);};
        // A helper that strips the leading `void,` from a list. This simplifies the macros.
        template <typename Void, typename ...P> requires std::is_same_v<Void, void>
        using TypeListSkipVoid = TypeList<P...>;
        // Get `N`th type in the list.
        template <int N, typename T>
        struct TypeAt {};
        template <int N, typename P0, typename ...P>
        struct TypeAt<N, TypeList<P0, P...>> : TypeAt<N-1, TypeList<P...>> {};
        template <typename P0, typename ...P>
        struct TypeAt<0, TypeList<P0, P...>> {using type = P0;};

        // Returns the number of variables in a coroutine.
        // Here and everywhere, `Raw` means that the variable index includes unused variables.
        template <typename T>
        struct NumVarsWithUnused : std::integral_constant<int, T::_rcoro_vars::size> {};

        // Stateful trick to store which variables are actually used.
        template <typename T, int N>
        struct RawVarUsedReader
        {
            RCORO_TEMPLATE_FRIEND(
            friend constexpr bool _adl_detail_rcoro_var_used(RawVarUsedReader<T, N>);
            )
        };
        template <typename T, int N>
        struct RawVarUsedWriter
        {
            friend constexpr bool _adl_detail_rcoro_var_used(RawVarUsedReader<T, N>) {return true;}
        };
        constexpr void _adl_detail_rcoro_var_used() {} // Dummy ADL target.
        template <typename T, int N, typename = void>
        struct RawVarUsed : std::false_type {};
        template <typename T, int N>
        struct RawVarUsed<T, N, std::enable_if_t<_adl_detail_rcoro_var_used(RawVarUsedReader<T, N>{})>> : std::true_type {};
        // Mark multiple variables as used.
        template <typename T, int I, bool ...B>
        struct RawVarUsedMultiWriter {};
        template <typename T, int I, bool ...B>
        struct RawVarUsedMultiWriter<T, I, true, B...> : RawVarUsedWriter<T, I>, RawVarUsedMultiWriter<T, I+1, B...> {};
        template <typename T, int I, bool ...B>
        struct RawVarUsedMultiWriter<T, I, false, B...> : RawVarUsedMultiWriter<T, I+1, B...> {};
        // Count used variables.
        template <typename T>
        struct NumVarsDense : std::integral_constant<int, []<int ...I>(std::integer_sequence<int, I...>){
            return (RawVarUsed<T, I>::value + ... + 0);
        }(std::make_integer_sequence<int, NumVarsWithUnused<T>::value>{})> {};
        // Sparse to dense variable indices. Dense indices ignore unused variables.
        template <typename T>
        constexpr int pack_var_index(int value)
        {
            RCORO_ASSERT(value >= 0 && value < NumVarsWithUnused<T>::value);
            if constexpr (NumVarsWithUnused<T>::value == 0)
            {
                return 0;
            }
            else
            {
                constexpr auto mapping = []<int ...I>(std::integer_sequence<int, I...>){
                    int i = 0;
                    return std::array{(RawVarUsed<T, I>::value ? i++ : -1)...};
                }(std::make_integer_sequence<int, NumVarsWithUnused<T>::value>{});
                int ret = mapping[value];
                RCORO_ASSERT(ret >= 0 && "Internal error: this variable is unused, it doesn't have a packed index.");
                return ret;
            }
        }
        // Dense to sparse variable indices.
        template <typename T>
        constexpr int unpack_var_index(int value)
        {
            RCORO_ASSERT(value >= 0 && value < NumVarsDense<T>::value);
            if constexpr (NumVarsDense<T>::value == 0)
            {
                return 0;
            }
            else
            {
                constexpr auto mapping = []<int ...I>(std::integer_sequence<int, I...>){
                    std::array<int, NumVarsDense<T>::value> ret{};
                    int i = 0;
                    ((RawVarUsed<T, I>::value ? void(ret[i++] = I) : void()), ...);
                    return ret;
                }(std::make_integer_sequence<int, NumVarsWithUnused<T>::value>{});
                return mapping[value];
            }
        }

        // Returns the name of a single variable. `::value` is a `const_string`.
        template <typename T, int N>
        using VarName = typename TypeAt<unpack_var_index<T>(N), typename T::_rcoro_vars>::type;

        // Returns the number of yield points in a coroutine.
        template <typename T>
        struct NumYields : std::integral_constant<int, T::_rcoro_yields::size> {};
        // Returns the name of a single yield point. `::value` is a `const_strict`.
        template <typename T, int N>
        using YieldName = typename TypeAt<N, typename T::_rcoro_yields>::type;

        // Stateful trick to store variable types.
        // This is the only stateful storage that performs writes in both fake and non-fake instantiations,
        // because the two types will be different if the type is defined locally (a lambda, a coroutine, or just a locally-defined class/enum),
        // and we need both (fake for computing the variable offsets, and non-fake for everything else).
        template <bool Fake, typename T, int N>
        struct RawVarTypeReader
        {
            RCORO_TEMPLATE_FRIEND(
            friend constexpr auto _adl_detail_rcoro_var_type(RawVarTypeReader<Fake, T, N>);
            )
        };
        constexpr void _adl_detail_rcoro_var_type() {} // Dummy ADL target.
        template <bool Fake, typename T, int N>
        using MaybeTentativeRawVarType = std::remove_pointer_t<decltype(_adl_detail_rcoro_var_type(RawVarTypeReader<Fake, T, N>{}))>;
        template <typename T, int N>
        using RawVarType = MaybeTentativeRawVarType<false, T, N>;
        template <typename T, int N>
        using VarType = RawVarType<T, unpack_var_index<T>(N)>;
        template <typename T, int N>
        using TentativeRawVarType = MaybeTentativeRawVarType<true, T, N>;
        template <typename T, int N>
        using TentativeVarType = TentativeRawVarType<T, unpack_var_index<T>(N)>;
        template <bool Fake, typename T, int N, typename U>
        struct RawVarTypeWriter
        {
            friend constexpr auto _adl_detail_rcoro_var_type(RawVarTypeReader<Fake, T, N>) {return (U *)nullptr;}
        };
        template <typename T, int N, typename U>
        struct RawVarTypeWriter<false, T, N, U>
        {
            // Make sure the new type is sufficiently similar to the tentative one.
            static_assert(sizeof(U) == sizeof(TentativeRawVarType<T, N>) && alignof(U) == alignof(TentativeRawVarType<T, N>) && std::is_empty_v<U> == std::is_empty_v<TentativeRawVarType<T, N>>,
                "You're doing something really stupid with the variable type.");
            friend constexpr auto _adl_detail_rcoro_var_type(RawVarTypeReader<false, T, N>) {return (U *)nullptr;}
        };
        template <bool Fake, typename T, int N>
        struct RawVarTypeHelper
        {
            template <typename U, typename = decltype(void(RawVarTypeWriter<Fake, T, N, std::decay_t<U>>{}))>
            constexpr RawVarTypeHelper(U *) {}
        };

        // Stateful trick to store var-to-var overlap data.
        template <typename T, int N>
        struct RawVarVarReachReader
        {
            RCORO_TEMPLATE_FRIEND(
            friend constexpr auto _adl_detail_rcoro_var_var_reach(RawVarVarReachReader<T, N>);
            )
        };
        template <bool Write, typename T, int N, auto M>
        struct RawVarVarReachWriter
        {
            friend constexpr auto _adl_detail_rcoro_var_var_reach(RawVarVarReachReader<T, N>) {return M;}
        };
        template <typename T, int N, auto M>
        struct RawVarVarReachWriter<false, T, N, M> {};
        constexpr void _adl_detail_rcoro_var_var_reach() {} // Dummy ADL target.
        template <typename T, int A, int B>
        struct RawVarVarReach : std::bool_constant<_adl_detail_rcoro_var_var_reach(RawVarVarReachReader<T, A>{})[B]> {};
        template <typename T, int A>
        struct RawVarVarReach<T, A, A> : std::true_type {};
        template <typename T, int A, int B> requires(A < B)
        struct RawVarVarReach<T, A, B> : RawVarVarReach<T, B, A> {};

        // Stateful trick to store var-to-yield overlap data.
        template <typename T, int N>
        struct RawVarYieldReachReader
        {
            RCORO_TEMPLATE_FRIEND(
            friend constexpr auto _adl_detail_rcoro_var_yield_reach(RawVarYieldReachReader<T, N>);
            )
        };
        template <bool Write, typename T, int N, bool ...I>
        struct RawVarYieldReachWriter : RawVarUsedMultiWriter<T, 0, I...> // We also flag used variables here.
        {
            friend constexpr auto _adl_detail_rcoro_var_yield_reach(RawVarYieldReachReader<T, N>) {return std::array<bool, sizeof...(I)>{I...};}
        };
        template <typename T, int N, bool ...I>
        struct RawVarYieldReachWriter<false, T, N, I...> {};
        constexpr void _adl_detail_rcoro_var_yield_reach() {} // Dummy ADL target.
        template <typename T, int V, int Y>
        struct RawVarYieldReach : std::bool_constant<(
            _adl_detail_rcoro_var_yield_reach(RawVarYieldReachReader<T, Y>{}).size() > V
            && _adl_detail_rcoro_var_yield_reach(RawVarYieldReachReader<T, Y>{})[V]
        )> {};
        // A special case for the 0-th implicit yield point.
        template <typename T, int V>
        struct RawVarYieldReach<T, V, 0> : std::false_type {};
        // An array of all variables reachable from a yield point.
        template <typename T, int Y>
        constexpr auto vars_reachable_from_yield()
        {
            return []<int ...V>(std::integer_sequence<int, V...>){
                std::array<int, (RawVarYieldReach<T, unpack_var_index<T>(V), Y>::value + ... + 0)> ret{};
                int pos = 0;
                ((RawVarYieldReach<T, unpack_var_index<T>(V), Y>::value ? void(ret[pos++] = V) : void()), ...);
                return ret;
            }(std::make_integer_sequence<int, NumVarsDense<T>::value>{});
        }

        // Determines the variable offset in the frame, based on `TentativeVarType`.
        // This uses raw unpacked variable indices.
        template <typename T, int N, int M = N>
        struct RawVarOffset {}; // This variable is unused, refuse to calculate offset.
        template <typename T, int N> requires RawVarUsed<T, N>::value
        struct RawVarOffset<T, N, 0> : std::integral_constant<std::size_t, 0> {}; // There are no overlapping variables, offset is 0.
        template <typename T, int N, int M> requires(RawVarUsed<T, N>::value && M > 0 && !RawVarVarReach<T, N, M-1>::value)
        struct RawVarOffset<T, N, M> : RawVarOffset<T, N, M-1> {}; // `M` doesn't overlap, check the previous variable.
        // Here we don't need to check whether `M-1` is used, it's implied by it being reachable from the usable variable `N`.
        template <typename T, int N, int M> requires(RawVarUsed<T, N>::value && M > 0 && RawVarVarReach<T, N, M-1>::value)
        struct RawVarOffset<T, N, M> : std::integral_constant<std::size_t, // Found an overlapping variable, place this one after it.
            (
                RawVarOffset<T, M-1>::value
                + sizeof(TentativeRawVarType<T, M-1>)
                // Prepare to divide, rounding up.
                + alignof(TentativeRawVarType<T, N>)
                - 1
            )
            / alignof(TentativeRawVarType<T, N>)
            * alignof(TentativeRawVarType<T, N>)>
        {};

        // The stack frame alignment.
        template <typename T>
        struct FrameAlignment : std::integral_constant<std::size_t, []<int ...I>(std::integer_sequence<int, I...>){
            return std::max({alignof(char), alignof(TentativeVarType<T, I>)...});
        }(std::make_integer_sequence<int, NumVarsDense<T>::value>{})> {};
        // The stack frame size.
        template <typename T>
        struct FrameSize : std::integral_constant<std::size_t, 0> {};
        template <typename T> requires(NumVarsDense<T>::value > 0)
        struct FrameSize<T> : std::integral_constant<std::size_t,
            (
                RawVarOffset<T, unpack_var_index<T>(NumVarsDense<T>::value - 1)>::value
                + sizeof(TentativeRawVarType<T, unpack_var_index<T>(NumVarsDense<T>::value - 1)>)
                // Prepare to divide, rounding up.
                + FrameAlignment<T>::value
                - 1
            )
            / FrameAlignment<T>::value
            * FrameAlignment<T>::value>
        {};

        // Inspect copyability of coroutine variables.
        template <typename T>
        struct FrameIsCopyConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_copy_constructible_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVarsDense<T>::value>{})> {};
        template <typename T>
        struct FrameIsMoveConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_move_constructible_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVarsDense<T>::value>{})> {};
        template <typename T>
        struct FrameIsNothrowCopyConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_nothrow_copy_constructible_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVarsDense<T>::value>{})> {};
        template <typename T>
        struct FrameIsNothrowMoveConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_nothrow_move_constructible_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVarsDense<T>::value>{})> {};
        template <typename T>
        struct FrameIsTriviallyCopyable : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_trivially_copyable_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVarsDense<T>::value>{})> {};

        // Stores coroutine variables and other state.
        // If `Fake`, becomes an empty structure.

        // A helper base.
        template <typename T>
        struct FrameBase
        {
            // The storage array is in the base, to get rid of it when it's empty. And also to stop it from being copied.
            // Can't use zero-sized `std::array`, because `std::is_empty` is false for it, so `[[no_unique_address]]` won't help.
            // Zero by default to make it constexpr-constructible, because why not.
            alignas(FrameAlignment<T>::value) char storage_array[FrameSize<T>::value]{};

            constexpr FrameBase() {}

            // Raw frame bytes shouldn't be copied.
            FrameBase(const FrameBase &) = delete;
            FrameBase &operator=(const FrameBase &) = delete;

            constexpr       char *storage()       {return storage_array;}
            constexpr const char *storage() const {return storage_array;}
        };
        template <typename T> requires(FrameSize<T>::value == 0)
        struct FrameBase<T>
        {
            constexpr       char *storage()       {return nullptr;}
            constexpr const char *storage() const {return nullptr;}
        };

        // Stores the coroutine state, including the stack frame.
        template <bool Fake, typename T>
        struct Frame : FrameBase<T>
        {
            using marker_t = T;
            static constexpr bool fake = Fake;

            // The state enum.
            State state = State::reset;
            // The current yield point.
            int pos = 0;

            // Returns true if the variable `V` exists at the current yield point.
            template <int V>
            constexpr bool raw_var_exists() const
            {
                bool ret = false;
                if (pos != 0) // Not strictly necessary, hopefully an optimization.
                {
                    with_const_index<NumYields<T>::value>(pos, [&](auto yieldindex)
                    {
                        ret = RawVarYieldReach<T, V, yieldindex.value>::value;
                    });
                }
                return ret;
            }
            template <int V>
            constexpr bool var_exists() const
            {
                return raw_var_exists<unpack_var_index<T>(V)>();
            }

            // Get `V`th variable storage, as a void pointer. We need a separate version for `void *`,
            // because when this is called, the actual variable type isn't known yet.
            // This uses raw unpacked variable indices.
            // If the variable is unused, returns `fallback` instead.
            template <int V>
            constexpr void *raw_var_storage_for_init(void *fallback)
            {
                if constexpr (RawVarUsed<T, V>::value)
                    return this->storage() + RawVarOffset<T, V>::value;
                else
                    return fallback;
            }
            // Get `V`th variable storage. Not laundered, so don't dereference.
            template <int V> constexpr       VarType<T, V> *var_storage()       {return reinterpret_cast<      VarType<T, V> *>(this->storage() + RawVarOffset<T, unpack_var_index<T>(V)>::value);}
            template <int V> constexpr const VarType<T, V> *var_storage() const {return reinterpret_cast<const VarType<T, V> *>(this->storage() + RawVarOffset<T, unpack_var_index<T>(V)>::value);}

            // Get `V`th variable. UB if it doesn't exist.
            template <int V> constexpr       VarType<T, V> &var()       {return *std::launder(var_storage<V>());}
            template <int V> constexpr const VarType<T, V> &var() const {return *std::launder(var_storage<V>());}

            // Get `V`th variable. An invalid reference if it doesn't exist and if `assume_good` is false.
            // If this variable is optimized out and `assume_good == true`, returns `fallback` instead. Otherwise returns an invalid reference.
            template <int V>
            constexpr RawVarType<T, V> &raw_var_or_bad_ref(bool assume_good, void *fallback)
            {
                if constexpr (RawVarUsed<T, V>::value)
                {
                    constexpr int v_packed = pack_var_index<T>(V);
                    if (assume_good || var_exists<v_packed>())
                        return var<v_packed>();
                    else
                        return bad_ref<RawVarType<T, V>>();
                }
                else
                {
                    if (assume_good)
                        return *std::launder(reinterpret_cast<RawVarType<T, V> *>(fallback));
                    else
                        return bad_ref<RawVarType<T, V>>();
                }
            }

            // Cleans the frame.
            constexpr void reset() noexcept
            {
                if (state >= State::_busy)
                    RCORO_ABORT("Can't reset a busy coroutine.");
                state = State::reset;
                if (pos == 0) // Not strictly necessary, hopefully an optimization.
                    return;
                with_const_index<NumYields<T>::value>(pos, [&](auto yieldindex)
                {
                    constexpr auto num_indices = vars_reachable_from_yield<T, yieldindex.value>().size();
                    const_reverse_for<num_indices>([&](auto varindex)
                    {
                        // MSVC doesn't see this variable if we declare it outside of the lambda,
                        // so we either have to duplicate the declaration, or call the helper function twice.
                        constexpr auto indices = vars_reachable_from_yield<T, yieldindex.value>();
                        std::destroy_at(&var<indices[varindex.value]>());
                    });
                });
                pos = 0;
            }

            // A helper for `handle_vars()`.
            // Can't define it inside of that function because of Clang bug: https://github.com/llvm/llvm-project/issues/59734
            template <typename F, int Y>
            struct HandleVarsGuard
            {
                F &rollback;
                std::size_t i = 0;
                bool failed = true;
                ~HandleVarsGuard()
                {
                    if (!failed)
                        return;

                    constexpr auto indices = vars_reachable_from_yield<T, Y>();
                    const_reverse_for<indices.size()>([&](auto var)
                    {
                        if (var.value < i)
                            rollback(std::integral_constant<int, indices[var.value]>{});
                    });
                }
            };

            // `func` is `bool func(auto index, auto completed)`, `rollback` is `void rollback(auto index)`, where `index` is `std::integral_constant<int, I>`.
            // `func` is called for every variable index for yield point `pos`.
            // If it throws or returns true, `rollback` is called for every index processed so far, in reverse.
            // If `func` returned true, the whole function also returns true.
            // `completed` should normally be ignored. It can be called at most once. If you then throw or return true, the current iteration will be rolled back as well.
            template <typename F, typename G>
            static constexpr bool handle_vars(int pos, F &&func, G &&rollback)
            {
                if (pos == 0) // Not strictly necessary, hopefully an optimization.
                    return false;
                bool ret = false;
                with_const_index<NumYields<T>::value>(pos, [&](auto yield)
                {
                    constexpr auto num_indices = vars_reachable_from_yield<T, yield.value>().size();

                    HandleVarsGuard<G, yield.value> guard{.rollback = rollback};

                    bool fail = const_any_of<num_indices>([&](auto var)
                    {
                        constexpr auto indices = vars_reachable_from_yield<T, yield.value>();

                        bool already_incremented = false;
                        bool stop = func(std::integral_constant<int, indices[var.value]>{}, [&]{
                            if (already_incremented)
                                return;
                            already_incremented = true;
                            guard.i++;
                        });
                        if (!stop && !already_incremented)
                            guard.i++;
                        return stop;
                    });
                    ret = guard.failed = fail;
                });
                return ret;
            }

            constexpr Frame() {}

            constexpr Frame(const Frame &other) noexcept(FrameIsNothrowCopyConstructible<T>::value) requires FrameIsCopyConstructible<T>::value
                : FrameBase<T>() // Sic, don't want to copy the frame. Specifying the initializer to silence a GCC warning.
            {
                *this = other;
            }
            constexpr Frame(Frame &&other) noexcept(FrameIsNothrowMoveConstructible<T>::value) requires FrameIsMoveConstructible<T>::value
                : FrameBase<T>() // Sic, don't want to copy the frame. Specifying the initializer to silence a GCC warning.
            {
                *this = std::move(other);
            }
            // If the assignment throws, the target will be zeroed.
            constexpr Frame &operator=(const Frame &other) noexcept(FrameIsNothrowCopyConstructible<T>::value) requires FrameIsCopyConstructible<T>::value
            {
                if (&other == this)
                    return *this;
                if (state >= State::_busy || other.state >= State::_busy)
                    RCORO_ABORT("Can't copy a busy coroutine.");
                reset();
                handle_vars(other.pos,
                    [&](auto index, auto)
                    {
                        constexpr int i = index.value;
                        std::construct_at(var_storage<i>(), other.template var<i>()); // GCC 11 needs `template` here.
                        return false;
                    },
                    [&](auto index) noexcept
                    {
                        constexpr int i = index.value;
                        std::destroy_at(&var<i>());
                    }
                );
                pos = other.pos;
                state = other.state;
                return *this;
            }
            // If the assignment throws, the target will be zeroed. The source is zeroed in any case.
            constexpr Frame &operator=(Frame &&other) noexcept(FrameIsNothrowMoveConstructible<T>::value) requires FrameIsMoveConstructible<T>::value
            {
                if (&other == this)
                    return *this;
                if (state >= State::_busy || other.state >= State::_busy)
                    RCORO_ABORT("Can't move a busy coroutine.");

                reset();

                struct Guard
                {
                    Frame &other;
                    constexpr ~Guard()
                    {
                        other.reset();
                    }
                };
                Guard guard{.other = other};

                handle_vars(other.pos,
                    [&](auto index, auto)
                    {
                        constexpr int i = index.value;
                        std::construct_at(var_storage<i>(), std::move(other.template var<i>())); // GCC 11 needs `template` here.
                        return false;
                    },
                    [&](auto index) noexcept
                    {
                        constexpr int i = index.value;
                        std::destroy_at(&var<i>());
                    }
                );
                pos = other.pos;
                state = other.state;
                return *this;
            }
            constexpr ~Frame()
            {
                reset();
            }
        };
        // A fake frame.
        template <typename T>
        struct Frame<true, T>
        {
            static constexpr bool fake = true;

            State state = State::reset;
            int pos = 0;

            template <int V>
            constexpr bool raw_var_exists()
            {
                return false;
            }
            template <int V>
            constexpr void *raw_var_storage_for_init(void *)
            {
                return nullptr;
            }
            template <int V>
            constexpr TentativeRawVarType<T, V> &raw_var_or_bad_ref(bool, void *)
            {
                return bad_ref<TentativeRawVarType<T, V>>();
            }
        };

        // If the variable ends up not being stored in the frame, it's stored here.
        template <bool Fake, typename T, int I>
        struct RawVarFallbackStorage
        {
            // Using the tentative type, because this is used before the final type is determined.
            // This shouldn't matter, since we `static_assert` size and alignment match elsewhere.
            using type = TentativeRawVarType<T, I>;
            alignas(type) char storage[sizeof(type)]; // Not zeroing this, must be trivial initializable for `goto` to agree to jump over us.
            void *ptr() {return storage;}
        };
        template <bool Fake, typename T, int I> requires(Fake || RawVarUsed<T, I>::value)
        struct RawVarFallbackStorage<Fake, T, I>
        {
            void *ptr() {return nullptr;}
        };

        // Destroys a variable when dies.
        template <typename Frame, int I>
        struct RawVarGuard
        {
            using type = RawVarType<typename Frame::marker_t, I>;

            Frame *frame = nullptr;
            type *target = nullptr;

            constexpr RawVarGuard(Frame *frame, void *fallback) : frame(frame)
            {
                if (!frame)
                    return;
                if constexpr (RawVarUsed<typename Frame::marker_t, I>::value)
                    target = &frame->template var<pack_var_index<typename Frame::marker_t>(I)>();
                else
                    target = std::launder(reinterpret_cast<type *>(fallback));
            }

            RawVarGuard(const RawVarGuard &) = delete;
            RawVarGuard &operator=(const RawVarGuard &) = delete;

            constexpr ~RawVarGuard()
            {
                if (target && frame->state != State::pausing)
                    std::destroy_at(target);
            }
        };
        // We need a separate fake version, because `var<I>()` isn't ready yet, because the actual variable type isn't determined yet.
        template <typename Frame, int I> requires Frame::fake
        struct RawVarGuard<Frame, I>
        {
            constexpr RawVarGuard(Frame *, void *) {}
            RawVarGuard(const RawVarGuard &) = delete;
            RawVarGuard &operator=(const RawVarGuard &) = delete;
        };

        // An array of pairs, mapping variable names to their indices.
        template <typename T>
        constexpr auto var_name_to_index_mapping = []{
            std::array<std::pair<std::string_view, int>, NumVarsDense<T>::value> ret{};
            const_for<NumVarsDense<T>::value>([&](auto index)
            {
                constexpr int i = index.value;
                ret[i].first = VarName<T, i>::value.view();
                ret[i].second = i;
            });
            std::sort(ret.begin(), ret.end());
            return ret;
        }();
        template <typename T, int Y>
        constexpr auto var_name_to_index_mapping_per_yield = []{
            constexpr auto indices = vars_reachable_from_yield<T, Y>();
            std::array<std::pair<std::string_view, int>, indices.size()> ret{};
            const_for<indices.size()>([&](auto index)
            {
                // MSVC doesn't see `indices` from above here, so we repeat the definition.
                constexpr auto indices = vars_reachable_from_yield<T, Y>();
                constexpr int i = index.value;
                ret[i].first = VarName<T, indices[i]>::value.view();
                ret[i].second = indices[i];
            });
            std::sort(ret.begin(), ret.end());
            return ret;
        }();
        // Same for yields.
        template <typename T>
        constexpr auto yield_name_to_index_mapping = []{
            std::array<std::pair<std::string_view, int>, NumYields<T>::value> ret{};
            const_for<NumYields<T>::value>([&](auto index)
            {
                constexpr int i = index.value;
                ret[i].first = YieldName<T, i>::value.view();
                ret[i].second = i;
            });
            std::sort(ret.begin(), ret.end());
            return ret;
        }();

        // Getting the type name as string:
        template <typename T>
        constexpr std::string_view raw_type_name()
        {
            #ifndef _MSC_VER
            return __PRETTY_FUNCTION__;
            #else
            return __FUNCSIG__;
            #endif
        }
        struct TypeNameFormat
        {
            std::size_t junk_leading = 0;
            std::size_t junk_total = 0;
        };
        constexpr TypeNameFormat type_name_format = []{
            TypeNameFormat ret;
            std::string_view sample = raw_type_name<int>();
            ret.junk_leading = sample.find("int");
            ret.junk_total = sample.size() - 3;
            return ret;
        }();
        template <typename T>
        constexpr auto type_name_storage = []{
            static_assert(type_name_format.junk_leading != std::size_t(-1), "Unable to determine the type name format on this compiler.");
            std::array<char, raw_type_name<T>().size() - type_name_format.junk_total + 1> ret{};
            std::copy_n(raw_type_name<T>().data() + type_name_format.junk_leading, ret.size() - 1, ret.data());
            return ret;
        }();
        // Returns the type name, as a `std::string_view`. The string is null-terminated, but the terminator is not included in the view.
        template <typename T>
        [[nodiscard]] constexpr std::string_view type_name()
        {
            return {type_name_storage<T>.data(), type_name_storage<T>.size() - 1};
        }

        // Check if `a << b` compiles.
        template <typename Target, typename Stream>
        concept Printable = requires(Stream &s, const Target &t){s << t;};
    }

    // Whether `T` is a `specific_coro<??>`.
    template <typename T>
    concept specific_coro_type = requires{typename detail::GetMarker<T>;};


    // Examining variables:

    // The number of variables in a coroutine.
    template <specific_coro_type T>
    constexpr int num_vars = detail::NumVarsDense<detail::GetMarker<T>>::value;

    // The name of a coroutine variable `V`.
    template <specific_coro_type T, int V> requires(V >= 0 && V < num_vars<T>)
    constexpr const_string var_name_const = detail::VarName<detail::GetMarker<T>, V>::value;

    // The type of a coroutine variable `V`.
    template <specific_coro_type T, int V>
    using var_type = typename detail::VarType<detail::GetMarker<T>, V>;

    // Variable name helpers:

    // Converts a variable name to its index.
    // Returns a negative error code on failure: either `unknown_name` or `ambiguous_name`.
    template <specific_coro_type T>
    [[nodiscard]] constexpr int var_index_or_negative(std::string_view name)
    {
        const auto &arr = detail::var_name_to_index_mapping<detail::GetMarker<T>>;
        auto it = std::partition_point(arr.begin(), arr.end(), [&](const auto &pair){return pair.first < name;});
        if (it == arr.end() || it->first != name)
            return unknown_name;
        auto next_it = it;
        ++next_it;
        if (next_it != arr.end() && next_it->first == name)
            return ambiguous_name;
        return it->second;
    }
    // Same, but for a specific yield point.
    template <specific_coro_type T>
    [[nodiscard]] constexpr int var_index_at_yield_or_negative(int yield_index, std::string_view name)
    {
        int ret = unknown_name;
        detail::with_const_index<detail::NumYields<detail::GetMarker<T>>::value>(yield_index, [&](auto yield_index_const)
        {
            const auto &arr = detail::var_name_to_index_mapping_per_yield<detail::GetMarker<T>, yield_index_const.value>;
            auto it = std::partition_point(arr.begin(), arr.end(), [&](const auto &pair){return pair.first < name;});
            if (it == arr.end() || it->first != name)
            {
                ret = unknown_name;
                return;
            }
            auto next_it = it;
            ++next_it;
            if (next_it != arr.end() && next_it->first == name)
            {
                ret = ambiguous_name;
                return;
            }
            ret = it->second;
        });
        return ret;
    }
    // Same, but throws on failure.
    template <specific_coro_type T>
    [[nodiscard]] constexpr int var_index(std::string_view name)
    {
        int ret = var_index_or_negative<T>(name);
        if (ret == ambiguous_name)
            throw std::runtime_error("Ambiguous coroutine variable name: `" + std::string(name) + "`.");
        if (ret < 0)
            throw std::runtime_error("Unknown coroutine variable name: `" + std::string(name) + "`.");
        return ret;
    }
    template <specific_coro_type T>
    [[nodiscard]] constexpr int var_index_at_yield(int yield_index, std::string_view name)
    {
        int ret = var_index_at_yield_or_negative<T>(yield_index, name);
        if (ret == ambiguous_name)
            throw std::runtime_error("Ambiguous coroutine variable name: `" + std::string(name) + "`.");
        if (ret < 0)
            throw std::runtime_error("Unknown coroutine variable name: `" + std::string(name) + "`.");
        return ret;
    }
    // Same, but works with constexpr strings, and causes a SOFT compilation error if the name is invalid.
    template <specific_coro_type T, const_string Name>
    requires(var_index_or_negative<T>(Name.view()) >= 0)
    constexpr int var_index_const = var_index_or_negative<T>(Name.view());

    // Given a variable index, returns its name. Same as `var_name_const`, but with a possibly dynamic index.
    // Throws if the index is out of range.
    template <specific_coro_type T>
    [[nodiscard]] constexpr std::string_view var_name(int i)
    {
        if (i < 0 || i >= num_vars<T>)
            throw std::runtime_error("Coroutine variable index is out of range.");
        if constexpr (num_vars<T> == 0)
        {
            return {};
        }
        else
        {
            constexpr auto arr = []<int ...I>(std::integer_sequence<int, I...>){
                return std::array{var_name_const<T, I>.view()...};
            }(std::make_integer_sequence<int, num_vars<T>>{});
            return arr[i];
        }
    }

    // Returns true if at every specific yield point, every variable name is unique.
    template <specific_coro_type T>
    constexpr bool var_names_are_unique_per_yield = !detail::const_any_of<detail::NumYields<detail::GetMarker<T>>::value>([](auto yield_index)
    {
        const auto &arr = detail::var_name_to_index_mapping_per_yield<detail::GetMarker<T>, yield_index.value>;
        return std::adjacent_find(arr.begin(), arr.end(), [](const auto &a, const auto &b){return a.first == b.first;}) != arr.end();
    });


    // Examining yield points:

    // The number of yield points.
    template <specific_coro_type T>
    constexpr int num_yields = detail::NumYields<detail::GetMarker<T>>::value;

    // The name of a yield point.
    template <specific_coro_type T, int Y> requires(Y >= 0 && Y < num_yields<T>)
    constexpr const_string yield_name_const = detail::YieldName<detail::GetMarker<T>, Y>::value;

    // A list of variables existing at yield point `Y`, of type `std::array<int, N>`.
    template <specific_coro_type T, int Y>
    constexpr auto yield_vars = detail::vars_reachable_from_yield<detail::GetMarker<T>, Y>();

    // Whether variable `V` exists at yield point `Y`.
    template <specific_coro_type T, int V, int Y>
    constexpr bool var_lifetime_overlaps_yield_const = detail::RawVarYieldReach<
        detail::GetMarker<T>,
        detail::unpack_var_index<detail::GetMarker<T>>(V),
        Y
    >::value;

    // Same as `var_lifetime_overlaps_yield_const`, but for non-const indices. Throws if anything is out of range.
    template <specific_coro_type T>
    [[nodiscard]] bool var_lifetime_overlaps_yield(int var_index, int yield_index)
    {
        if (var_index < 0 || var_index >= num_vars<T>)
            throw std::runtime_error("Coroutine variable index is out of range.");
        if (yield_index < 0 || yield_index >= num_yields<T>)
            throw std::runtime_error("Coroutine yield point index is out of range.");
        if constexpr (num_vars<T> == 0) // We always have at least one yield point, otherwise we'd check that here too.
        {
            return false;
        }
        else
        {
            bool ret = false;
            detail::with_const_index<num_yields<T>>(yield_index, [&](auto yield)
            {
                auto vars = [&]<int ...V>(std::integer_sequence<int, V...>){
                    return std::array{var_lifetime_overlaps_yield_const<T, V, yield.value>...};
                }(std::make_integer_sequence<int, num_vars<T>>{});
                ret = vars[var_index];
            });
            return ret;
        }
    }

    // Yield point name helpers:

    // Converts a yield point name to its index.
    // Returns a negative error code on failure: either `unknown_name` or `ambiguous_name`.
    template <specific_coro_type T>
    [[nodiscard]] constexpr int yield_index_or_negative(std::string_view name)
    {
        const auto &arr = detail::yield_name_to_index_mapping<detail::GetMarker<T>>;
        auto it = std::partition_point(arr.begin(), arr.end(), [&](const auto &pair){return pair.first < name;});
        if (it == arr.end() || it->first != name)
            return unknown_name;
        auto next_it = it;
        ++next_it;
        if (next_it != arr.end() && next_it->first == name)
            return ambiguous_name;
        return it->second;
    }
    // Same, but throws on failure.
    template <specific_coro_type T>
    [[nodiscard]] constexpr int yield_index(std::string_view name)
    {
        int ret = yield_index_or_negative<T>(name);
        if (ret == ambiguous_name)
            throw std::runtime_error("Ambiguous coroutine yield point name: `" + std::string(name) + "`.");
        if (ret < 0)
            throw std::runtime_error("Unknown coroutine yield point name: `" + std::string(name) + "`.");
        return ret;
    }
    // Same, but works with constexpr strings, and causes a SOFT compilation error if the name is invalid.
    template <specific_coro_type T, const_string Name>
    requires(yield_index_or_negative<T>(Name.view()) >= 0)
    constexpr int yield_index_const = yield_index_or_negative<T>(Name.view());

    // Given a yield point index, returns its name. Same as `yield_name_const`, but with a possibly dynamic index.
    // Throws if the index is out of range.
    template <specific_coro_type T>
    [[nodiscard]] constexpr std::string_view yield_name(int i)
    {
        if (i < 0 || i >= num_yields<T>)
            throw std::runtime_error("Coroutine yield point index is out of range.");
        constexpr auto arr = []<int ...I>(std::integer_sequence<int, I...>){
            return std::array{yield_name_const<T, I>.view()...};
        }(std::make_integer_sequence<int, num_yields<T>>{});
        return arr[i];
    }

    // If true, all yield points are uniquely named.
    // The names can't be empty, because the implicit 0-th checkpoint has an empty name.
    // If there are no yields, returns true.
    template <specific_coro_type T>
    constexpr bool yields_names_are_unique = []{
        std::array<std::string_view, num_yields<T>> arr;
        for (int i = 0; i < num_yields<T>; i++)
            arr[i] = yield_name<T>(i);
        std::sort(arr.begin(), arr.end());
        return std::adjacent_find(arr.begin(), arr.end()) == arr.end();
    }();


    // Examining low-level variable layout:

    // Returns the stack frame size of the coroutine.
    template <specific_coro_type T>
    constexpr std::size_t frame_size = detail::FrameSize<detail::GetMarker<T>>::value;
    // Returns the stack frame alignment of the coroutine.
    template <specific_coro_type T>
    constexpr std::size_t frame_alignment = detail::FrameAlignment<detail::GetMarker<T>>::value;
    // If true, the coroutine only uses `std::is_trivially_copyable` variables, meaning its frame can be freely copied around as bytes.
    template <specific_coro_type T>
    constexpr bool frame_is_trivially_copyable = detail::FrameIsTriviallyCopyable<detail::GetMarker<T>>::value;

    // The offset of variable `V` in the stack frame.
    // Different variables can overlap if `var_lifetime_overlaps_var` is false for them.
    template <specific_coro_type T, int V>
    constexpr std::size_t var_offset = detail::RawVarOffset<detail::GetMarker<T>, detail::unpack_var_index<detail::GetMarker<T>>(V)>::value;

    // Whether variables `A` and `B` have overlapping lifetime.
    template <specific_coro_type T, int A, int B>
    constexpr bool var_lifetime_overlaps_var = detail::RawVarVarReach<
        detail::GetMarker<T>,
        detail::unpack_var_index<detail::GetMarker<T>>(A),
        detail::unpack_var_index<detail::GetMarker<T>>(B)
    >::value;


    // Misc:

    struct rewind_tag_t {};
    // Pass this to the constructor of `specific_coro<T>` to automatically rewind the coroutine.
    // `specific_coro<T>{}.rewind()` isn't enough, because it requires moveability.
    // This is what `RCORO()` uses internally.
    constexpr rewind_tag_t rewind{};


    // The coroutine class.
    template <detail::ValidCoroTypes T>
    class specific_coro : public detail::BasicStateInterface<specific_coro<T>>
    {
        friend detail::SpecificCoroFriend;

        typename T::_rcoro_frame_t frame;

        // `detail::BasicStateInterface` uses this.
        friend detail::BasicStateInterface<specific_coro>;
        constexpr detail::State _state_interface_enum() const noexcept {return frame.state;}

        // `load_unordered` uses this.
        // Can't define it inside of that function because of Clang bug: https://github.com/llvm/llvm-project/issues/59734
        template <int Y>
        struct LoadUnorderedGuard
        {
            specific_coro &co;
            bool fail = true;
            int i = 0;
            std::array<void (*)(specific_coro &), yield_vars<specific_coro, Y>.size()> funcs{};
            constexpr ~LoadUnorderedGuard()
            {
                if (!fail)
                    return;
                while (i > 0)
                {
                    i--;
                    funcs[i](co);
                }
            }
        };

      public:
        // Constructs a finished coroutine, with `finish_reason() == reset`.
        constexpr specific_coro() {}
        // Constructs a ready coroutine.
        constexpr specific_coro(rewind_tag_t) {rewind();}

        // Copyable and movable, assuming all the elements are.
        // Copying, moving, and destroying abort the program if any of the involved coroutines are `busy()`.
        // Can't use exceptions for those errors, because then we lose `noexcept`ness (and using them only if `noexcept` is missing is lame).


        // Zeroes the coroutine, destroying all variables. Throws if `busy()`. Returns `*this`.
        constexpr specific_coro &reset() &
        {
            if (this->busy())
                throw std::runtime_error("Can't reset a busy coroutine.");
            frame.reset();
            return *this;
        }
        constexpr specific_coro &&reset() &&
        {
            reset();
            return std::move(*this);
        }

        // Resets the coroutine to the initial position.
        // After this, `finished() == false` and `yield_point() == 0`.
        constexpr specific_coro  &rewind() &  {reset(); frame.state = detail::State::not_finished; return *this;}
        constexpr specific_coro &&rewind() && {rewind(); return std::move(*this);}


        // Manipulating the coroutine:

        // Returns true if the coroutine can be called with arguments `P...`.
        template <typename ...P>
        static constexpr bool callable_with_args = std::is_invocable_v<typename T::_rcoro_lambda_t, typename T::_rcoro_frame_t &, int, P...>;

        // Runs a single step of the coroutine.
        // Returns `*this`. Note that the return value is convertible to bool, returning `!finished()`.
        // Throws if `can_resume() == false`.
        template <typename ...P> requires callable_with_args<P...>
        constexpr specific_coro &operator()(P &&... params) &
        {
            if (!this->can_resume())
                throw std::runtime_error("This coroutine can't be resumed. It's either finished or busy.");

            frame.state = detail::State::running;

            bool had_exception = true;
            struct Guard
            {
                specific_coro &self;
                bool &had_exception;
                constexpr ~Guard()
                {
                    if (had_exception)
                    {
                        self.frame.state = detail::State::exception;
                        self.frame.pos = 0;
                    }
                }
            };
            Guard guard{*this, had_exception};

            int jump_to = frame.pos;

            // This trick forces the return type to be `void`.
            false ? void() : typename T::_rcoro_lambda_t{}(frame, jump_to, std::forward<P>(params)...);

            had_exception = false;

            if (frame.state == detail::State::running)
            {
                frame.state = detail::State::success;
                frame.pos = 0;
            }
            else
            {
                frame.state = detail::State::not_finished;
            }
            return *this;
        }
        template <typename ...P> requires callable_with_args<P...>
        constexpr specific_coro &&operator()(P &&... params) &&
        {
            operator()(std::forward<P>(params)...);
            return std::move(*this);
        }


        // Variable reflection:

        // Returns true if the variable currently exists.
        // Throws if `busy()`.
        template <int V>
        [[nodiscard]] constexpr bool var_exists() const
        {
            if (this->busy())
                throw std::runtime_error("Can't manipulate variables in a busy coroutine.");
            return frame.template var_exists<V>();
        }
        template <const_string Name>
        [[nodiscard]] constexpr bool var_exists() const
        {
            return var_exists<var_index_const<specific_coro, Name>>();
        }

        // Returns a variable.
        // Throws if `busy()`, or if the variable doesn't exist at the current yield point.
        template <int V>
        [[nodiscard]] constexpr var_type<specific_coro, V> &var()
        {
            if (this->busy())
                throw std::runtime_error("Can't manipulate variables in a busy coroutine.");
            if (!frame.template var_exists<V>())
                throw std::runtime_error("The coroutine variable `" + std::string(var_name_const<specific_coro, V>.view()) + "` doesn't exist at this point.");
            return frame.template var<V>();
        }
        template <int V>
        [[nodiscard]] constexpr const var_type<specific_coro, V> &var() const
        {
            return const_cast<specific_coro *>(this)->var<V>();
        }
        template <const_string Name> [[nodiscard]] constexpr       var_type<specific_coro, var_index_const<specific_coro, Name>> &var()       {return var<var_index_const<specific_coro, Name>>();}
        template <const_string Name> [[nodiscard]] constexpr const var_type<specific_coro, var_index_const<specific_coro, Name>> &var() const {return var<var_index_const<specific_coro, Name>>();}


        // Stack frame access:

        // Returns a pointer to `frame_size` bytes, containing the variables. You can index into it using `var_offset`.
        // Notably works and doesn't throw even if the coroutine is `busy()`.
        [[nodiscard]]       void *frame_storage()       noexcept {return frame.storage();}
        [[nodiscard]] const void *frame_storage() const noexcept {return frame.storage();}


        // Serialization:

        // Returns the current yield point index. Note that there's an implicit 0-th yield point at the beginning.
        // Note that this never throws, and can return a valid index even while the coroutine is running.
        // When finished, returns `0`.
        [[nodiscard]] constexpr int yield_point() const noexcept {return frame.pos;}
        // Returns the current yield point name.
        // The name can be specified in `RC_YIELD("...")`, empty by default. For 0-th point the name is always empty.
        // Like `yield_point()`, never throws. Returns an empty string when finished.
        [[nodiscard]] constexpr std::string_view yield_point_name() const noexcept
        {
            return yield_name<specific_coro>(frame.pos);
        }

        // Calls a function for each alive variable. Throws if `busy()`. Does nothing if finished or paused at the implicit 0-th yield point.
        // `func` is `bool func(auto index)`, where `index.value` is the constexpr variable index.
        // Use `.var<i>()` and `rcoro::var_name_const<i>` to then manipulate the variables.
        // If `func` returns true, stops immeidately and also returns true.
        template <typename F>
        constexpr bool for_each_alive_var(F &&func) const
        {
            if (this->busy())
                throw std::runtime_error("Can't manipulate variables in a busy coroutine.");
            bool ret = false;
            if (frame.pos != 0) // Not strictly necessary, hopefully an optimization.
            {
                detail::with_const_index<num_yields<specific_coro>>(frame.pos, [&](auto yieldindex)
                {
                    constexpr auto num_indices = yield_vars<specific_coro, yieldindex.value>.size();
                    ret = detail::const_any_of<num_indices>([&](auto varindex)
                    {
                        constexpr auto indices = yield_vars<specific_coro, yieldindex.value>;
                        return bool(func(std::integral_constant<int, indices[varindex.value]>{}));
                    });
                });
            }
            return ret;
        }


        // Deserialization:

        // The most basic deserialization primitive. Does following, in this order:
        // Calls `reset()`.
        // Validates `fin_reason` and `yield_index`, throws if they are out of range. Also throws if `fin_reason != not_finished && yield_index != 0`.
        // Calls `func`, which is `bool func()`. If it returns false, stops immediately and also returns false.
        // `func` is supposed to load the variables into `frame_storage()` as raw bytes, or throw or return false on failure.
        // Applies `fin_reason` and `yield_index`, then returns true.
        // The `func` can be empty and always return true, if you load the variables beforehand.
        template <typename F>
        requires frame_is_trivially_copyable<specific_coro>
        constexpr bool load_raw_bytes(rcoro::finish_reason fin_reason, int yield_index, F &&func)
        {
            return load_raw_bytes_UNSAFE(fin_reason, yield_index, std::forward<F>(func));
        }

        // Same as `load_raw_bytes`, but compiles for any variable types (compiles even if `frame_is_trivially_copyable<Coro>` is false).
        // Warning! If `func` returns true, it must placement-new all the `yield_vars` into the `frame_storage()`, otherwise UB ensues.
        // And if `func` returns false or throws, it must not leave any variables alive.
        // The recommended way of creating the variables is `::new((void *)((char *)frame_storage() + rcoro::var_offset<Coro, i>)) type(init);`.
        template <typename F>
        constexpr bool load_raw_bytes_UNSAFE(rcoro::finish_reason fin_reason, int yield_index, F &&func)
        {
            reset();
            if (fin_reason < rcoro::finish_reason{} || fin_reason >= rcoro::finish_reason::_count)
                throw std::runtime_error("Coroutine finish reason is out of range.");
            if (yield_index < 0 || yield_index >= num_yields<specific_coro>)
                throw std::runtime_error("Coroutine yield point index is out of range.");
            if (fin_reason != rcoro::finish_reason::not_finished && yield_index != 0)
                throw std::runtime_error("When loading a finished coroutine, the yield point index must be 0.");
            if (!bool(std::forward<F>(func)()))
                return false;
            frame.pos = yield_index;
            frame.state = detail::State(fin_reason);
            return true;
        }

        // Switches the coroutine to a custom state.
        // Throws if `fin_reason` or `yield_index` are invalid, see `load_raw_bytes` for how they're validated.
        // `func` is then called for every variable existing at that yield point.
        // `func` is `void func(auto index, auto construct)`, where `index` is the variable index in form of `std::integral_constant<int, N>`,
        // and `construct` is `void construct(auto &&...)`. `construct` must be called at most once to construct the variable. Throws if it's called more than once.
        // If `construct` is not called, will abort the load, destroy the previously constructed variables, and return false.
        // If the function ends up throwing or returning false, the coroutine is zeroed as if by `reset()`.
        template <typename F>
        constexpr bool load(rcoro::finish_reason fin_reason, int yield_index, F &&func)
        {
            return load_raw_bytes_UNSAFE(fin_reason, yield_index, [&]
            {
                return !frame.handle_vars(
                    yield_index,
                    [&](auto varindex, auto completed)
                    {
                        bool ok = false;
                        // This trick forces the return type to be `void`.
                        false ? void() : func(varindex, [&]<typename ...P>(P &&... params)
                        {
                            if (ok)
                                throw std::runtime_error("Coroutine `load()` user callback must call the callback it receives at most once.");
                            std::construct_at(frame.template var_storage<varindex.value>(), std::forward<P>(params)...);
                            completed(); // Now `handle_vars` will clean up this var too if something goes wrong.
                            ok = true;
                        });
                        return !ok;
                    },
                    [&](auto varindex) noexcept
                    {
                        std::destroy_at(&frame.template var<varindex.value>());
                    }
                );
            });
        }

        // Switches the coroutine to a custom state, loading the variables in a custom order.
        // `func` is `void func(auto var)`. `load_var` is `void load_var(auto var_index, auto construct, ...)` (see below).
        // Throws if `fin_reason` or `yield_index` are invalid, see `load_raw_bytes` for how they're validated.
        // Calls `func` once.
        // `func` should call `var`, which is `void var(int var_index, ...)`, for every variable it knows.
        // `var_index` can be obtained by calling `rcoro::var_index()` on a string, which is the variable name.
        // `var` validates the given variable index (throws if invalid), then calls `load_var` passing
        // the index as the first argument as `std::integral_constant`. The remaining arguments are forwarded to `load_var` as is.
        // The sole purpose of `var` is to validate the variable index and convert it to a compile-time constant.
        // `load_var` then should call `construct` (which is `void construct(auto &&...)`) at most once to initialize the variable.
        // Calling `construct` more than once per variable throws. Not calling it at all is valid, but you have to revisit that variable later.
        // `func` can return false at any time to abort, then the whole function also returns false.
        // Ultimately `func` should return `true` to finish the load. Then the final validation is performed, throwing if any variables were missed.
        // Then, on success, returns true.
        // If the function ends up throwing or returning false, the coroutine is zeroed as if by `reset()`, and any variables
        // constructed so far are destroyed (in reverse construction order, though elsewhere we use reverse declaration order).
        template <typename F, typename G>
        constexpr bool load_unordered(rcoro::finish_reason fin_reason, int yield_index, F &&func, G &&load_var)
        {
            // Don't want to forward the return value out of `load_var`, because this gets wonky when there are no yield points.
            // What do we return then? `void` might not work with the user code,
            // and we can't get any type out of `load_var` without throwing a wrong index at it.
            return load_raw_bytes_UNSAFE(fin_reason, yield_index, [&]
            {
                bool ret = false;

                // We always have at least one
                detail::with_const_index<num_yields<specific_coro>>(yield_index, [&](auto yield_index_const)
                {
                    constexpr int yield_index = yield_index_const.value;

                    LoadUnorderedGuard<yield_index> guard{.co = *this};
                    std::array<bool, yield_vars<specific_coro, yield_index>.size()> vars_done{};

                    bool ok = std::forward<F>(func)([&](int var_index, auto &&... extra)
                    {
                        constexpr int yield_index = yield_index_const.value; // Redefine for buggy MSVC.

                        if (var_index < 0 || var_index >= num_vars<specific_coro>)
                            throw std::runtime_error("Coroutine variable index is out of range.");
                        auto it = std::lower_bound(yield_vars<specific_coro, yield_index>.begin(), yield_vars<specific_coro, yield_index>.end(), var_index);
                        if (it == yield_vars<specific_coro, yield_index>.end() || *it != var_index)
                            throw std::runtime_error("This coroutine variable doesn't exist at this yield point.");
                        auto packed_var_index = it - yield_vars<specific_coro, yield_index>.begin();
                        if (vars_done[packed_var_index])
                            throw std::runtime_error("This coroutine variable was already loaded.");

                        detail::with_const_index<yield_vars<specific_coro, yield_index>.size()>(packed_var_index, [&](auto packed_var_index_const)
                        {
                            static constexpr auto var_index_const =
                                std::integral_constant<int, yield_vars<specific_coro, yield_index_const.value>[packed_var_index_const.value]>{};
                            // This trick forces the return type to be `void`.
                            false ? void() : load_var(var_index_const, [&]<typename ...P>(P &&... params)
                            {
                                if (vars_done[packed_var_index])
                                    throw std::runtime_error("This coroutine variable was already loaded.");
                                std::construct_at(frame.template var_storage<var_index_const.value>(), std::forward<P>(params)...);
                                vars_done[packed_var_index] = true;
                                guard.funcs[guard.i++] = [](specific_coro &self) noexcept {std::destroy_at(&self.frame.template var<var_index_const.value>());};
                            }, decltype(extra)(extra)...);
                        });
                    });

                    if (!ok)
                        return;
                    if (guard.i != yield_vars<specific_coro, yield_index>.size())
                        throw std::runtime_error("Some coroutine variables are missing.");
                    guard.fail = false;
                    ret = true;
                    return;
                });
                return ret;
            });
        }


        // Printing debug state.
        template <typename A, typename B>
        friend std::basic_ostream<A, B> &operator<<(std::basic_ostream<A, B> &s, specific_coro &c)
        {
            if (c.finished())
            {
                s << "finish_reason = ";
                switch (c.finish_reason())
                {
                    case finish_reason::not_finished:    RCORO_ASSERT(false); break;
                    case finish_reason::reset:           s << "reset";      break;
                    case finish_reason::success:         s << "success";   break;
                    case finish_reason::exception:       s << "exception"; break;
                    case finish_reason::null:            RCORO_ASSERT(false); break; // A non-type-erased coroutine can't be null.
                    case finish_reason::_extended_count: RCORO_ASSERT(false); break;
                }
                return s;
            }

            if (c.busy())
                s << "busy, ";

            s << "yield_point = " << c.yield_point();

            if (std::string_view str = c.yield_point_name(); !str.empty())
                s << ", `" << str << "`";

            if (c.busy())
                return s;

            detail::const_for<num_vars<specific_coro>>([&](auto varindex)
            {
                constexpr int i = varindex.value;
                s << "\n  " << i << ". " << var_name_const<specific_coro, i>.view();
                if constexpr (detail::Printable<var_type<specific_coro, i>, std::basic_ostream<A, B>>)
                {
                    if (c.template var_exists<i>()) // GCC 11 needs `template` here.
                        s << " = " << c.template var<i>(); // GCC 11 needs `template` here.
                }
                else
                {
                    if (c.template var_exists<i>())
                        s << " - alive but not printable";
                }

                if (!c.template var_exists<i>())
                    s << " - dead";
            });

            return s;
        }
    };


    // Debug info.

    namespace detail
    {
        template <specific_coro_type T>
        struct DebugInfoPrinter
        {
            template <typename A, typename B>
            friend std::basic_ostream<A, B> &operator<<(std::basic_ostream<A, B> &s, DebugInfoPrinter)
            {
                s << "copying: ctor=" << (!std::is_copy_constructible_v<T> ? "no" : std::is_nothrow_copy_constructible_v<T> ? "yes(nothrow)" : "yes")
                  << " assign=" << (!std::is_copy_assignable_v<T> ? "no" : std::is_nothrow_copy_assignable_v<T> ? "yes(nothrow)" : "yes") << '\n';
                s << "moving: ctor=" << (!std::is_move_constructible_v<T> ? "no" : std::is_nothrow_move_constructible_v<T> ? "yes(nothrow)" : "yes")
                  << " assign=" << (!std::is_move_assignable_v<T> ? "no" : std::is_nothrow_move_assignable_v<T> ? "yes(nothrow)" : "yes") << '\n';

                s << "frame: size=" << frame_size<T> << " align=" << frame_alignment<T> << '\n';

                s << num_vars<T> << " variable" << (num_vars<T> != 1 ? "s" : "") << ":\n";
                detail::const_for<num_vars<T>>([&](auto varindex)
                {
                    constexpr int v = varindex.value;
                    s << "  " << v << ". " << var_name_const<T, v>.view() << ", " << detail::type_name<var_type<T, v>>() << '\n';
                    s << "      offset=" << var_offset<T, v> << ", size=" << sizeof(var_type<T, v>) << ", align=" << alignof(var_type<T, v>) << '\n';

                    bool first = true;
                    detail::const_for<v>([&](auto sub_varindex)
                    {
                        constexpr int v = varindex.value; // Redeclare for buggy MSVC.
                        constexpr int v2 = sub_varindex.value;
                        if constexpr (var_lifetime_overlaps_var<T, v, v2>)
                        {
                            if (first)
                            {
                                first = false;
                                s << "      visible_vars: ";
                            }
                            else
                                s << ", ";
                            s << v2 << "." << var_name_const<T, v2>.view();
                        }
                    });
                    if (!first)
                        s << '\n';
                });

                s << num_yields<T> << " yield" << (num_yields<T> != 1 ? "s" : "") << ":";
                detail::const_for<num_yields<T>>([&](auto yieldindex)
                {
                    constexpr int y = yieldindex.value;
                    s << "\n  " << y << ". `" << yield_name_const<T, y>.view() << "`";
                    detail::const_for<yield_vars<T, y>.size()>([&](auto packed_varindex)
                    {
                        if (packed_varindex.value == 0)
                            s << ", visible_vars: ";
                        else
                            s << ", ";
                        constexpr auto varindex = yield_vars<T, yieldindex.value>[packed_varindex.value];
                        s << varindex << "." << var_name_const<T, varindex>.view();
                    });
                });

                return s;
            }
        };
    }

    // Print `debug_info<Coro>` to an `std::ostream` to get the debug information about a type.
    template <specific_coro_type T>
    constexpr detail::DebugInfoPrinter<T> debug_info;


    // Wrappers.

    namespace type_erasure_bits
    {
        struct basic_vtable
        {
            detail::State (*internal_state)(const void *) = nullptr;
            void (*reset)(void *) = nullptr;
            void (*rewind)(void *) = nullptr;

            template <typename T>
            constexpr void fill()
            {
                internal_state = [](const void *c){return detail::SpecificCoroFriend::get_state(*static_cast<const T *>(c));};
                reset = [](void *c){static_cast<T *>(c)->reset();};
                rewind = [](void *c){static_cast<T *>(c)->rewind();};
            }
        };

        template <typename T, typename Vtable> requires std::is_base_of_v<basic_vtable, Vtable>
        constexpr Vtable vtable_storage = []{Vtable ret; ret.template fill<T>(); return ret;}();


        template <typename ...P>
        struct basic_interface_vtable : basic_vtable
        {
            void (*invoke)(void *, P &&...) = nullptr;

            template <typename T>
            constexpr void fill()
            {
                basic_vtable::fill<T>();
                invoke = [](void *c, P &&... params){static_cast<T *>(c)->operator()(std::forward<P>(params)...);};
            }
        };

        template <typename Derived, typename Vtable, typename ...P>
        class basic_interface
            // Not passing `Derived` directly here, because then it needs to `friend` the `BasicStateInterface`, which is annoying.
            : public detail::BasicStateInterface<basic_interface<Derived, Vtable, P...>>
        {
          protected:
            const Vtable *basic_interface_vptr() const {return static_cast<const Derived *>(this)->_basic_interface_vptr();}
                  void *basic_interface_target()       {return static_cast<      Derived *>(this)->_basic_interface_target();}
            const void *basic_interface_target() const {return static_cast<const Derived *>(this)->_basic_interface_target();}

            // `detail::BasicStateInterface` uses this.
            friend detail::BasicStateInterface<basic_interface>;
            constexpr detail::State _state_interface_enum() const noexcept {return basic_interface_vptr() ? basic_interface_vptr()->internal_state(basic_interface_target()) : detail::State(finish_reason::null);}

          public:
            // No-op if null.
            Derived &reset() & {if (basic_interface_vptr()) basic_interface_vptr()->reset(basic_interface_target()); return static_cast<Derived &>(*this);}
            Derived &&reset() && {return std::move(reset());}
            // Throws if null.
            Derived &rewind() &
            {
                if (!basic_interface_vptr())
                    throw std::runtime_error("Can't rewind a null coroutine.");
                basic_interface_vptr()->rewind(basic_interface_target());
                return static_cast<Derived &>(*this);
            }
            Derived &&rewind() && {return std::move(rewind());}
            // Throws if null.
            Derived &operator()(P ...params) &
            {
                if (!basic_interface_vptr())
                    throw std::runtime_error("Can't call a null coroutine.");
                basic_interface_vptr()->invoke(basic_interface_target(), std::forward<P>(params)...);
                return static_cast<Derived &>(*this);
            }
            Derived &&operator()(P ...params) &&
            {
                if (!basic_interface_vptr())
                    throw std::runtime_error("Can't call a null coroutine.");
                basic_interface_vptr()->invoke(basic_interface_target(), std::forward<P>(params)...);
                return std::move(static_cast<Derived &>(*this));
            }
        };


        template <typename ...P> using basic_view_vtable = basic_interface_vtable<P...>;

        template <typename Derived, typename Vtable, typename ...P>
        class basic_view : public basic_interface<Derived, Vtable, P...>
        {
            using base = basic_interface<Derived, Vtable, P...>;

          public:
            const Vtable *vptr = nullptr;
            void *target = nullptr;

            // `basic_interface` uses this.
            friend base;
            const Vtable *_basic_interface_vptr() const {return vptr;}
            void *_basic_interface_target() const {return target;}

          public:
            constexpr basic_view() noexcept {}
            constexpr basic_view(std::nullptr_t) noexcept {}

            template <specific_coro_type T> requires T::template callable_with_args<P...>
            constexpr basic_view(T &coro) : vptr(&vtable_storage<T, Vtable>), target(&coro) {}
            template <specific_coro_type T> requires T::template callable_with_args<P...>
            constexpr basic_view(T &&coro) : vptr(&vtable_storage<T, Vtable>), target(&coro) {}
        };


        template <typename ...P>
        struct basic_any_noncopyable_vtable : basic_interface_vtable<P...>
        {
            // Can't seem to put this directly into the `basic_any_noncopyable`.
            template <typename T>
            static constexpr bool constructor_param_allowed = true;

            void (*destroy_at)(void *) noexcept = nullptr;

            template <typename T>
            constexpr void fill()
            {
                basic_interface_vtable<P...>::template fill<T>();
                destroy_at = [](void *c) noexcept
                {
                    std::destroy_at(static_cast<T *>(c));
                };
            }
        };

        template <typename Derived, typename Vtable, typename ...P>
        class basic_any_noncopyable : public basic_interface<Derived, Vtable, P...>
        {
            using base = basic_interface<Derived, Vtable, P...>;

          protected:
            const Vtable *vptr = nullptr;
            void *memory = nullptr;

            // `basic_interface` uses this.
            friend base;
            const Vtable *_basic_interface_vptr() const {return vptr;}
            void *_basic_interface_target() const {return memory;}

          public:
            constexpr basic_any_noncopyable() {}
            constexpr basic_any_noncopyable(std::nullptr_t) {}

            template <typename T>
            requires
                specific_coro_type<std::remove_cvref_t<T>>
                && std::is_constructible_v<std::remove_cvref_t<T>, T &&> // Can copy/move into the wrapper.
                && Vtable::template constructor_param_allowed<T>
                && std::remove_cvref_t<T>::template callable_with_args<P...>
            constexpr basic_any_noncopyable(T &&coro)
            {
                using type = std::remove_cvref_t<T>;
                static_assert(alignof(type) <= __STDCPP_DEFAULT_NEW_ALIGNMENT__, "Overaligned types are not supported.");

                vptr = &vtable_storage<type, Vtable>;
                memory = operator new(sizeof(type));
                struct Guard
                {
                    basic_any_noncopyable &self;
                    bool fail = true;
                    ~Guard()
                    {
                        if (fail)
                            operator delete(self.memory);
                    }
                };
                Guard guard{.self = *this};
                std::construct_at(reinterpret_cast<type *>(memory), std::forward<T>(coro));
                guard.fail = false;
            }

            constexpr basic_any_noncopyable(basic_any_noncopyable &&other) noexcept
                : vptr(std::exchange(other.vptr, {})), memory(std::exchange(other.memory, {}))
            {}

            constexpr basic_any_noncopyable &operator=(basic_any_noncopyable other) noexcept
            {
                swap(*this, other);
                return *this;
            }

            friend constexpr void swap(basic_any_noncopyable &a, basic_any_noncopyable &b) noexcept
            {
                std::swap(a.vptr, b.vptr);
                std::swap(a.memory, b.memory);
            }

            ~basic_any_noncopyable()
            {
                if (vptr)
                {
                    vptr->destroy_at(memory);
                    operator delete(memory);
                }
            }
        };


        namespace type_erasure_detail
        {
            struct MemoryGuard
            {
                void *ret = nullptr;
                bool fail = true;
                ~MemoryGuard()
                {
                    if (fail)
                        operator delete(ret);
                }
            };
        }

        template <typename ...P>
        struct basic_any_vtable : basic_any_noncopyable_vtable<P...>
        {
            // `basic_any_noncopyable` uses this.
            template <typename T>
            static constexpr bool constructor_param_allowed = std::is_copy_constructible_v<std::remove_cvref_t<T>>;

            void *(*copy_construct)(const void *) = nullptr;

            template <typename T>
            constexpr void fill()
            {
                basic_any_noncopyable_vtable<P...>::template fill<T>();
                copy_construct = [](const void *c)
                {
                    type_erasure_detail::MemoryGuard guard;
                    guard.ret = operator new(sizeof(T));
                    std::construct_at(static_cast<T *>(guard.ret), *static_cast<const T *>(c));
                    guard.fail = false;
                    return guard.ret;
                };
            }
        };

        template <typename Derived, typename Vtable, typename ...P>
        class basic_any : public basic_any_noncopyable<Derived, Vtable, P...>
        {
            using base = basic_any_noncopyable<Derived, Vtable, P...>;

          public:
            using base::base;

            constexpr basic_any(const basic_any &other) : base()
            {
                if (!other.vptr)
                    return;
                this->memory = other.vptr->copy_construct(other.memory);
                this->vptr = other.vptr;
            }
            constexpr basic_any(basic_any &&other) noexcept = default;

            constexpr basic_any &operator=(basic_any other) noexcept
            {
                swap(static_cast<base &>(*this), static_cast<base &>(other));
                return *this;
            }
        };
    }

    template <typename ...P>
    class view : public type_erasure_bits::basic_view<view<P...>, type_erasure_bits::basic_view_vtable<P...>, P...>
    {
      public:
        using type_erasure_bits::basic_view<view<P...>, type_erasure_bits::basic_view_vtable<P...>, P...>::basic_view;
    };

    template <typename ...P>
    class any_noncopyable : public type_erasure_bits::basic_any_noncopyable<any_noncopyable<P...>, type_erasure_bits::basic_any_noncopyable_vtable<P...>, P...>
    {
      public:
        using type_erasure_bits::basic_any_noncopyable<any_noncopyable<P...>, type_erasure_bits::basic_any_noncopyable_vtable<P...>, P...>::basic_any_noncopyable;

        // Unsure why this is needed, but without this, we get `copy_assignable == true`, then a hard compilation error when trying to copy.
        any_noncopyable(const any_noncopyable &) = delete;
        any_noncopyable(any_noncopyable &&) = default;
        any_noncopyable &operator=(const any_noncopyable &) = delete;
        any_noncopyable &operator=(any_noncopyable &&) = default;
    };

    template <typename ...P>
    class any : public type_erasure_bits::basic_any<any<P...>, type_erasure_bits::basic_any_vtable<P...>, P...>
    {
      public:
        using type_erasure_bits::basic_any<any<P...>, type_erasure_bits::basic_any_vtable<P...>, P...>::basic_any;

        any(const any &) = default;
        any(any &&) = default;
        // Unsure why this is needed, but without this, we get `nothrow_copy_assignable == true`, which is wrong.
        any &operator=(const any &) noexcept(false) = default;
        any &operator=(any &&) = default;
    };
}

// Pause a coroutine. `ident` is a unique identifier for this yield point.
#define RC_YIELD(name) )(yield,name)(code,
// Declare a coroutine-friendly variable.
// Usage: `RC_VAR(name, init);`. The type is deduced from the initializer.
// Can't be used inside of a `for` header, `if` header, etc. We have helpers for that, see below.
#define RC_VAR(name, .../*init*/) )(var,name,__VA_ARGS__)(code,
// Same as `RC_VAR`, but the variable is only visible in the next statement.
// Would be good for `for` loops, except we have `RC_FOR` specifically for those, see below.
// Usage: `RC_WITH_VAR(name, init) ...`.
// E.g. `RC_WITH_VAR(i, 0) for (...) {...}` is same as `{RC_VAR(i, 0); for (...) {...}}`.
// Several calls can be stacked.
#define RC_WITH_VAR(name, .../*init*/) )(withvar,name,__VA_ARGS__)(code,
// `RC_FOR((name, init); cond; step) {...}` is equivalent to `RC_WITH_VAR(name, init) for(; cond; step) {...}`.
#define RC_FOR(...) DETAIL_RCORO_CALL(RC_WITH_VAR, DETAIL_RCORO_GET_PAR(__VA_ARGS__)) for (DETAIL_RCORO_SKIP_PAR(__VA_ARGS__))
// Creates a coroutine. Usage: `RCORO({...})` or `RCORO((params...){...})`.
// In any case, the braces are optional, and are pasted into the coroutine body as is. They help with clang-format.
#define RCORO(...) \
    [&]{ \
        /* A marker for stateful templates. */\
        struct _rcoro_Marker \
        { \
            /* The variable descriptions. */\
            /* Without the dummy `enable_if`, GCC 10 breaks with a strange error. */\
            using _rcoro_vars [[maybe_unused]] = typename ::std::enable_if<true, ::rcoro::detail::TypeListSkipVoid<void SF_FOR_EACH(DETAIL_RCORO_VARDESC_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__))>>::type; \
            /* The yield point descriptions. */\
            /* Without the dummy `enable_if`, GCC 10 breaks with a strange error. */\
            using _rcoro_yields [[maybe_unused]] = typename ::std::enable_if<true, ::rcoro::detail::TypeList<::rcoro::detail::ValueTag<::rcoro::const_string("")> SF_FOR_EACH(DETAIL_RCORO_YIELDDESC_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__))>>::type; \
        }; \
        /* Fallback marker variables, used when checking reachibility from yield points. */\
        SF_FOR_EACH(DETAIL_RCORO_MARKERVARS_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
        auto _rcoro_lambda = [](auto &RCORO_RESTRICT _rcoro_frame, int _rcoro_jump_to DETAIL_RCORO_LEADING_COMMA(DETAIL_RCORO_GET_PAR(DETAIL_RCORO_GET_PAR((__VA_ARGS__))))) \
        { \
            RCORO_ASSUME(_rcoro_jump_to >= 0 && _rcoro_jump_to < _rcoro_Marker::_rcoro_yields::size); \
            using _rcoro_frame_t [[maybe_unused]] = ::std::remove_cvref_t<decltype(*(&_rcoro_frame + 0))>; /* Need some redundant operations to remove `restrict`. */\
            if (_rcoro_jump_to != 0) \
                goto _rcoro_label_i; \
            SF_FOR_EACH(DETAIL_RCORO_CODEGEN_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, DETAIL_RCORO_CODEGEN_LOOP_FINAL, DETAIL_RCORO_LOOP_INIT_STATE, (firstcode,__VA_ARGS__)) \
        }; \
        /* The first pass instantiation, to calculate the stack frame layout. */\
        /* It also duplicates all of our warnings. */\
        /* GCC doesn't have a reasonable pragma to disable them, and Clang's pragma doesn't seem to work in a macro (hmm). */\
        /* Here we form a member-pointer to `operator()` to instantiate it. Don't want to call it in `if (false)`, because we don't know what to pass to user parameters. */\
        /* This also conveniently fails if there are any extra template parameters added by the user (i.e. implicitly, by using `auto` in parameters). */\
        /* Note that we need `ValueTag<...>` here. Simply forming the pointer doesn't instantiate it in MSVC. */\
        (void)::rcoro::detail::ValueTag<&decltype(_rcoro_lambda)::template operator()<::rcoro::detail::Frame<true, _rcoro_Marker>>>{}; \
        /* The second pass instantiation. It would happen automatically when the coroutine is called, but then we miss out variable type information, */\
        /* which we can't properly collect in the first pass. See `RawVarTypeReader` and others for details. */\
        (void)::rcoro::detail::ValueTag<&decltype(_rcoro_lambda)::template operator()<::rcoro::detail::Frame<false, _rcoro_Marker>>>{}; \
        struct _rcoro_Types \
        { \
            using _rcoro_marker_t [[maybe_unused]] = _rcoro_Marker; \
            using _rcoro_frame_t [[maybe_unused]] = ::rcoro::detail::Frame<false, _rcoro_Marker>; \
            using _rcoro_lambda_t [[maybe_unused]] = decltype(_rcoro_lambda); \
        }; \
        return ::rcoro::specific_coro<_rcoro_Types>(::rcoro::rewind); \
    }()

#define DETAIL_RCORO_NULL(...)
#define DETAIL_RCORO_IDENTITY(...) __VA_ARGS__
#define DETAIL_RCORO_CALL(m, ...) m(__VA_ARGS__)
#define DETAIL_RCORO_LEADING_COMMA(...) __VA_OPT__(, __VA_ARGS__)

// Given `(a)b`, returns `b`. Otherwise returns the argument unchanged.
#define DETAIL_RCORO_SKIP_PAR(...) DETAIL_RCORO_SKIP_PAR_END(DETAIL_RCORO_SKIP_PAR_ __VA_ARGS__)
#define DETAIL_RCORO_SKIP_PAR_(...) DETAIL_RCORO_SKIPPED_PAR
#define DETAIL_RCORO_SKIP_PAR_END(...) DETAIL_RCORO_SKIP_PAR_END_(__VA_ARGS__)
#define DETAIL_RCORO_SKIP_PAR_END_(...) DETAIL_RCORO_SKIP_PAR_END_##__VA_ARGS__
#define DETAIL_RCORO_SKIP_PAR_END_DETAIL_RCORO_SKIP_PAR_
#define DETAIL_RCORO_SKIP_PAR_END_DETAIL_RCORO_SKIPPED_PAR

// Given `(a)b`, returns `a`. Otherwise returns nothing.
#define DETAIL_RCORO_GET_PAR(...) DETAIL_RCORO_GET_PAR_END(DETAIL_RCORO_GET_PAR_ __VA_ARGS__) )
#define DETAIL_RCORO_GET_PAR_(...) DETAIL_RCORO_GOT_PAR(__VA_ARGS__)
#define DETAIL_RCORO_GET_PAR_END(...) DETAIL_RCORO_GET_PAR_END_(__VA_ARGS__)
#define DETAIL_RCORO_GET_PAR_END_(...) DETAIL_RCORO_GET_PAR_END_##__VA_ARGS__
#define DETAIL_RCORO_GET_PAR_END_DETAIL_RCORO_GET_PAR_ DETAIL_RCORO_NULL(
#define DETAIL_RCORO_GET_PAR_END_DETAIL_RCORO_GOT_PAR(...) __VA_ARGS__ DETAIL_RCORO_NULL(

// Generate an internal name for a coroutine variable. `name` is the user-friendly name, and `ident` is a unique identifier.
#define DETAIL_RCORO_STORAGE_VAR_NAME(ident, name) SF_CAT(_rcoro_var_, SF_CAT(ident, SF_CAT(_, name)))
// Generate an internal name for a marker variable used by reachibility checks.
#define DETAIL_RCORO_MARKER_VAR_NAME(ident) SF_CAT(_rcoro_visibility_check_, ident)
// Apply this to an element of the loop state containing variable reachibility markers, to get a proper list of those markers.
#define DETAIL_RCORO_EXPAND_MARKERS(x, ...) __VA_ARGS__

// The initial state for the macro loops.
// (0) is a unique identifier consisting of repeated `i`s, (1), (2) are yield and variable counters respectively,
// and (3) is a list of variable markers to test their reachibility at yield points.
// Yield index starts from `1` because the 0-th yield point is implicit.
#define DETAIL_RCORO_LOOP_INIT_STATE (i,1,0,(x))

// An universal step function for our loops.
#define DETAIL_RCORO_LOOP_STEP(n, d, kind, ...) SF_CAT(DETAIL_RCORO_LOOP_STEP_, kind) d
#define DETAIL_RCORO_LOOP_STEP_firstcode(ident, yieldindex, rawvarindex, markers) (       ident    , yieldindex  , rawvarindex  , markers)
#define DETAIL_RCORO_LOOP_STEP_code(ident, yieldindex, rawvarindex, markers)      (       ident    , yieldindex  , rawvarindex  , markers)
#define DETAIL_RCORO_LOOP_STEP_var(ident, yieldindex, rawvarindex, markers)       (SF_CAT(ident, i), yieldindex  , rawvarindex+1, (DETAIL_RCORO_IDENTITY markers, DETAIL_RCORO_MARKER_VAR_NAME(ident)))
#define DETAIL_RCORO_LOOP_STEP_withvar(ident, yieldindex, rawvarindex, markers)   (SF_CAT(ident, i), yieldindex  , rawvarindex+1, (DETAIL_RCORO_IDENTITY markers, DETAIL_RCORO_MARKER_VAR_NAME(ident)))
#define DETAIL_RCORO_LOOP_STEP_yield(ident, yieldindex, rawvarindex, markers)     (SF_CAT(ident, i), yieldindex+1, rawvarindex  , markers)

// The loop body for the function body generation.
#define DETAIL_RCORO_CODEGEN_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_CODEGEN_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_firstcode(ident, yieldindex, rawvarindex, markers, ...) DETAIL_RCORO_SKIP_PAR(__VA_ARGS__) // Skip optional parameters.
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_code(ident, yieldindex, rawvarindex, markers, ...) __VA_ARGS__
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_var(ident, yieldindex, rawvarindex, markers, name, ...) \
    /* Make sure we're not preceded by an if/for/while/etc. */\
    /* The test is performed by having two separate statements, where the latter needs the former to be visible to compile. */\
    /* Note that the first `[[maybe_unused]]` is not strictly necessary, but it removes an extra useless warning when this check fails. */\
    [[maybe_unused]] static constexpr bool SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))) = true; \
    [[maybe_unused]] static constexpr bool SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_CheckBracesAroundDeclarationOf_, name))) = SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))); \
    /* If the variable ends up unused, it will be stored here. */\
    DETAIL_RCORO_VAR_FALLBACKSTORAGE(ident, rawvarindex); \
    /* If we're not jumping, initialize the variable. */\
    DETAIL_RCORO_VAR_INIT(ident, rawvarindex, __VA_ARGS__); \
    /* Jump target. */\
  SF_CAT(_rcoro_label_, ident): \
    /* The scope marker for `RC_YIELD()`. This used to be combined with one of the two `...CheckBraces...` variables above, */\
    /* but MSVC has a bug where a static constexpr variable can't shadow another one, and silently returns the original value. */\
    /* Because of that, this must be non-static. And since `goto` can't jump over init of non-static constexpr variables, */\
    /* we can no longer combine those. */\
    [[maybe_unused]] constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = true; \
    /* This will destroy the variable when it goes out of scope. */\
    DETAIL_RCORO_VAR_GUARD(ident, rawvarindex, name); \
    /* Create a reference as a fancy name for our storage variable. */\
    DETAIL_RCORO_VAR_REF(ident, rawvarindex, name); \
    /* Jump to the next macro, if necessary. */\
    if (_rcoro_jump_to != 0) \
    { \
        /* Jump to the next location. */\
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    /* Stateful meta magic. This can be placed anywhere. */\
    DETAIL_RCORO_VAR_META(rawvarindex, markers) \
    /* Force trailing semicolon. */\
    do {} while (false)
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_withvar(ident, yieldindex, rawvarindex, markers, name, ...) \
    /* Jump through some hoops to get ourselves an uninitialized fallback-storage. */\
    /* Would normally put it into an `if` condition, but that requires an initializer, stopping us from `goto`ing over it. */\
    if (true) goto SF_CAT(_rcoro_label2_, ident); else \
    for (DETAIL_RCORO_VAR_FALLBACKSTORAGE(ident, rawvarindex); false;) \
  SF_CAT(_rcoro_label2_, ident): \
    if (void(DETAIL_RCORO_VAR_INIT(ident, rawvarindex, __VA_ARGS__)), false) {} else \
  SF_CAT(_rcoro_label_, ident): \
    if (constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = true; false) {} else \
    if (DETAIL_RCORO_VAR_GUARD(ident, rawvarindex, name); false) {} else \
    if (DETAIL_RCORO_VAR_REF(ident, rawvarindex, name); _rcoro_jump_to != 0) \
    { \
        /* MSVC doesn't like an attribute plus `; cond` in an `if`-statement, so we do it the old way. */\
        (void)DETAIL_RCORO_MARKER_VAR_NAME(ident); \
        DETAIL_RCORO_VAR_META(rawvarindex, markers) \
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    else
// Variable code pieces:
#define DETAIL_RCORO_VAR_FALLBACKSTORAGE(ident, rawvarindex) \
    ::rcoro::detail::RawVarFallbackStorage<_rcoro_frame_t::fake, _rcoro_Marker, rawvarindex> SF_CAT(_rcoro_fallback_storage_, ident)
#define DETAIL_RCORO_VAR_INIT(ident, rawvarindex, ...) \
    ::rcoro::detail::RawVarTypeHelper<_rcoro_frame_t::fake, _rcoro_Marker, rawvarindex>( \
        ::new( \
            (void *)_rcoro_frame.template raw_var_storage_for_init<rawvarindex>(SF_CAT(_rcoro_fallback_storage_, ident).ptr()) \
        ) auto(__VA_ARGS__) \
    )
#define DETAIL_RCORO_VAR_GUARD(ident, rawvarindex, name) \
    ::rcoro::detail::RawVarGuard<_rcoro_frame_t, rawvarindex> SF_CAT(_rcoro_var_guard_, name)(_rcoro_jump_to == 0 || _rcoro_frame.template raw_var_exists<rawvarindex>() ? &_rcoro_frame : nullptr, SF_CAT(_rcoro_fallback_storage_, ident).ptr())
#define DETAIL_RCORO_VAR_REF(ident, rawvarindex, name) \
    auto &RCORO_RESTRICT name = _rcoro_frame.template raw_var_or_bad_ref<rawvarindex>(_rcoro_jump_to == 0, SF_CAT(_rcoro_fallback_storage_, ident).ptr())
#define DETAIL_RCORO_VAR_META(rawvarindex, markers) \
    /* Analyze lifetime overlap with other variables. */\
    (void)::rcoro::detail::RawVarVarReachWriter<_rcoro_frame_t::fake, _rcoro_Marker, rawvarindex, ::std::array<bool, rawvarindex>{DETAIL_RCORO_EXPAND_MARKERS markers}>{};
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_yield(ident, yieldindex, rawvarindex, markers, name) \
    do \
    { \
        /* Analyze lifetime overlap with other variables. */\
        (void)::rcoro::detail::RawVarYieldReachWriter<_rcoro_frame_t::fake, _rcoro_Marker, yieldindex DETAIL_RCORO_LEADING_COMMA(DETAIL_RCORO_EXPAND_MARKERS markers)>{}; \
        /* Remember the position. */\
        _rcoro_frame.pos = yieldindex; \
        /* Pause. */\
        _rcoro_frame.state = ::rcoro::detail::State::pausing; \
        return; \
      SF_CAT(_rcoro_label_, ident): \
        if (_rcoro_jump_to != 0) \
        { \
            if (_rcoro_jump_to == yieldindex) \
                /* Finish jumping. */\
                _rcoro_jump_to = 0; \
            else \
                /* Jump to the next location. */\
                goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
        } \
    } \
    while (false)
// The code inserted after the loop.
#define DETAIL_RCORO_CODEGEN_LOOP_FINAL(n, d) DETAIL_RCORO_CALL(DETAIL_RCORO_CODEGEN_LOOP_FINAL_, DETAIL_RCORO_IDENTITY d)
#define DETAIL_RCORO_CODEGEN_LOOP_FINAL_(ident, yieldindex, rawvarindex, markers) SF_CAT(_rcoro_label_, ident): ;

// The loop body to generate helper variables to analyze which variables are visible from which yield points.
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_MARKERVARS_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_code(ident, yieldindex, rawvarindex, markers, ...)
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_var(ident, yieldindex, rawvarindex, markers, name, ...) [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = false;
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_withvar(ident, yieldindex, rawvarindex, markers, name, ...) [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = false;
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_yield(ident, yieldindex, rawvarindex, markers, ...)

// The loop body to generate variable descriptions for reflection.
#define DETAIL_RCORO_VARDESC_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_VARDESC_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_code(ident, yieldindex, rawvarindex, markers, ...)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_var(ident, yieldindex, rawvarindex, markers, name, ...) , ::rcoro::detail::ValueTag<::rcoro::const_string(#name)>
#define DETAIL_RCORO_VARDESC_LOOP_BODY_withvar(ident, yieldindex, rawvarindex, markers, name, ...) , ::rcoro::detail::ValueTag<::rcoro::const_string(#name)>
#define DETAIL_RCORO_VARDESC_LOOP_BODY_yield(ident, yieldindex, rawvarindex, markers, ...)

// The loop body to generate yield point descriptions for reflection.
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_YIELDDESC_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_code(ident, yieldindex, rawvarindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_var(ident, yieldindex, rawvarindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_withvar(ident, yieldindex, rawvarindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_yield(ident, yieldindex, rawvarindex, markers, name) , ::rcoro::detail::ValueTag<::rcoro::const_string("" name "")>

#endif
