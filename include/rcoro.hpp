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
#include <iterator>
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
#ifndef RC_ASSERT
#include <cassert>
#define RC_ASSERT(expr) assert(expr)
#endif

// Aborts the program.
#ifndef RC_ABORT
#define RC_ABORT(text) do {assert(false && text); std::terminate();} while(false)
#endif

// 'Restrict' qualifier.
#ifndef RC_RESTRICT
#define RC_RESTRICT __restrict // There's also `__restrict__`, but that doesn't work on MSVC.
#endif

// An assumption.
#ifndef RC_ASSUME
#if defined(__clang__)
#define RC_ASSUME(...) __builtin_assume(__VA_ARGS__)
#elif defined(_MSC_VER)
#define RC_ASSUME(...) __assume(__VA_ARGS__)
#else
#define RC_ASSUME(...) (__VA_ARGS__ ? void() : __builtin_unreachable())
#endif
#endif

// Silences GCC's silly `-Wnon-template-friend` warning.
#ifndef DETAIL_RCORO_TEMPLATE_FRIEND
#if defined(__GNUC__) && !defined(__clang__)
#define DETAIL_RCORO_TEMPLATE_FRIEND(...) \
    _Pragma("GCC diagnostic push") \
    _Pragma("GCC diagnostic ignored \"-Wnon-template-friend\"") \
    __VA_ARGS__ \
    _Pragma("GCC diagnostic pop")
#else
#define DETAIL_RCORO_TEMPLATE_FRIEND(...) __VA_ARGS__
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

    // Why the coroutine finished executing.
    enum class finish_reason
    {
        // Don't reorder the first two, this allows casting booleans to `finish_reason`.
        not_finished = 0, // We're not actually finsihed.
        reset = 1, // Wasn't executing in the first place.
        success, // Finished normally.
        exception, // Finished because of an exception.
        _count,
    };

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
            _busy = std::underlying_type_t<finish_reason>(finish_reason::_count),
            running = _busy, // Currently running.
            pausing, // In the process of being paused.
        };

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

        // Transforms a runtime index to a compile-time one. UB if out of bounds.
        // `func` is `?? func(int i)`. It's called with the current yield point index, if any.
        template <auto N, typename F>
        constexpr void with_const_index(decltype(N) i, F &&func)
        {
            RC_ASSERT(i >= 0 && i < N);
            if constexpr (N > 0)
            {
                constexpr auto arr = []<decltype(N) ...I>(std::integer_sequence<decltype(N), I...>){
                    return std::array<void (*)(F &&), N>{+[](F &&func){std::forward<F>(func)(std::integral_constant<decltype(N), I>{});}...};
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
        template <typename T>
        struct NumVars : std::integral_constant<int, T::_rcoro_vars::size> {};
        // Returns the name of a single variable. `::value` is a `const_strict`.
        template <typename T, int N>
        using VarName = typename TypeAt<N, typename T::_rcoro_vars>::type;

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
        struct VarTypeReader
        {
            DETAIL_RCORO_TEMPLATE_FRIEND(
            friend constexpr auto _adl_detail_rcoro_var_type(VarTypeReader<Fake, T, N>);
            )
        };
        constexpr void _adl_detail_rcoro_var_type() {} // Dummy ADL target.
        template <bool Fake, typename T, int N>
        using MaybeTentativeVarType = std::remove_pointer_t<decltype(_adl_detail_rcoro_var_type(VarTypeReader<Fake, T, N>{}))>;
        template <typename T, int N>
        using VarType = MaybeTentativeVarType<false, T, N>;
        template <typename T, int N>
        using TentativeVarType = MaybeTentativeVarType<true, T, N>;
        template <bool Fake, typename T, int N, typename U>
        struct VarTypeWriter
        {
            friend constexpr auto _adl_detail_rcoro_var_type(VarTypeReader<Fake, T, N>) {return (U *)nullptr;}
        };
        template <typename T, int N, typename U>
        struct VarTypeWriter<false, T, N, U>
        {
            // Make sure the new type is sufficiently similar to the tentative one.
            static_assert(sizeof(U) == sizeof(TentativeVarType<T, N>) && alignof(U) == alignof(TentativeVarType<T, N>) && std::is_empty_v<U> == std::is_empty_v<TentativeVarType<T, N>>,
                "You're doing something really stupid with the variable type.");
            friend constexpr auto _adl_detail_rcoro_var_type(VarTypeReader<false, T, N>) {return (U *)nullptr;}
        };
        template <bool Fake, typename T, int N>
        struct VarTypeHelper
        {
            template <typename U, typename = decltype(void(VarTypeWriter<Fake, T, N, std::decay_t<U>>{}))>
            constexpr VarTypeHelper(U *) {}
        };

        // Stateful trick to store var-to-var overlap data.
        template <typename T, int N>
        struct VarVarReachReader
        {
            DETAIL_RCORO_TEMPLATE_FRIEND(
            friend constexpr auto _adl_detail_rcoro_var_var_reach(VarVarReachReader<T, N>);
            )
        };
        template <bool Write, typename T, int N, auto M>
        struct VarVarReachWriter
        {
            friend constexpr auto _adl_detail_rcoro_var_var_reach(VarVarReachReader<T, N>) {return M;}
        };
        template <typename T, int N, auto M>
        struct VarVarReachWriter<false, T, N, M> {};
        constexpr void _adl_detail_rcoro_var_var_reach() {} // Dummy ADL target.
        template <typename T, int A, int B>
        struct VarVarReach : std::bool_constant<_adl_detail_rcoro_var_var_reach(VarVarReachReader<T, A>{})[B]> {};
        template <typename T, int A>
        struct VarVarReach<T, A, A> : std::true_type {};
        template <typename T, int A, int B> requires(A < B)
        struct VarVarReach<T, A, B> : VarVarReach<T, B, A> {};

        // Stateful trick to store var-to-yield overlap data.
        template <typename T, int N>
        struct VarYieldReachReader
        {
            DETAIL_RCORO_TEMPLATE_FRIEND(
            friend constexpr auto _adl_detail_rcoro_var_yield_reach(VarYieldReachReader<T, N>);
            )
        };
        template <bool Write, typename T, int N, auto M>
        struct VarYieldReachWriter
        {
            friend constexpr auto _adl_detail_rcoro_var_yield_reach(VarYieldReachReader<T, N>) {return M;}
        };
        template <typename T, int N, auto M>
        struct VarYieldReachWriter<false, T, N, M> {};
        constexpr void _adl_detail_rcoro_var_yield_reach() {} // Dummy ADL target.
        template <typename T, int V, int Y>
        struct VarYieldReach : std::bool_constant<(
            _adl_detail_rcoro_var_yield_reach(VarYieldReachReader<T, Y>{}).size() > V
            && _adl_detail_rcoro_var_yield_reach(VarYieldReachReader<T, Y>{})[V]
        )> {};
        // A special case for the 0-th implicit yield point.
        template <typename T, int V>
        struct VarYieldReach<T, V, 0> : std::false_type {};
        // An array of all variables reachable from a yield point.
        template <typename T, int Y>
        constexpr auto vars_reachable_from_yield()
        {
            return []<int ...V>(std::integer_sequence<int, V...>){
                std::array<int, (VarYieldReach<T, V, Y>::value + ... + 0)> ret{};
                int pos = 0;
                ((VarYieldReach<T, V, Y>::value ? void(ret[pos++] = V) : void()), ...);
                return ret;
            }(std::make_integer_sequence<int, NumVars<T>::value>{});
        }

        // Stateful trick to store variable offsets.
        template <typename T, int N>
        struct VarOffsetReader
        {
            DETAIL_RCORO_TEMPLATE_FRIEND(
            friend constexpr std::size_t _adl_detail_rcoro_var_offset(VarOffsetReader<T, N>);
            )
        };
        template <typename T, int N, std::size_t M>
        struct VarOffsetWriter
        {
            friend constexpr std::size_t _adl_detail_rcoro_var_offset(VarOffsetReader<T, N>)
            {
                return M;
            }
        };
        constexpr void _adl_detail_rcoro_var_offset() {} // Dummy ADL target.
        template <typename T, int N>
        struct VarOffset : std::integral_constant<std::size_t, _adl_detail_rcoro_var_offset(VarOffsetReader<T, N>{})> {};

        // Determines the variable offset automatically, and writes it to the stateful storage.
        template <bool Write, typename T, int N, int M = N>
        struct VarOffsetWriterAuto {};
        template <typename T, int N, int M>
        struct VarOffsetWriterAuto<true, T, N, M> : VarOffsetWriterAuto<true, T, N, M-1> {};
        template <typename T, int N>
        struct VarOffsetWriterAuto<true, T, N, 0> : VarOffsetWriter<T, N, 0> {};
        template <typename T, int N, int M> requires(M > 0 && VarVarReach<T, N, M-1>::value)
        struct VarOffsetWriterAuto<true, T, N, M> : VarOffsetWriter<T, N,
            (
                VarOffset<T, M-1>::value
                + sizeof(TentativeVarType<T, M-1>)
                // Prepare to divide, rounding up.
                + alignof(TentativeVarType<T, N>)
                - 1
            )
            / alignof(TentativeVarType<T, N>)
            * alignof(TentativeVarType<T, N>)>
        {};

        // The stack frame alignment.
        template <typename T>
        struct FrameAlignment : std::integral_constant<std::size_t, []<int ...I>(std::integer_sequence<int, I...>){
            return std::max({alignof(char), alignof(TentativeVarType<T, I>)...});
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        // The stack frame size.
        template <typename T>
        struct FrameSize : std::integral_constant<std::size_t, 0> {};
        template <typename T> requires(NumVars<T>::value > 0)
        struct FrameSize<T> : std::integral_constant<std::size_t,
            (
                VarOffset<T, NumVars<T>::value - 1>::value
                + sizeof(TentativeVarType<T, NumVars<T>::value - 1>)
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
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        template <typename T>
        struct FrameIsMoveConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_move_constructible_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        template <typename T>
        struct FrameIsNothrowCopyConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_nothrow_copy_constructible_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        template <typename T>
        struct FrameIsNothrowMoveConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_nothrow_move_constructible_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        template <typename T>
        struct FrameIsTriviallyCopyable : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_trivially_copyable_v<TentativeVarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};

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
            static constexpr bool fake = Fake;
            using marker_t = T;

            // The state enum.
            State state = State::reset;
            // The current yield point.
            int pos = 0;

            // Returns true if the variable `V` exists at the current yield point.
            template <int V>
            constexpr bool var_exists() const
            {
                bool ret = false;
                if (pos != 0) // Not strictly necessary, hopefully an optimization.
                {
                    with_const_index<NumYields<T>::value>(pos, [&](auto yieldindex)
                    {
                        ret = VarYieldReach<T, V, yieldindex.value>::value;
                    });
                }
                return ret;
            }

            // Get `V`th variable storage, as a void pointer. We need a separate version for `void *`,
            // because when this is called, the actual variable type isn't known yet.
            template <int V> constexpr       void *var_storage_untyped()       {return this->storage() + VarOffset<T, V>::value;}
            template <int V> constexpr const void *var_storage_untyped() const {return this->storage() + VarOffset<T, V>::value;}
            // Get `V`th variable storage. Not laundered, so don't dereference.
            template <int V> constexpr       VarType<T, V> *var_storage()       {return reinterpret_cast<      VarType<T, V> *>(var_storage_untyped<V>());}
            template <int V> constexpr const VarType<T, V> *var_storage() const {return reinterpret_cast<const VarType<T, V> *>(var_storage_untyped<V>());}

            // Get `V`th variable. UB if it doesn't exist.
            template <int V> constexpr       VarType<T, V> &var()       {return *std::launder(var_storage<V>());}
            template <int V> constexpr const VarType<T, V> &var() const {return *std::launder(var_storage<V>());}

            // Get `V`th variable. An invalid reference if it doesn't exist and if `assume_good` is false.
            template <int V>
            constexpr VarType<T, V> &var_or_bad_ref(bool assume_good)
            {
                if (assume_good || var_exists<V>())
                    return var<V>();
                else
                    return bad_ref<VarType<T, V>>();
            }
            template <int V>
            constexpr const VarType<T, V> &var_or_bad_ref() const
            {
                return const_cast<Frame *>(this)->var_or_bad_ref();
            }

            // Cleans the frame.
            constexpr void reset() noexcept
            {
                if (state >= State::_busy)
                    RC_ABORT("Can't reset a busy coroutine.");
                state = State::reset;
                if (pos == 0) // Not strictly necessary, hopefully an optimization.
                    return;
                with_const_index<NumYields<T>::value>(pos, [&](auto yieldindex)
                {
                    constexpr auto indices = vars_reachable_from_yield<T, yieldindex.value>();
                    const_reverse_for<indices.size()>([&](auto varindex)
                    {
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

            // `func` is `bool func(auto index)`, `rollback` is `void rollback(auto index)`, where `index` is `std::integral_constant<int, I>`.
            // `func` is called for every variable index for yield point `pos`.
            // If it throws or returns true, `rollback` is called for every index processed so far, in reverse.
            // If `func` returned true, the whole function also returns true.
            template <typename F, typename G>
            static constexpr bool handle_vars(int pos, F &&func, G &&rollback)
            {
                if (pos == 0) // Not strictly necessary, hopefully an optimization.
                    return false;
                bool ret = false;
                with_const_index<NumYields<T>::value>(pos, [&](auto yield)
                {
                    constexpr auto indices = vars_reachable_from_yield<T, yield.value>();

                    HandleVarsGuard<G, yield.value> guard{.rollback = rollback};

                    bool fail = const_any_of<indices.size()>([&](auto var)
                    {
                        bool stop = func(std::integral_constant<int, indices[var.value]>{});
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
                    RC_ABORT("Can't copy a busy coroutine.");
                reset();
                handle_vars(other.pos,
                    [&](auto index)
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
                    RC_ABORT("Can't move a busy coroutine.");

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
                    [&](auto index)
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
            using marker_t = T;

            State state = State::reset;
            int pos = 0;

            template <int V>
            constexpr bool var_exists()
            {
                return false;
            }
            template <int V>
            constexpr TentativeVarType<T, V> &var()
            {
                return bad_ref<TentativeVarType<T, V>>();
            }
            template <int V>
            constexpr void *var_storage_untyped()
            {
                return nullptr;
            }
            template <int V>
            constexpr TentativeVarType<T, V> &var_or_bad_ref(bool assume_good)
            {
                (void)assume_good;
                return bad_ref<TentativeVarType<T, V>>();
            }
        };

        // Creates a variable in a frame in its constructor, and destroys it in the destructor.
        template <typename Frame, int I>
        struct VarGuard
        {
            Frame *frame = nullptr;

            constexpr VarGuard(Frame *frame) : frame(frame) {}
            VarGuard(const VarGuard &) = delete;
            VarGuard &operator=(const VarGuard &) = delete;
            constexpr ~VarGuard()
            {
                if (frame && frame->state != State::pausing)
                    std::destroy_at(&frame->template var<I>());
            }
        };
        // We need a separate fake version, because `var<I>()` isn't ready yet, because the actual variable type isn't determined yet.
        template <typename Frame, int I> requires Frame::fake
        struct VarGuard<Frame, I>
        {
            constexpr VarGuard(Frame *) {}
            VarGuard(const VarGuard &) = delete;
            VarGuard &operator=(const VarGuard &) = delete;
        };

        // An array of pairs, mapping variable names to their indices.
        template <typename T>
        constexpr auto var_name_to_index_mapping = []{
            std::array<std::pair<std::string_view, int>, NumVars<T>::value> ret{};
            const_for<NumVars<T>::value>([&](auto index)
            {
                constexpr int i = index.value;
                ret[i].first = VarName<T, i>::value.view();
                ret[i].second = i;
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

    // Whether `T` is a template argument of `coro<??>`.
    // Can also be obtained from `coro<??>` as `::tag`.
    template <typename T>
    concept tag = requires
    {
        typename T::_rcoro_marker_t;
        typename T::_rcoro_frame_t;
        typename T::_rcoro_lambda_t;
    };


    // Examining variables:

    // The number of variables in a coroutine.
    template <tag T>
    constexpr int num_vars = detail::NumVars<typename T::_rcoro_marker_t>::value;

    // The name of a coroutine variable `V`.
    template <tag T, int V> requires(V >= 0 && V < num_vars<T>)
    constexpr const_string var_name_const = detail::VarName<typename T::_rcoro_marker_t, V>::value;

    // The type of a coroutine variable `V`.
    template <tag T, int V>
    using var_type = typename detail::VarType<typename T::_rcoro_marker_t, V>;

    // Variable name helpers:

    // Converts a variable name to its index.
    // Returns a negative error code on failure: either `unknown_name` or `ambiguous_name`.
    template <tag T>
    [[nodiscard]] constexpr int var_index_or_negative(std::string_view name)
    {
        const auto &arr = detail::var_name_to_index_mapping<typename T::_rcoro_marker_t>;
        auto it = std::partition_point(arr.begin(), arr.end(), [&](const auto &pair){return pair.first < name;});
        if (it == arr.end() || it->first != name)
            return unknown_name;
        auto next_it = std::next(it);
        if (next_it != arr.end() && next_it->first == name)
            return ambiguous_name;
        return it->second;
    }
    // Same, but throws on failure.
    template <tag T>
    [[nodiscard]] constexpr int var_index(std::string_view name)
    {
        int ret = var_index_or_negative<T>(name);
        if (ret == ambiguous_name)
            throw std::runtime_error("Ambiguous coroutine variable name: `" + std::string(name) + "`.");
        if (ret < 0)
            throw std::runtime_error("Unknown coroutine variable name: `" + std::string(name) + "`.");
        return ret;
    }
    // Same, but works with constexpr strings, and causes a SOFT compilation error if the name is invalid.
    template <tag T, const_string Name>
    requires(var_index_or_negative<T>(Name.view()) >= 0)
    constexpr int var_index_const = var_index_or_negative<T>(Name.view());

    // Given a variable index, returns its name. Same as `var_name_const`, but with a possibly dynamic index.
    // Throws if the index is out of range.
    template <tag T>
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


    // Examining yield points:

    // The number of yield points.
    template <tag T>
    constexpr int num_yields = detail::NumYields<typename T::_rcoro_marker_t>::value;

    // The name of a yield point.
    template <tag T, int Y> requires(Y >= 0 && Y < num_yields<T>)
    constexpr const_string yield_name_const = detail::YieldName<typename T::_rcoro_marker_t, Y>::value;

    // Whether variable `V` exists at yield point `Y`.
    template <tag T, int V, int Y>
    constexpr bool var_lifetime_overlaps_yield = detail::VarYieldReach<typename T::_rcoro_marker_t, V, Y>::value;

    // A list of variables existing at yield point `Y`, of type `std::array<int, N>`.
    template <tag T, int Y>
    constexpr auto yield_vars = detail::vars_reachable_from_yield<typename T::_rcoro_marker_t, Y>();

    // Yield point name helpers:

    // Converts a yield point name to its index.
    // Returns a negative error code on failure: either `unknown_name` or `ambiguous_name`.
    template <tag T>
    [[nodiscard]] constexpr int yield_index_or_negative(std::string_view name)
    {
        const auto &arr = detail::yield_name_to_index_mapping<typename T::_rcoro_marker_t>;
        auto it = std::partition_point(arr.begin(), arr.end(), [&](const auto &pair){return pair.first < name;});
        if (it == arr.end() || it->first != name)
            return unknown_name;
        auto next_it = std::next(it);
        if (next_it != arr.end() && next_it->first == name)
            return ambiguous_name;
        return it->second;
    }
    // Same, but throws on failure.
    template <tag T>
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
    template <tag T, const_string Name>
    requires(yield_index_or_negative<T>(Name.view()) >= 0)
    constexpr int yield_index_const = yield_index_or_negative<T>(Name.view());

    // Given a yield point index, returns its name. Same as `yield_name_const`, but with a possibly dynamic index.
    // Throws if the index is out of range.
    template <tag T>
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
    template <tag T>
    constexpr bool yields_names_are_unique = []{
        std::array<std::string_view, num_yields<T>> arr;
        for (int i = 0; i < num_yields<T>; i++)
            arr[i] = yield_name<T>(i);
        std::sort(arr.begin(), arr.end());
        return std::adjacent_find(arr.begin(), arr.end()) == arr.end();
    }();


    // Examining low-level variable layout:

    // Returns the stack frame size of the coroutine.
    template <tag T>
    constexpr std::size_t frame_size = detail::FrameSize<typename T::_rcoro_marker_t>::value;
    // Returns the stack frame alignment of the coroutine.
    template <tag T>
    constexpr std::size_t frame_alignment = detail::FrameAlignment<typename T::_rcoro_marker_t>::value;
    // If true, the coroutine only uses `std::is_trivially_copyable` variables, meaning its frame can be freely copied around as bytes.
    template <tag T>
    constexpr bool frame_is_trivially_copyable = detail::FrameIsTriviallyCopyable<typename T::_rcoro_marker_t>::value;

    // The offset of variable `V` in the stack frame.
    // Different variables can overlap if `var_lifetime_overlaps_var` is false for them.
    template <tag T, int V>
    constexpr std::size_t var_offset = detail::VarOffset<typename T::_rcoro_marker_t, V>::value;

    // Whether variables `A` and `B` have overlapping lifetime.
    template <tag T, int A, int B>
    constexpr bool var_lifetime_overlaps_var = detail::VarVarReach<typename T::_rcoro_marker_t, A, B>::value;


    // Misc:

    struct rewind_tag_t {};
    // Pass this to the constructor of `coro<T>` to automatically rewind the coroutine.
    // `coro<T>{}.rewind()` isn't enough, because it requires moveability.
    // This is what `RCORO()` uses internally.
    constexpr rewind_tag_t rewind{};


    // The coroutine class.
    template <tag T>
    class coro
    {
        typename T::_rcoro_frame_t frame;

      public:
        using tag = T;

        // Constructs a finished coroutine, with `finish_reason() == reset`.
        constexpr coro() {}
        // Constructs a ready coroutine.
        constexpr coro(rewind_tag_t) {rewind();}

        // Copyable and movable, assuming all the elements are.
        // Copying, moving, and destroying aborts the program if any of the involved coroutines are `busy()`.
        // Can't use exceptions for those errors, because then we lose `noexcept`ness (and using them only if `noexcept` is missing is lame).

        // Resets the coroutine to the initial position.
        // After this, `finished() == false` and `yield_point() == 0`.
        constexpr coro  &rewind() &  {reset(); frame.state = detail::State::not_finished; return *this;}
        constexpr coro &&rewind() && {rewind(); return std::move(*this);}

        // Examining coroutine state:

        // Each coroutine can be either finished or not finished.
        // If finished, it'll have a `finish_reason()` set, and will have `yield_point() == 0`.
        // If not finished, it can either be `busy()` (currently executing) or not, and will have `yield_point() >= 0 && yield_point() < num_yields<T>`.
        // Yield point 0 is special, it's automatically inserted at the beginning of a coroutine.

        // Same as `!finished()`.
        [[nodiscard]] explicit constexpr operator bool() const noexcept {return !finished();}

        // Returns true if the coroutine is currently running and can't be manipulated.
        [[nodiscard]] constexpr bool busy() const noexcept {return frame.state >= detail::State::_busy;}

        // Returns true if the coroutine has finished executing.
        [[nodiscard]] constexpr bool finished() const noexcept {return !busy() && frame.state != detail::State::not_finished;}
        // Returns the reason why the coroutine has finished executing, or `not_finished` if not actually finished.
        [[nodiscard]] constexpr rcoro::finish_reason finish_reason() const noexcept {return finished() ? rcoro::finish_reason(frame.state) : finish_reason::not_finished;}

        // Returns true if this coroutine can be resumed.
        // Same as `!finished() && !busy()`.
        [[nodiscard]] constexpr bool can_resume() const noexcept {return frame.state == detail::State::not_finished;}

        // Manipulating the coroutine:

        // Runs a single step of the coroutine.
        // Returns `*this`. Note that the return value is convertible to bool, returning `!finished()`.
        // Throws if `can_resume() == false`.
        template <typename ...P> requires std::is_invocable_v<typename T::_rcoro_lambda_t, typename T::_rcoro_frame_t &, int, P...>
        constexpr coro &operator()(P &&... params) &
        {
            if (!can_resume())
                throw std::runtime_error("This coroutine can't be resumed. It's either finished or busy.");

            frame.state = detail::State::running;

            bool had_exception = true;
            struct Guard
            {
                coro &self;
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

            typename T::_rcoro_lambda_t{}(frame, jump_to, std::forward<P>(params)...);

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
        template <typename ...P> requires std::is_invocable_v<typename T::_rcoro_lambda_t, typename T::_rcoro_frame_t &, int, P...>
        constexpr coro &&operator()(P &&... params) &&
        {
            operator()(std::forward<P>(params)...);
            return std::move(*this);
        }

        // Zeroes the coroutine, destroying all variables. Throws if `busy()`. Returns `*this`.
        constexpr coro &reset() &
        {
            if (busy())
                throw std::runtime_error("Can't reset a busy coroutine.");
            frame.reset();
            return *this;
        }
        constexpr coro &&reset() &&
        {
            reset();
            return std::move(*this);
        }


        // Variable reflection:

        // Returns true if the variable currently exists.
        // Throws if `busy()`.
        template <int V>
        [[nodiscard]] constexpr bool var_exists() const
        {
            if (busy())
                throw std::runtime_error("Can't manipulate variables in a busy coroutine.");
            return frame.template var_exists<V>();
        }
        template <const_string Name>
        [[nodiscard]] constexpr bool var_exists() const
        {
            return var_exists<var_index_const<T, Name>>();
        }

        // Returns a variable.
        // Throws if `busy()`, or if the variable doesn't exist at the current yield point.
        template <int V>
        [[nodiscard]] constexpr var_type<T, V> &var()
        {
            if (busy())
                throw std::runtime_error("Can't manipulate variables in a busy coroutine.");
            if (!frame.template var_exists<V>())
                throw std::runtime_error("The coroutine variable `" + std::string(var_name_const<T, V>.view()) + "` doesn't exist at this point.");
            return frame.template var<V>();
        }
        template <int V>
        [[nodiscard]] constexpr const var_type<T, V> &var() const
        {
            return const_cast<coro *>(this)->var<V>();
        }
        template <const_string Name> [[nodiscard]] constexpr       var_type<T, var_index_const<T, Name>> &var()       {return var<var_index_const<T, Name>>();}
        template <const_string Name> [[nodiscard]] constexpr const var_type<T, var_index_const<T, Name>> &var() const {return var<var_index_const<T, Name>>();}


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
            return yield_name<T>(frame.pos);
        }

        // Calls a function for each alive variable. Throws if `busy()`. Does nothing if finished or paused at the implicit 0-th yield point.
        // `func` is `bool func(auto index)`, where `index.value` is the constexpr variable index.
        // Use `.var<i>()` and `rcoro::var_name_const<i>` to then manipulate the variables.
        // If `func` returns true, stops immeidately and also returns true.
        template <typename F>
        constexpr bool for_each_alive_var(F &&func) const
        {
            if (busy())
                throw std::runtime_error("Can't manipulate variables in a busy coroutine.");
            bool ret = false;
            if (frame.pos != 0) // Not strictly necessary, hopefully an optimization.
            {
                detail::with_const_index<num_yields<T>>(frame.pos, [&](auto yieldindex)
                {
                    constexpr auto indices = yield_vars<T, yieldindex.value>;
                    ret = detail::const_any_of<indices.size()>([&](auto varindex)
                    {
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
        // `func` is supposed to load the variables into `stack_frame()`, or throw or return false on failure.
        // Applies `fin_reason` and `yield_index`, then returns true.
        // The `func` can be empty and always return true, if you load the variables beforehand.
        template <typename F>
        requires frame_is_trivially_copyable<T>
        constexpr bool load_uninitialized(rcoro::finish_reason fin_reason, int yield_index, F &&func)
        {
            return load_uninitialized_UNSAFE(fin_reason, yield_index, std::forward<F>(func));
        }

        // Same as `load_uninitialized`, but compiles for any variable types (compiles even if `frame_is_trivially_copyable<T>` is false).
        // Warning! If `func` returns true, it must placement-new all the `yield_vars` into the `frame_storage()`, otherwise UB ensues.
        // And if `func` returns false or throws, it must not leave any variables alive.
        template <typename F>
        constexpr bool load_uninitialized_UNSAFE(rcoro::finish_reason fin_reason, int yield_index, F &&func)
        {
            reset();
            if (fin_reason < rcoro::finish_reason{} || fin_reason >= rcoro::finish_reason::_count)
                throw std::runtime_error("Coroutine finish reason is out of range.");
            if (yield_index < 0 || yield_index >= num_yields<T>)
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
        // Throws if `fin_reason` is out of range.
        // If `fin_reason != not_finished`, throws if `yield_index` is not `0`.
        // `func` is then called for every variable existing at that yield point.
        // `func` is `void func(auto index, auto construct)`, where `index` is the variable index in form of `std::integral_constant<int, N>`,
        // and `construct` is `void construct(auto &&...)`. `construct` must be called at most once to construct the variable. Throws if it's called more than once.
        // If `construct` is not called, will abort the load, destroy the previously constructed variables, and return false.
        // If the function ends up throwing or returning false, the coroutine is zeroed as if by `reset()`.
        template <typename F>
        constexpr bool load(rcoro::finish_reason fin_reason, int yield_index, F &&func)
        {
            reset();

            bool fail = frame.handle_vars(
                yield_index,
                [&](auto varindex)
                {
                    bool ok = false;
                    func(varindex, [&]<typename ...P>(P &&... params)
                    {
                        if (ok)
                            throw std::runtime_error("Coroutine `load()` user callback must call the callback it receives at most once.");
                        std::construct_at(frame.template var_storage<varindex.value>(), std::forward<P>(params)...);
                        ok = true;
                    });
                    return !ok;
                },
                [&](auto varindex) noexcept
                {
                    std::destroy_at(&frame.template var<varindex.value>());
                }
            );
            if (!fail)
            {
                frame.pos = yield_index;
                frame.state = detail::State(fin_reason);
            }
            return !fail;
        }


        // Debug info printer.
        template <typename A, typename B>
        friend std::basic_ostream<A, B> &operator<<(std::basic_ostream<A, B> &s, coro &c)
        {
            if (c.finished())
            {
                s << "finish_reason = ";
                switch (c.finish_reason())
                {
                    case finish_reason::not_finished: RC_ASSERT(false); break;
                    case finish_reason::reset:        s << "reset";      break;
                    case finish_reason::success:      s << "success";   break;
                    case finish_reason::exception:    s << "exception"; break;
                    case finish_reason::_count:       RC_ASSERT(false); break;
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

            detail::const_for<num_vars<T>>([&](auto varindex)
            {
                constexpr int i = varindex.value;
                s << "\n  " << i << ". " << var_name_const<T, i>.view() << " - " << (c.template var_exists<i>() ? "alive" : "dead"); // GCC 11 needs `template` here.
                if constexpr (detail::Printable<var_type<T, i>, std::basic_ostream<A, B>>)
                {
                    if (c.template var_exists<i>()) // GCC 11 needs `template` here.
                        s << ", " << c.template var<i>(); // GCC 11 needs `template` here.
                }
            });

            return s;
        }
    };


    // Debug info.

    // Print `debug_info<tag>` to an `std::ostream` to get a debug information about a type.
    template <tag T>
    struct debug_info_t
    {
        template <typename A, typename B>
        friend std::basic_ostream<A, B> &operator<<(std::basic_ostream<A, B> &s, debug_info_t)
        {
            s << "copying: ctor=" << (!std::is_copy_constructible_v<coro<T>> ? "no" : std::is_nothrow_copy_constructible_v<coro<T>> ? "yes(nothrow)" : "yes")
              << " assign=" << (!std::is_copy_assignable_v<coro<T>> ? "no" : std::is_nothrow_copy_assignable_v<coro<T>> ? "yes(nothrow)" : "yes") << '\n';
            s << "moving: ctor=" << (!std::is_move_constructible_v<coro<T>> ? "no" : std::is_nothrow_move_constructible_v<coro<T>> ? "yes(nothrow)" : "yes")
              << " assign=" << (!std::is_move_assignable_v<coro<T>> ? "no" : std::is_nothrow_move_assignable_v<coro<T>> ? "yes(nothrow)" : "yes") << '\n';

            s << "frame: size=" << frame_size<T> << " align=" << frame_alignment<T> << '\n';

            s << num_vars<T> << " variable" << (num_vars<T> != 1 ? "s" : "") << ":\n";
            detail::const_for<num_vars<T>>([&](auto varindex)
            {
                constexpr int i = varindex.value;
                s << "  " << i << ". " << var_name_const<T, i>.view() << ", " << detail::type_name<var_type<T, i>>() << '\n';
                s << "      offset=" << var_offset<T, i> << ", size=" << sizeof(var_type<T, i>) << ", align=" << alignof(var_type<T, i>) << '\n';

                bool first = true;
                detail::const_for<i>([&](auto sub_varindex)
                {
                    constexpr int j = sub_varindex.value;
                    if constexpr (var_lifetime_overlaps_var<T, i, j>)
                    {
                        if (first)
                        {
                            first = false;
                            s << "      visible_vars: ";
                        }
                        else
                            s << ", ";
                        s << j << "." << var_name_const<T, j>.view();
                    }
                });
                if (!first)
                    s << '\n';
            });

            s << num_yields<T> << " yield" << (num_yields<T> != 1 ? "s" : "") << ":\n";
            detail::const_for<num_yields<T>>([&](auto yieldindex)
            {
                constexpr int i = yieldindex.value;
                s << "  " << i << ". `" << yield_name_const<T, i>.view() << "`";
                constexpr auto vars = yield_vars<T, i>;
                detail::const_for<vars.size()>([&](auto varindex)
                {
                    constexpr int j = varindex.value;
                    if (j == 0)
                        s << ", visible_vars: ";
                    else
                        s << ", ";
                    s << j << "." << var_name_const<T, j>.view();
                });
                s << '\n';
            });

            return s;
        }
    };
    template <tag T>
    constexpr debug_info_t<T> debug_info;
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
// Creates a coroutine.
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
        auto _rcoro_lambda = [](auto &RC_RESTRICT _rcoro_frame, int _rcoro_jump_to DETAIL_RCORO_LEADING_COMMA(DETAIL_RCORO_GET_PAR(DETAIL_RCORO_GET_PAR((__VA_ARGS__))))) \
        { \
            RC_ASSUME(_rcoro_jump_to >= 0 && _rcoro_jump_to < _rcoro_Marker::_rcoro_yields::size); \
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
        (void)&decltype(_rcoro_lambda)::template operator()<::rcoro::detail::Frame<true, _rcoro_Marker>>; \
        /* The second pass instantiation. It would happen automatically when the coroutine is called, but then we miss out variable type information, */\
        /* which we can't properly collect in the first pass. See `VarTypeReader` and others for details. */\
        (void)&decltype(_rcoro_lambda)::template operator()<::rcoro::detail::Frame<false, _rcoro_Marker>>; \
        struct _rcoro_Types \
        { \
            using _rcoro_marker_t [[maybe_unused]] = _rcoro_Marker; \
            using _rcoro_frame_t [[maybe_unused]] = ::rcoro::detail::Frame<false, _rcoro_Marker>; \
            using _rcoro_lambda_t [[maybe_unused]] = decltype(_rcoro_lambda); \
        }; \
        return ::rcoro::coro<_rcoro_Types>(::rcoro::rewind); \
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
#define DETAIL_RCORO_LOOP_STEP_firstcode(ident, yieldindex, varindex, markers) (       ident    , yieldindex  , varindex  , markers)
#define DETAIL_RCORO_LOOP_STEP_code(ident, yieldindex, varindex, markers)      (       ident    , yieldindex  , varindex  , markers)
#define DETAIL_RCORO_LOOP_STEP_var(ident, yieldindex, varindex, markers)       (SF_CAT(ident, i), yieldindex  , varindex+1, (DETAIL_RCORO_IDENTITY markers, DETAIL_RCORO_MARKER_VAR_NAME(ident)))
#define DETAIL_RCORO_LOOP_STEP_withvar(ident, yieldindex, varindex, markers)   (SF_CAT(ident, i), yieldindex  , varindex+1, (DETAIL_RCORO_IDENTITY markers, DETAIL_RCORO_MARKER_VAR_NAME(ident)))
#define DETAIL_RCORO_LOOP_STEP_yield(ident, yieldindex, varindex, markers)     (SF_CAT(ident, i), yieldindex+1, varindex  , markers)

// The loop body for the function body generation.
#define DETAIL_RCORO_CODEGEN_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_CODEGEN_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_firstcode(ident, yieldindex, varindex, markers, ...) DETAIL_RCORO_SKIP_PAR(__VA_ARGS__) // Skip optional parameters.
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...) __VA_ARGS__
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) \
    /* Make sure we're not preceded by an if/for/while/etc. */\
    /* The test is performed by having two separate statements, where the latter needs the former to be visible to compile. */\
    /* Note that `[[maybe_unused]]` is not strictly necessary, but it removes an extra useless warning when this check fails. */\
    /* The rcorond variable also serves as a scope checker for yields. */\
    [[maybe_unused]] static constexpr bool SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))) = true; \
    [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))); \
    /* If we're not jumping, initialize the variable. */\
    DETAIL_RCORO_VAR_INIT(varindex, __VA_ARGS__); \
    /* Jump target. */\
  SF_CAT(_rcoro_label_, ident): \
    /* This will destroy the variable when it goes out of scope. */\
    DETAIL_RCORO_VAR_GUARD(varindex, name); \
    /* Create a reference as a fancy name for our storage variable. */\
    DETAIL_RCORO_VAR_REF(varindex, name); \
    /* Jump to the next macro, if necessary. */\
    if (_rcoro_jump_to != 0) \
    { \
        /* Jump to the next location. */\
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    /* Stateful meta magic. This can be placed anywhere. */\
    DETAIL_RCORO_VAR_META(varindex, markers) \
    /* Force trailing semicolon. */\
    void()
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_withvar(ident, yieldindex, varindex, markers, name, ...) \
    if (void(DETAIL_RCORO_VAR_INIT(varindex, __VA_ARGS__)), false) {} else \
  SF_CAT(_rcoro_label_, ident): \
    if ([[maybe_unused]] constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = true; false) {} else \
    if (DETAIL_RCORO_VAR_GUARD(varindex, name); false) {} else \
    if (DETAIL_RCORO_VAR_REF(varindex, name); _rcoro_jump_to != 0) \
    { \
        DETAIL_RCORO_VAR_META(varindex, markers) \
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    else
// Variable code pieces:
#define DETAIL_RCORO_VAR_INIT(varindex, ...) \
    ::rcoro::detail::VarTypeHelper<_rcoro_frame_t::fake, _rcoro_Marker, varindex>(::new((void *)_rcoro_frame.template var_storage_untyped<varindex>()) auto(__VA_ARGS__))
#define DETAIL_RCORO_VAR_GUARD(varindex, name) \
    ::rcoro::detail::VarGuard<_rcoro_frame_t, varindex> SF_CAT(_rcoro_var_guard_, name)(_rcoro_jump_to == 0 || _rcoro_frame.template var_exists<varindex>() ? &_rcoro_frame : nullptr)
#define DETAIL_RCORO_VAR_REF(varindex, name) \
    auto &RC_RESTRICT name = _rcoro_frame.template var_or_bad_ref<varindex>(_rcoro_jump_to == 0)
#define DETAIL_RCORO_VAR_META(varindex, markers) \
    /* Analyze lifetime overlap with other variables. */\
    (void)::rcoro::detail::VarVarReachWriter<_rcoro_frame_t::fake, _rcoro_Marker, varindex, ::std::array<bool, varindex>{DETAIL_RCORO_EXPAND_MARKERS markers}>{}; \
    /* Determine the stack frame offset for this variable. */\
    (void)::rcoro::detail::VarOffsetWriterAuto<_rcoro_frame_t::fake, _rcoro_Marker, varindex>{};
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_yield(ident, yieldindex, varindex, markers, name) \
    do \
    { \
        /* Analyze lifetime overlap with other variables. */\
        (void)::rcoro::detail::VarYieldReachWriter<_rcoro_frame_t::fake, _rcoro_Marker, yieldindex, ::std::array<bool, varindex>{DETAIL_RCORO_EXPAND_MARKERS markers}>{}; \
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
#define DETAIL_RCORO_CODEGEN_LOOP_FINAL_(ident, yieldindex, varindex, markers) SF_CAT(_rcoro_label_, ident): ;

// The loop body to generate helper variables to analyze which variables are visible from which yield points.
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_MARKERVARS_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = false;
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_withvar(ident, yieldindex, varindex, markers, name, ...) [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = false;
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)

// The loop body to generate variable descriptions for reflection.
#define DETAIL_RCORO_VARDESC_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_VARDESC_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) , ::rcoro::detail::ValueTag<::rcoro::const_string(#name)>
#define DETAIL_RCORO_VARDESC_LOOP_BODY_withvar(ident, yieldindex, varindex, markers, name, ...) , ::rcoro::detail::ValueTag<::rcoro::const_string(#name)>
#define DETAIL_RCORO_VARDESC_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)

// The loop body to generate yield point descriptions for reflection.
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_YIELDDESC_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_var(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_withvar(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_yield(ident, yieldindex, varindex, markers, name) , ::rcoro::detail::ValueTag<::rcoro::const_string("" name "")>

#endif
