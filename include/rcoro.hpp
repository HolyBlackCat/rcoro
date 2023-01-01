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
        none = 1, // Wasn't executing in the first place.
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
            none         = std::underlying_type_t<finish_reason>(finish_reason::none),
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
        template <typename T, int N>
        struct VarTypeReader
        {
            friend constexpr auto _adl_detail_rcoro_var_type(VarTypeReader<T, N>);
        };
        template <typename T, int N, typename U>
        struct VarTypeWriter
        {
            friend constexpr auto _adl_detail_rcoro_var_type(VarTypeReader<T, N>) {return (U *)nullptr;}
        };
        constexpr void _adl_detail_rcoro_var_type() {} // Dummy ADL target.
        template <typename T, int N>
        using VarType = std::remove_pointer_t<decltype(_adl_detail_rcoro_var_type(VarTypeReader<T, N>{}))>;

        // Stateful trick to store var-to-var overlap data.
        template <typename T, int N>
        struct VarVarReachReader
        {
            friend constexpr auto _adl_detail_rcoro_var_var_reach(VarVarReachReader<T, N>);
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
            friend constexpr auto _adl_detail_rcoro_var_yield_reach(VarYieldReachReader<T, N>);
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
                std::array<int, (VarYieldReach<T, V, Y>::value + ... + 0)> ret;
                int pos = 0;
                ((VarYieldReach<T, V, Y>::value ? void(ret[pos++] = V) : void()), ...);
                return ret;
            }(std::make_integer_sequence<int, NumVars<T>::value>{});
        }

        // Stateful trick to store variable offsets.
        template <typename T, int N>
        struct VarOffsetReader
        {
            friend constexpr std::size_t _adl_detail_rcoro_var_offset(VarOffsetReader<T, N>);
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
                + sizeof(VarType<T, M-1>)
                // Prepare to divide, rounding up.
                + alignof(VarType<T, M>)
                - 1
            )
            / alignof(VarType<T, M>)
            * alignof(VarType<T, M>)>
        {};

        // The stack frame alignment.
        template <typename T>
        struct FrameAlignment : std::integral_constant<std::size_t, []<int ...I>(std::integer_sequence<int, I...>){
            return std::max({alignof(char), alignof(VarType<T, I>)...});
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        // The stack frame size.
        template <typename T>
        struct FrameSize : std::integral_constant<std::size_t, 0> {};
        template <typename T> requires(NumVars<T>::value > 0)
        struct FrameSize<T> : std::integral_constant<std::size_t,
            (
                VarOffset<T, NumVars<T>::value - 1>::value
                + sizeof(VarType<T, NumVars<T>::value - 1>)
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
            return (std::is_copy_constructible_v<VarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        template <typename T>
        struct FrameIsMoveConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_move_constructible_v<VarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        template <typename T>
        struct FrameIsNothrowCopyConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return (std::is_nothrow_copy_constructible_v<VarType<T, I>> && ...);
        }(std::make_integer_sequence<int, NumVars<T>::value>{})> {};
        template <typename T>
        struct FrameIsNothrowCopyOrMoveConstructible : std::bool_constant<[]<int ...I>(std::integer_sequence<int, I...>){
            return ((std::is_nothrow_copy_constructible_v<VarType<T, I>> || std::is_nothrow_move_constructible_v<VarType<T, I>>) && ...);
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

            // Copying is no-op.
            constexpr FrameBase(const FrameBase &) {}
            constexpr FrameBase &operator=(const FrameBase &) {return *this;}

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
            State state = State::none;
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

            // Get `V`th variable storage. Not laundered, so don't dereference.
            template <int V> constexpr       VarType<T, V> *var_storage()       {return reinterpret_cast<      VarType<T, V> *>(this->storage() + VarOffset<T, V>::value);}
            template <int V> constexpr const VarType<T, V> *var_storage() const {return reinterpret_cast<const VarType<T, V> *>(this->storage() + VarOffset<T, V>::value);}

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
            void reset() noexcept
            {
                if (state >= State::_busy)
                    RC_ABORT("Can't reset a busy coroutine.");
                state = State::none;
                if (pos == 0) // Not strictly necessary, hopefully an optimization.
                    return;
                with_const_index<NumYields<T>::value>(pos, [&](auto yieldindex)
                {
                    constexpr auto indices = vars_reachable_from_yield<T, yieldindex.value>();
                    const_reverse_for<indices.size()>([&](auto varindex)
                    {
                        std::destroy_at(&var<varindex.value>());
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
            {
                *this = other;
            }
            constexpr Frame(Frame &&other) noexcept(FrameIsNothrowCopyOrMoveConstructible<T>::value) requires FrameIsMoveConstructible<T>::value
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
                        std::construct_at(var_storage<i>(), other.var<i>());
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
            constexpr Frame &operator=(Frame &&other) noexcept(FrameIsNothrowCopyOrMoveConstructible<T>::value) requires FrameIsMoveConstructible<T>::value
            {
                if (&other == this)
                    return *this;
                if (state >= State::_busy || other.state >= State::_busy)
                    RC_ABORT("Can't move a busy coroutine.");

                reset();

                struct Guard
                {
                    Frame &other;
                    ~Guard()
                    {
                        other.reset();
                    }
                };
                Guard guard{other};

                handle_vars(other.pos,
                    [&](auto index)
                    {
                        constexpr int i = index.value;
                        std::construct_at(var_storage<i>(), std::move_if_noexcept(other.var<i>()));
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

            State state = State::none;
            int pos = 0;

            template <int V>
            constexpr VarType<T, V> &var()
            {
                return bad_ref<VarType<T, V>>();
            }
            template <int V>
            constexpr VarType<T, V> &var_or_bad_ref(bool assume_good)
            {
                (void)assume_good;
                return bad_ref<VarType<T, V>>();
            }
        };

        // Creates a variable in a frame in its constructor, and destroys it in the destructor.
        template <typename Frame, int I>
        struct VarGuard
        {
            const Frame *frame;

            template <typename T>
            constexpr VarGuard(const Frame *frame, T &&init) : frame(frame)
            {
                if (frame)
                    std::construct_at(frame->template var_storage<I>(), std::forward<T>(init));
            }
            VarGuard(const VarGuard &) = delete;
            VarGuard &operator=(const VarGuard &) = delete;
            constexpr ~VarGuard()
            {
                if (frame && frame->state != State::pausing)
                    std::destroy_at(&frame->template var<I>());
            }
        };
        template <typename Frame, int I> requires(Frame::fake)
        struct VarGuard<Frame, I>
        {
            // This saves the variable type to a stateful storage.
            template <typename T>
            constexpr VarGuard(const Frame *, T &&)
            {
                (void)VarTypeWriter<typename Frame::marker_t, I, std::decay_t<T>>{};
            }
        };

        // An array of pairs, mapping variable names to their indices.
        template <typename T>
        constexpr auto var_name_to_index_mapping = []{
            std::array<std::pair<std::string_view, int>, NumVars<T>::value> ret;
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
            std::array<std::pair<std::string_view, int>, NumYields<T>::value> ret;
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
    template <tag T, int V>
    constexpr const_string var_name_const = detail::VarName<typename T::_rcoro_marker_t, V>::value;

    // The type of a coroutine variable `V`.
    template <tag T, int V>
    using var_type = typename detail::VarType<typename T::_rcoro_marker_t, V>;

    // Variable name helpers:

    // Converts a variable name to its index.
    // Returns a negative error code on failure: either `unknown_name` or `ambiguous_name`.
    template <tag T>
    [[nodiscard]] constexpr int var_index_or_error_code(std::string_view name)
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
        int ret = var_index_or_error_code<T>(name);
        if (ret == ambiguous_name)
            throw std::runtime_error("Ambiguous coroutine variable name: `" + std::string(name) + "`.");
        if (ret < 0)
            throw std::runtime_error("Unknown coroutine variable name: `" + std::string(name) + "`.");
        return ret;
    }
    // Same, but works with constexpr strings, and causes a SOFT compilation error if the name is invalid.
    template <tag T, const_string Name>
    requires(var_index_or_error_code<T>(Name.view()) >= 0)
    constexpr int var_index_const = var_index_or_error_code<T>(Name.view());

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
    template <tag T, int Y>
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
    [[nodiscard]] constexpr int yield_index_or_error_code(std::string_view name)
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
        int ret = yield_index_or_error_code<T>(name);
        if (ret == ambiguous_name)
            throw std::runtime_error("Ambiguous coroutine yield point name: `" + std::string(name) + "`.");
        if (ret < 0)
            throw std::runtime_error("Unknown coroutine yield point name: `" + std::string(name) + "`.");
        return ret;
    }
    // Same, but works with constexpr strings, and causes a SOFT compilation error if the name is invalid.
    template <tag T, const_string Name>
    requires(yield_index_or_error_code<T>(Name.view()) >= 0)
    constexpr int yield_index_const = yield_index_or_error_code<T>(Name.view());

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
    constexpr bool yield_points_uniquely_named = []{
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

    // The offset of variable `V` in the stack frame.
    // Different variables can overlap if `var_lifetime_overlaps_var` is false for them.
    template <tag T, int V>
    constexpr std::size_t var_offset = detail::VarOffset<typename T::_rcoro_marker_t, V>::value;

    // Whether variables `A` and `B` have overlapping lifetime.
    template <tag T, int A, int B>
    constexpr bool var_lifetime_overlaps_var = detail::VarVarReach<typename T::_rcoro_marker_t, A, B>::value;

    // Print this into an `std::ostream` to get a debug information about a type.
    template <tag T>
    struct debug_info_t
    {
        template <typename A, typename B>
        friend std::basic_ostream<A, B> &operator<<(std::basic_ostream<A, B> &s, debug_info_t)
        {
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

    // The coroutine class.
    template <tag T>
    class coro
    {
        typename T::_rcoro_frame_t frame;

      public:
        using tag = T;

        // Constructs a finished coroutine, with `finish_reason() == none`.
        constexpr coro() {}

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

        // Runs a single step of the coroutine. Returns the new value of `!finished()`.
        // Throws if `can_resume() == false`.
        constexpr bool resume()
        {
            if (!can_resume())
                throw std::runtime_error("This coroutine can't be resumed. It's either finished or busy.");

            frame.state = detail::State::running;

            bool had_exception = true;
            struct Guard
            {
                coro &self;
                bool &had_exception;
                ~Guard()
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

            typename T::_rcoro_lambda_t{}(frame, jump_to);

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
            return !finished();
        }

        // Zeroes the coroutine, destroying all variables. Throws if `busy()`.
        constexpr void reset()
        {
            if (busy())
                throw std::runtime_error("Can't reset a busy coroutine.");
            frame.reset();
        }


        // Variable reflection:

        // Returns true if the variable currently exists.
        // Throws if `busy()`.
        template <int V>
        [[nodiscard]] constexpr bool var_exists() const
        {
            if (busy())
                throw std::runtime_error("Can't manipulate variables while a coroutine is running.");
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
                throw std::runtime_error("Can't manipulate variables while a coroutine is running.");
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


        // Serialization:

        // Returns the current yield point index. Note that there's an implicit 0-th yield point at the beginning.
        [[nodiscard]] constexpr int yield_point() const noexcept {return frame.pos;}
        // Returns the current yield point name.
        // The name can be specified in `RC_YIELD("...")`, empty by default. For 0-th point the name is always empty.
        [[nodiscard]] constexpr std::string_view yield_point_name() const noexcept
        {
            return yield_name<T>(frame.pos);
        }

        // Calls a function for each alive variable. Does nothing if the coroutine is not paused.
        // `func` is `void func(auto index)`, where `index.value` is the constexpr variable index.
        // Use `.var<i>()` and `rcoro::var_name_const<i>` to then manipulate the variables.
        // Throws if `busy()`.
        template <typename F>
        constexpr void for_each_alive_var(F &&func) const
        {
            if (busy())
                throw std::runtime_error("Can't manipulate variables while a coroutine is running.");
            if (frame.pos == 0) // Not strictly necessary, hopefully an optimization.
                return;
            detail::with_const_index<num_yields<T>>(frame.pos, [&](auto yieldindex)
            {
                constexpr auto indices = yield_vars<T, yieldindex.value>;
                detail::const_for<indices.size()>([&](auto varindex)
                {
                    func(std::integral_constant<int, indices[varindex.value]>{});
                });
            });
        }


        // Deserialization:

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
            if (fin_reason < rcoro::finish_reason{} || fin_reason >= rcoro::finish_reason::_count)
                throw std::runtime_error("Coroutine finish reason is out of range.");
            if (yield_index < 0 || yield_index >= num_yields<T>)
                throw std::runtime_error("Coroutine yield point index is out of range.");
            if (fin_reason != rcoro::finish_reason::not_finished && yield_index != 0)
                throw std::runtime_error("When loading a finished coroutine, the yield point index must be 0.");
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
                    case finish_reason::none:         s << "none";      break;
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
                s << "\n  " << i << ". " << var_name_const<T, i>.view() << " - " << (c.var_exists<i>() ? "alive" : "dead");
                if constexpr (detail::Printable<var_type<T, i>, std::basic_ostream<A, B>>)
                {
                    if (c.var_exists<i>())
                        s << ", " << c.var<i>();
                }
            });

            return s;
        }
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
// Creates a coroutine.
#define RCORO(...) \
    [&]{ \
        /* A marker for stateful templates. */\
        struct _rcoro_Marker \
        { \
            /* The variable descriptions. */\
            using _rcoro_vars [[maybe_unused]] = ::rcoro::detail::TypeListSkipVoid<void SF_FOR_EACH(DETAIL_RCORO_VARDESC_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__))>; \
            /* The yield point descriptions. */\
            using _rcoro_yields [[maybe_unused]] = ::rcoro::detail::TypeList<::rcoro::detail::ValueTag<::rcoro::const_string("")> SF_FOR_EACH(DETAIL_RCORO_YIELDDESC_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__))>; \
        }; \
        /* Fallback marker variables, used when checking reachibility from yield points. */\
        SF_FOR_EACH(DETAIL_RCORO_MARKERVARS_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
        auto _rcoro_lambda = [](auto &RC_RESTRICT _rcoro_frame, int _rcoro_jump_to) \
        { \
            using _rcoro_frame_t = ::std::remove_cvref_t<decltype(*(&_rcoro_frame + 0))>; /* Need some redundant operations to remove `restrict`. */\
            if (_rcoro_jump_to != 0) \
                goto _rcoro_label_i; \
            SF_FOR_EACH(DETAIL_RCORO_CODEGEN_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, DETAIL_RCORO_CODEGEN_LOOP_FINAL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
        }; \
        /* The first pass call, to calculate the stack frame layout. */\
        /* It also duplicates all of our warnings. */\
        /* GCC doesn't have a reasonable pragma to disable them, and Clang's pragma doesn't seem to work in a macro (hmm). */\
        if (false) \
        { \
            ::rcoro::detail::Frame<true, _rcoro_Marker> _rcoro_fake_frame; \
            _rcoro_lambda(_rcoro_fake_frame, 0); \
        } \
        struct _rcoro_Types \
        { \
            using _rcoro_marker_t = _rcoro_Marker; \
            using _rcoro_frame_t = ::rcoro::detail::Frame<false, _rcoro_Marker>; \
            using _rcoro_lambda_t = decltype(_rcoro_lambda); \
        }; \
        return ::rcoro::coro<_rcoro_Types>().rewind(); \
    }()

#define DETAIL_RCORO_NULL(...)
#define DETAIL_RCORO_IDENTITY(...) __VA_ARGS__
#define DETAIL_RCORO_CALL(m, ...) m(__VA_ARGS__)

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
#define DETAIL_RCORO_LOOP_STEP_code(ident, yieldindex, varindex, markers)    (       ident    , yieldindex  , varindex  , markers)
#define DETAIL_RCORO_LOOP_STEP_var(ident, yieldindex, varindex, markers)     (SF_CAT(ident, i), yieldindex  , varindex+1, (DETAIL_RCORO_IDENTITY markers, DETAIL_RCORO_MARKER_VAR_NAME(ident)))
#define DETAIL_RCORO_LOOP_STEP_withvar(ident, yieldindex, varindex, markers) (SF_CAT(ident, i), yieldindex  , varindex+1, (DETAIL_RCORO_IDENTITY markers, DETAIL_RCORO_MARKER_VAR_NAME(ident)))
#define DETAIL_RCORO_LOOP_STEP_yield(ident, yieldindex, varindex, markers)   (SF_CAT(ident, i), yieldindex+1, varindex  , markers)

// The loop body for the function body generation.
#define DETAIL_RCORO_CODEGEN_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_CODEGEN_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...) __VA_ARGS__
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) \
  SF_CAT(_rcoro_label_, ident): \
    /* Make sure we're not preceded by an if/for/while/etc. */\
    /* The test is performed by having two separate statements, where the latter needs the former to be visible to compile. */\
    /* Note that `[[maybe_unused]]` is not strictly necessary, but it removes an extra useless warning when this check fails. */\
    /* The rcorond variable also serves as a scope checker for yields. */\
    [[maybe_unused]] static constexpr bool SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))) = true; \
    [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))); \
    /* If we're not jumping, initialize the variable. */\
    DETAIL_RCORO_VAR_GUARD(varindex, name, __VA_ARGS__); \
    /* Create a reference as a fancy name for our storage variable. */\
    DETAIL_RCORO_VAR_REF(varindex, name); \
    /* Jump to the next macro, if necessary. */\
    if (_rcoro_jump_to != 0) \
    { \
        /* Jump to the next location. */\
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    /* Stateful meta magic. This can be placed anywhere. */\
    DETAIL_RCORO_VAR_META(varindex, markers)
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_withvar(ident, yieldindex, varindex, markers, name, ...) \
  SF_CAT(_rcoro_label_, ident): \
    if ([[maybe_unused]] constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = true; false) {} else \
    if (DETAIL_RCORO_VAR_GUARD(varindex, name, __VA_ARGS__); false) {} else \
    if (DETAIL_RCORO_VAR_REF(varindex, name); _rcoro_jump_to != 0) \
    { \
        DETAIL_RCORO_VAR_META(varindex, markers) \
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    else
// Variable code pieces:
#define DETAIL_RCORO_VAR_GUARD(varindex, name, ...) \
    ::rcoro::detail::VarGuard<_rcoro_frame_t, varindex> SF_CAT(_rcoro_var_guard, name)(_rcoro_jump_to == 0 ? &_rcoro_frame : nullptr, __VA_ARGS__)
#define DETAIL_RCORO_VAR_REF(varindex, name) \
    auto &name = _rcoro_frame.template var_or_bad_ref<varindex>(_rcoro_jump_to == 0)
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
