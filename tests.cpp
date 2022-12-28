// #include <iostream>

#include <algorithm>
#include <array>
#include <memory>
#include <optional>
#include <string_view>
#include <type_traits>
#include <utility>

#include "macro_sequence_for.h"

// An assertion macro. If not customized, uses the standard `assert()`.
#ifndef RC_ASSERT
#include <cassert>
#define RC_ASSERT(expr) assert(expr)
#endif

// Wraps `[[no_unique_address]]`.
#ifndef RC_NO_UNIQUE_ADDRESS
#ifdef _MSC_VER
#define RC_NO_UNIQUE_ADDRESS [[msvc::no_unique_address]] // Dang it, MSVC.
#else
#define RC_NO_UNIQUE_ADDRESS [[no_unique_address]]
#endif
#endif

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

        // Used internally when constructing a coroutine wrapper.
        struct ConstructCoroTag {explicit ConstructCoroTag() = default;};

        // Coroutine state.
        enum class State
        {
            null, // Default-constructed.
            executing,
            paused,
            finished_ok,
            finished_exception,
        };

        // Has no-op assignment, templated for any type.
        struct Sink
        {
            constexpr Sink() {}
            template <typename T>
            constexpr void operator=(T &&) {}
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
        template <int N, typename T>
        struct TypeAt {};
        template <int N, typename P0, typename ...P>
        struct TypeAt<N, TypeList<P0, P...>> : TypeAt<N-1, TypeList<P...>> {};
        template <typename P0, typename ...P>
        struct TypeAt<0, TypeList<P0, P...>> {using type = P0;};

        // Returns the number of variables in a coroutine.
        template <typename T>
        struct NumVars : std::integral_constant<int, T::_rcoro_vars::size - 1> {};
        // Describes a single variable in a coroutine.
        template <typename T, const_string Name>
        struct VarDesc
        {
            using type = T;
            static constexpr const_string name = Name;
        };
        // Returns description for a single variable.
        // Note offsetting the index by one. We need it because the 0th description is `void`, to simplify the macros.
        template <typename T, int N> requires(N >= 0)
        using VarDescFor = typename TypeAt<N + 1, typename T::_rcoro_vars>::type;

        // Returns the number of yield points in a coroutine.
        template <typename T>
        struct NumYields : std::integral_constant<int, T::_rcoro_yields::size - 1> {};
        // Returns description for a single yield point.
        // Note offsetting the index by one. We need it because the 0th description is `void`, to simplify the macros.
        template <typename T, int N> requires(N >= 0)
        using YieldDescFor = typename TypeAt<N + 1, typename T::_rcoro_yields>::type;

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
                + sizeof(typename VarDescFor<T, M-1>::type)
                // Prepare to divide, rounding up.
                + alignof(typename VarDescFor<T, M>::type)
                - 1
            )
            / alignof(typename VarDescFor<T, M>::type)
            * alignof(typename VarDescFor<T, M>::type)>
        {};

        // The stack frame alignment.
        template <typename T>
        struct FrameAlignment : std::integral_constant<std::size_t, []<typename Void, typename ...P>(TypeList<Void, P...>) {
            return std::max({alignof(char), alignof(typename P::type)...});
        }(typename T::_rcoro_vars{})> {};
        // The stack frame size.
        template <typename T>
        struct FrameSize : std::integral_constant<std::size_t, 0> {};
        template <typename T> requires(NumVars<T>::value > 0)
        struct FrameSize<T> : std::integral_constant<std::size_t,
            (
                VarOffset<T, NumVars<T>::value - 1>::value
                + sizeof(typename VarDescFor<T, NumVars<T>::value - 1>::type)
                // Prepare to divide, rounding up.
                + FrameAlignment<T>::value
                - 1
            )
            / FrameAlignment<T>::value
            * FrameAlignment<T>::value>
        {};

        // Inspect noexcept-ness of coroutine variables.
        template <typename T>
        struct FrameIsNothrowCopyConstructible : std::integral_constant<std::size_t, []<typename Void, typename ...P>(TypeList<Void, P...>) {
            return (std::is_nothrow_copy_constructible_v<typename P::type> && ...);
        }(typename T::_rcoro_vars{})> {};
        template <typename T>
        struct FrameIsNothrowCopyOrMoveConstructible : std::integral_constant<std::size_t, []<typename Void, typename ...P>(TypeList<Void, P...>) {
            return ((std::is_nothrow_copy_constructible_v<typename P::type> || std::is_nothrow_move_constructible_v<typename P::type>) && ...);
        }(typename T::_rcoro_vars{})> {};

        // Stores coroutine variables and other state.
        // If `Fake`, becomes an empty structure.

        // A helper base.
        template <typename T>
        struct FrameBase
        {
            // The storage array is in the base, to get rid of it when it's empty.
            // Can't use zero-sized `std::array`, because `std::is_empty` is false for it, so `[[no_unique_address]]` won't help.
            // Zero by default to make it constexpr-constructible, because why not.
            alignas(FrameAlignment<T>::value) char storage_array[FrameSize<T>::value]{};

            constexpr       char *storage()       {return storage_array;}
            constexpr const char *storage() const {return storage_array;}
        };
        template <typename T> requires(FrameSize<T>::value == 0)
        struct FrameBase<T>
        {
            constexpr       char *storage()       {return nullptr;}
            constexpr const char *storage() const {return nullptr;}
        };

        template <bool Fake, typename T>
        struct Frame : FrameBase<T>
        {
            static constexpr bool fake = Fake;

            // The state enum.
            State state = State::null;
            // The current yield point.
            int pos = -1;

            // Returns true if the variable `V` exists at the current yield point.
            template <int V>
            constexpr bool var_exists() const
            {
                bool ret = false;
                if (pos != -1)
                {
                    with_const_index<NumYields<T>::value>(pos, [&](auto yieldindex)
                    {
                        ret = VarYieldReach<T, V, yieldindex.value>::value;
                    });
                }
                return ret;
            }

            // Get `V`th variable storage. Not laundered, so don't dereference.
            template <int V> constexpr       typename VarDescFor<T, V>::type *var_storage()       {return reinterpret_cast<      typename VarDescFor<T, V>::type *>(this->storage() + VarOffset<T, V>::value);}
            template <int V> constexpr const typename VarDescFor<T, V>::type *var_storage() const {return reinterpret_cast<const typename VarDescFor<T, V>::type *>(this->storage() + VarOffset<T, V>::value);}

            // Get `V`th variable. UB if it doesn't exist.
            template <int V> constexpr       typename VarDescFor<T, V>::type &var()       {return *std::launder(var_storage<V>());}
            template <int V> constexpr const typename VarDescFor<T, V>::type &var() const {return *std::launder(var_storage<V>());}

            // Get `V`th variable. An invalid reference if it doesn't exist and if `assume_good` is false.
            template <int V>
            constexpr typename VarDescFor<T, V>::type &var_or_bad_ref(bool assume_good)
            {
                if (assume_good || var_exists<V>())
                    return var<V>();
                else
                    return bad_ref<typename VarDescFor<T, V>::type>();
            }
            template <int V>
            constexpr const typename VarDescFor<T, V>::type &var_or_bad_ref() const
            {
                return const_cast<Frame *>(this)->var_or_bad_ref();
            }

            // Cleans the frame.
            void reset() noexcept
            {
                if (pos == -1)
                    return;
                with_const_index<NumYields<T>::value>(pos, [&](auto yieldindex)
                {
                    constexpr auto indices = vars_reachable_from_yield<T, yieldindex.value>();
                    const_reverse_for<indices.size()>([&](auto varindex)
                    {
                        using type = typename VarDescFor<T, varindex.value>::type;
                        var<varindex.value>().type::~type();
                    });
                });
                pos = -1;
                state = State::null;
            }

            // A helper for `handle_vars()`.
            // Can't define it inside of that function because of Clang bug: https://github.com/llvm/llvm-project/issues/59734
            template <typename F, int N>
            struct HandleVarsGuard
            {
                F &rollback;
                int i = 0;
                bool failed = true;
                ~HandleVarsGuard()
                {
                    const_reverse_for<N>([&](auto var)
                    {
                        if (var.value < i)
                            rollback(var);
                    });
                }
            };

            // Both `func` and `rollback` are `void func(auto index)`, where `index` is `std::integer_constant<int, I>`.
            // `func` is called for every variable index for yield point `pos`.
            // If it throws, `rollback` is called for every index processed so far, in reverse.
            template <typename F, typename G>
            static constexpr void handle_vars(int pos, F &&func, G &&rollback)
            {
                if (pos == -1)
                    return;
                with_const_index<NumYields<T>::value>(pos, [&](auto yield)
                {
                    constexpr auto indices = vars_reachable_from_yield<T, yield.value>();

                    HandleVarsGuard<G, indices.size()> guard{.rollback = rollback};

                    const_for<indices.size()>([&](auto var)
                    {
                        func(var);
                        guard.i++;
                    });

                    guard.failed = false;
                });
            }

            constexpr Frame() {}

            constexpr Frame(const Frame &other) noexcept(FrameIsNothrowCopyConstructible<T>::value)
            {
                *this = other;
            }
            constexpr Frame(Frame &&other) noexcept(FrameIsNothrowCopyOrMoveConstructible<T>::value)
            {
                *this = std::move(other);
            }
            // If the assignment throws, the target will be zeroed.
            constexpr Frame &operator=(const Frame &other) noexcept(FrameIsNothrowCopyConstructible<T>::value)
            {
                if (&other == this)
                    return *this;
                reset();
                handle_vars(other.pos,
                    [&](auto index)
                    {
                        constexpr int i = index.value;
                        std::construct_at(var_storage<i>(), other.var<i>());
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
            // If the assignment throws, the target will be zeroed.
            constexpr Frame &operator=(Frame &&other) noexcept(FrameIsNothrowCopyOrMoveConstructible<T>::value)
            {
                if (&other == this)
                    return *this;
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
            State state = State::null;
            int pos = -1;

            template <int V>
            constexpr typename VarDescFor<T, V>::type &var()
            {
                return bad_ref<typename VarDescFor<T, V>::type>();
            }
            template <int V>
            constexpr typename VarDescFor<T, V>::type &var_or_bad_ref(bool assume_good)
            {
                (void)assume_good;
                return bad_ref<typename VarDescFor<T, V>::type>();
            }
        };

        // Creates a variable in a frame in its constructor, and destroys it in the destructor.
        template <typename Frame, int I>
        struct VarGuard
        {
            const Frame *frame;

            constexpr VarGuard(const Frame *frame) : frame(frame)
            {
                if (frame)
                    std::construct_at(frame->template var_storage<I>());
            }
            VarGuard(const VarGuard &) = delete;
            VarGuard &operator=(const VarGuard &) = delete;
            constexpr ~VarGuard()
            {
                if (frame && frame->state != State::paused)
                    std::destroy_at(&frame->template var<I>());
            }
        };
        template <typename Frame, int I> requires(Frame::fake)
        struct VarGuard<Frame, I>
        {
            constexpr VarGuard(const Frame *) {}
        };
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
    constexpr const_string var_name = detail::VarDescFor<typename T::_rcoro_marker_t, V>::name;

    // The type of a coroutine variable `V`.
    template <tag T, int V>
    using var_type = typename detail::VarDescFor<typename T::_rcoro_marker_t, V>::type;


    // Examining yield points:

    // The number of yield points.
    template <tag T>
    constexpr int num_yields = detail::NumYields<typename T::_rcoro_marker_t>::value;

    // The name of a yield point.
    template <tag T, int Y>
    constexpr const_string yield_name = detail::YieldDescFor<typename T::_rcoro_marker_t, Y>::value;

    // Whether variable `V` exists at yield point `Y`.
    template <tag T, int V, int Y>
    constexpr bool var_lifetime_overlaps_yield = detail::VarYieldReach<typename T::_rcoro_marker_t, V, Y>::value;

    // A list of variables existing at yield point `Y`, of type `std::array<int, N>`.
    template <tag T, int Y>
    constexpr auto yield_vars = detail::vars_reachable_from_yield<typename T::_rcoro_marker_t, Y>();


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


    template <tag T>
    class coro
    {
        typename T::_rcoro_frame_t frame;

      public:
        using tag = T;

        constexpr coro() {}

        constexpr coro(detail::ConstructCoroTag)
        {
            resume();
        }

        // Each coroutine can be in one of three states:
        // * empty (aka default-constructed),
        // * unfinished (either running or paused), and
        // * finished (either normally or with an exception).

        // Same as `unfinished()`.
        [[nodiscard]] explicit constexpr operator bool() const noexcept {return unfinished();}

        // Was default-constructed.
        [[nodiscard]] constexpr bool empty()                   const noexcept {return frame.state == detail::State::null;}
        // Not empty and not finished yet.
        [[nodiscard]] constexpr bool unfinished()              const noexcept {return unfinished_paused() || unfinished_executing();}
        // Was paused and can be resumed.
        [[nodiscard]] constexpr bool unfinished_paused()       const noexcept {return frame.state == detail::State::paused;}
        // Was unpaused and is now executing.
        [[nodiscard]] constexpr bool unfinished_executing()    const noexcept {return frame.state == detail::State::executing;}
        // Finished running, either normally or because of an exception.
        [[nodiscard]] constexpr bool finished()                const noexcept {return finished_successfully() || finished_with_exception();}
        // Finished running normally.
        [[nodiscard]] constexpr bool finished_successfully()   const noexcept {return frame.state == detail::State::finished_ok;}
        // Finished running because of an exception.
        [[nodiscard]] constexpr bool finished_with_exception() const noexcept {return frame.state == detail::State::finished_exception;}

        // Runs a single step of the coroutine. Returns the new value of `unfinished()`.
        // Does nothing when applied to a not `unfinished()` coroutine.
        bool resume()
        {
            frame.state = detail::State::executing;

            bool had_exception = true;
            struct Guard
            {
                coro &self;
                bool &had_exception;
                ~Guard()
                {
                    if (had_exception)
                        self.frame.state = detail::State::finished_exception;
                }
            };
            Guard guard{*this, had_exception};

            int jump_to = frame.pos;

            typename T::_rcoro_lambda_t{}(frame, jump_to);

            had_exception = false;

            if (frame.state == detail::State::executing)
                frame.state = detail::State::finished_ok; // Otherwise it should be `State::paused`.
            return unfinished();
        }
    };
}

// Declare a coroutine-friendly variable.
// Usage: `RC_VAR(name, type);` or `VAR(name, type) = init;` or `VAR(name, type)(init...);`.
#define RC_VAR(name, .../*type*/) )(var,name,__VA_ARGS__)(code,
// Pause a coroutine. `ident` is a unique identifier for this yield point.
#define RC_YIELD(name) )(yield,name)(code,
#define RCORO(...) \
    [&]{ \
        /* A marker for stateful templates. */\
        struct _rcoro_Marker \
        { \
            /* The variable descriptions. */\
            using _rcoro_vars [[maybe_unused]] = ::rcoro::detail::TypeList<void SF_FOR_EACH(DETAIL_RCORO_VARDESC_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__))>; \
            /* The yield point descriptions. */\
            using _rcoro_yields [[maybe_unused]] = ::rcoro::detail::TypeList<void SF_FOR_EACH(DETAIL_RCORO_YIELDDESC_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__))>; \
        }; \
        /* Fallback marker variables, used when checking reachibility from yield points. */\
        SF_FOR_EACH(DETAIL_RCORO_MARKERVARS_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
        auto _rcoro_lambda = [](auto &RC_RESTRICT _rcoro_frame, int _rcoro_jump_to) \
        { \
            using _rcoro_frame_t = ::std::remove_cvref_t<decltype(*(&_rcoro_frame + 0))>; /* Need some redundant operations to remove `restrict`. */\
            if (_rcoro_jump_to != -1) \
                goto _rcoro_label_i; \
            SF_FOR_EACH(DETAIL_RCORO_CODEGEN_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, DETAIL_RCORO_CODEGEN_LOOP_FINAL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
        }; \
        /* The first pass call, to calculate the stack frame layout. */\
        /* It also duplicates all of our warnings. */\
        /* GCC doesn't have a reasonable pragma to disable them, and Clang's pragma doesn't seem to work in a macro (hmm). */\
        if (false) \
        { \
            ::rcoro::detail::Frame<true, _rcoro_Marker> _rcoro_fake_frame; \
            _rcoro_lambda(_rcoro_fake_frame, -1); \
        } \
        struct _rcoro_Types \
        { \
            using _rcoro_marker_t = _rcoro_Marker; \
            using _rcoro_frame_t = ::rcoro::detail::Frame<false, _rcoro_Marker>; \
            using _rcoro_lambda_t = decltype(_rcoro_lambda); \
        }; \
        return ::rcoro::coro<_rcoro_Types>(::rcoro::detail::ConstructCoroTag{}); \
    }()

#define DETAIL_RCORO_IDENTITY(...) __VA_ARGS__
#define DETAIL_RCORO_CALL(m, ...) m(__VA_ARGS__)

// Generate an internal name for a coroutine variable. `name` is the user-friendly name, and `ident` is a unique identifier.
#define DETAIL_RCORO_STORAGE_VAR_NAME(ident, name) SF_CAT(_rcoro_var_, SF_CAT(ident, SF_CAT(_, name)))
// Generate an internal name for a marker variable used by reachibility checks.
#define DETAIL_RCORO_MARKER_VAR_NAME(ident) SF_CAT(_rcoro_visibility_check_, ident)
// Apply this to an element of the loop state containing variable reachibility markers, to get a proper list of those markers.
#define DETAIL_RCORO_EXPAND_MARKERS(x, ...) __VA_ARGS__

// The initial state for the macro loops.
// (0) is a unique identifier consisting of repeated `i`s, (1), (2) are yield and variable counters respectively,
// and (3) is a list of variable markers to test their reachibility at yield points.
#define DETAIL_RCORO_LOOP_INIT_STATE (i,0,0,(x))

// An universal step function for our loops.
#define DETAIL_RCORO_LOOP_STEP(n, d, kind, ...) SF_CAT(DETAIL_RCORO_LOOP_STEP_, kind) d
#define DETAIL_RCORO_LOOP_STEP_code(ident, yieldindex, varindex, markers)  (       ident    , yieldindex  , varindex  , markers)
#define DETAIL_RCORO_LOOP_STEP_var(ident, yieldindex, varindex, markers)   (SF_CAT(ident, i), yieldindex  , varindex+1, (DETAIL_RCORO_IDENTITY markers, DETAIL_RCORO_MARKER_VAR_NAME(ident)))
#define DETAIL_RCORO_LOOP_STEP_yield(ident, yieldindex, varindex, markers) (SF_CAT(ident, i), yieldindex+1, varindex  , markers)

// The loop body for the function body generation.
#define DETAIL_RCORO_CODEGEN_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_CODEGEN_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...) __VA_ARGS__
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) \
    /* Make sure we're not preceded by an if/for/while/etc. */\
    /* The test is performed by having two separate statements, where the latter needs the former to be visible to compile. */\
    /* Note that `[[maybe_unused]]` is not strictly necessary, but it removes an extra useless warning when this check fails. */\
    /* The rcorond variable also serves as a scope checker for yields. */\
    [[maybe_unused]] static constexpr bool SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))) = true; \
    [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = SF_CAT(_rcoro_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))); \
    /* Analyze lifetime overlap with other variables. */\
    (void)::rcoro::detail::VarVarReachWriter<_rcoro_frame_t::fake, _rcoro_Marker, varindex, ::std::array<bool, varindex>{DETAIL_RCORO_EXPAND_MARKERS markers}>{}; \
    /* Determine the stack frame offset for this variable. */\
    (void)::rcoro::detail::VarOffsetWriterAuto<_rcoro_frame_t::fake, _rcoro_Marker, varindex>{}; \
  SF_CAT(_rcoro_label_, ident): \
    /* If we're not jumping, initialize the variable. */\
    ::rcoro::detail::VarGuard<_rcoro_frame_t, varindex> SF_CAT(_rcoro_var_guard, name)(_rcoro_jump_to == -1 ? &_rcoro_frame : nullptr); \
    /* Create a reference as a fancy name for our storage variable. */\
    auto &name = _rcoro_frame.template var_or_bad_ref<varindex>(_rcoro_jump_to == -1); \
    /* Jump to the next macro, if necessary. */\
    if (_rcoro_jump_to != -1) \
    { \
        /* Jump to the next location. */\
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    /* Construct the variable. */\
    /* Prepare for a possible assignment following this macro. */\
    /* Note that we don't directly use `name` here, to avoid silencing the "unused variable" warning. */\
    ::rcoro::detail::Sink{} = _rcoro_frame.template var<varindex>()
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_yield(ident, yieldindex, varindex, markers, name) \
    do \
    { \
        /* Analyze lifetime overlap with other variables. */\
        (void)::rcoro::detail::VarYieldReachWriter<_rcoro_frame_t::fake, _rcoro_Marker, yieldindex, ::std::array<bool, varindex>{DETAIL_RCORO_EXPAND_MARKERS markers}>{}; \
        /* Remember the position. */\
        _rcoro_frame.pos = yieldindex; \
        /* Pause. */\
        _rcoro_frame.state = ::rcoro::detail::State::paused; \
        return; \
      SF_CAT(_rcoro_label_, ident): \
        if (_rcoro_jump_to != -1) \
        { \
            if (_rcoro_jump_to == yieldindex) \
                /* Finish jumping. */\
                _rcoro_jump_to = -1; \
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
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)

// The loop body to generate variable descriptions for reflection.
#define DETAIL_RCORO_VARDESC_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_VARDESC_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) , ::rcoro::detail::VarDesc<__VA_ARGS__, #name>
#define DETAIL_RCORO_VARDESC_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)

// The loop body to generate yield point descriptions for reflection.
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_YIELDDESC_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_var(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_YIELDDESC_LOOP_BODY_yield(ident, yieldindex, varindex, markers, name) , ::rcoro::detail::ValueTag<::rcoro::const_string("" name "")>




// VAR(x) = 1

// auto& x = y = 1

#include <iostream>
#include <iomanip>

template <typename T>
std::string_view type_name()
{
    std::string_view ret(__PRETTY_FUNCTION__);
    ret.remove_prefix(34);
    ret.remove_suffix(1);
    return ret;
}

int main()
{
    // int rcoro;
    // int detail;
    // int std;
    auto x = RCORO(
        ::std::cout << 1 << '\n';
        RC_YIELD();
        ::std::cout << 2 << '\n';
        RC_YIELD();
        ::std::cout << 3 << '\n';
        {
            RC_VAR(unreachable, int) = 0;
            (void)unreachable;
        }
        RC_VAR(i, int) = 0;
        for (; i < 5; i++)
        {
            RC_VAR(j, char) = 0;
            RC_YIELD();
            ::std::cout << i * 10 << '\n';
            RC_VAR(k, short) = 0;
            RC_VAR(l, int) = 0;
            (void)j;
            (void)k;
            (void)l;
        }
    );

    x.resume();
    x.resume();
    x.resume();
    x.resume();
    auto y = x;

    ::std::cout << "-\n";
    while (x.resume())
    {
        ::std::cout << "---\n";
    }

    x = std::move(y);

    ::std::cout << "####\n";
    while (x.resume())
    {
        ::std::cout << "---\n";
    }

    #if 0
    using tag = decltype(x)::tag;

    ::std::cout << "frame_size = " << rcoro::frame_size<tag> << '\n';
    ::std::cout << "frame_alignment = " << rcoro::frame_alignment<tag> << '\n';
    ::std::cout << "num_vars = " << rcoro::num_vars<tag> << '\n';
    []<::std::size_t ...I>(::std::index_sequence<I...>){
        ([]{
            ::std::cout << " " << I << ". " << rcoro::var_name<tag, I>.view() << ", " << type_name<rcoro::var_type<tag, I>>() << '\n';
            ::std::cout << "    offset=" << rcoro::var_offset<tag, I> << ", size=" << sizeof(rcoro::var_type<tag, I>) << ", align=" << alignof(rcoro::var_type<tag, I>) << '\n';
            constexpr auto i = I;
            []<::std::size_t ...J>(::std::index_sequence<J...>){
                bool first = true;
                ([&]{
                    if constexpr (rcoro::var_lifetime_overlaps_var<tag, i, J>)
                    {
                        if (first)
                        {
                            first = false;
                            ::std::cout << "    overlaps: ";
                        }
                        else
                            ::std::cout << ", ";
                        ::std::cout << J << "." << rcoro::var_name<tag, J>.view();
                    }
                }(), ...);
                if (!first)
                    ::std::cout << '\n';
            }(::std::make_index_sequence<i>{});
            ::std::cout << '\n';
        }(), ...);
    }(::std::make_index_sequence<rcoro::num_vars<tag>>{});

    ::std::cout << "num_yields = " << rcoro::num_yields<tag> << '\n';
    []<::std::size_t ...I>(::std::index_sequence<I...>){
        ([]{
            ::std::cout << " " << I << ". " << ::std::quoted(rcoro::yield_name<tag, I>.view());
            constexpr int i = I;
            []<::std::size_t ...J>(::std::index_sequence<J...>){
                bool first = true;
                ([&]{
                    if (first)
                    {
                        first = false;
                        ::std::cout << " overlaps: ";
                    }
                    else
                        ::std::cout << ", ";
                    constexpr int index = rcoro::yield_vars<tag, i>[J];
                    ::std::cout << index << "." << rcoro::var_name<tag, index>.view();
                }(), ...);
            }(::std::make_index_sequence<rcoro::yield_vars<tag, I>.size()>{});
            ::std::cout << '\n';
        }(), ...);
    }(::std::make_index_sequence<rcoro::num_yields<tag>>{});
    #endif
}
