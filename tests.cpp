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
        // Used to store the yield point index.
        using coro_pos_t = unsigned int;

        // Used internally when constructing a coroutine wrapper.
        struct ConstructCoroTag {explicit ConstructCoroTag() = default;};

        enum class State
        {
            null, // Default-constructed.
            executing,
            paused,
            finished_ok,
            finished_exception,
        };

        struct Sink
        {
            constexpr Sink() {}
            template <typename T>
            constexpr void operator=(T &&) {}
        };

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
        struct VarVarReach : std::bool_constant<_adl_detail_rcoro_var_var_reach(VarVarReachReader<T,A>{})[B]> {};
        template <typename T, int A>
        struct VarVarReach<T, A, A> : std::true_type {};
        template <typename T, int A, int B> requires(A < B)
        struct VarVarReach<T, A, B> : VarVarReach<T, B, A> {};

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

        // A storage for variables.
        template <bool Fake, typename T>
        struct FrameStorage
        {
            // Zero by default to make it constexpr-constructible, because why not.
            alignas(FrameAlignment<T>::value) char value[FrameSize<T>::value]{};
            FrameStorage() {}
            // Copying this is a no-op, it's handled separately.
            FrameStorage(const FrameStorage &) {}
            FrameStorage &operator=(const FrameStorage &) {}
        };
        // Those specializations should satisfy `std::is_empty`.
        template <typename T>
        struct FrameStorage<true, T> {};
        template <typename T> requires(FrameSize<T>::value == 0)
        struct FrameStorage<false, T> {};

        // An optional-like class that only stores the boolean flag, and uses an external memory location at the specified offset relative to `this`.

        // A stateful trick to configure the offset from the optional to its frame (not directly to the target).
        template <typename T, int N>
        struct OptionalFrameOffsetReader
        {
            friend constexpr std::ptrdiff_t _adl_detail_rcoro_optional_frame_offset(OptionalFrameOffsetReader<T, N>);
        };
        template <typename T, int N, std::ptrdiff_t O>
        struct OptionalFrameOffsetWriter
        {
            friend constexpr std::ptrdiff_t _adl_detail_rcoro_optional_frame_offset(OptionalFrameOffsetReader<T, N>)
            {
                return O;
            }
        };
        constexpr void _adl_detail_rcoro_optional_frame_offset() {} // Dummy ADL target.

        // The optional class itself.
        template <bool Fake, typename T, int N>
        class Optional
        {
            bool exists = false;

            // The target type.
            using type = typename VarDescFor<T, N>::type;

            // The target location.
            void *target_ptr()
            {
                if constexpr (Fake)
                    return nullptr;
                else
                {
                    // Force constant evaluation.
                    constexpr std::ptrdiff_t frame_offset = _adl_detail_rcoro_optional_frame_offset(OptionalFrameOffsetReader<T, N>{});
                    return reinterpret_cast<char *>(this) + frame_offset + VarOffset<T, N>::value;
                }
            }

          public:
            constexpr Optional() {}

            constexpr Optional(const Optional &other) {*this = other;}
            constexpr Optional(Optional &&other) noexcept {*this = std::move(other);}

            constexpr Optional &operator=(const Optional &other)
            {
                if (this == &other)
                    return *this;
                *this = Optional(other); // Do a copy separately, in case it throws.
                return *this;
            }

            constexpr Optional &operator=(Optional &&other) noexcept
            {
                if (this == &other)
                    return *this;
                if (other)
                {
                    emplace(std::move(*other));
                    other.reset();
                }
                else
                {
                    reset();
                }
                return *this;
            }

            [[nodiscard]] constexpr explicit operator bool() const
            {
                return exists;
            }

            constexpr void reset() noexcept
            {
                if (exists)
                {
                    operator*().type::~type();
                    exists = false;
                }
            }

            template <typename ...P>
            constexpr void emplace(P &&... params)
            {
                reset();
                ::new((void *)target_ptr()) type(std::forward<P>(params)...);
                exists = true;
            }

            [[nodiscard]]       type &operator*()       {RC_ASSERT(exists); return *std::launder(reinterpret_cast<type *>(target_ptr()));}
            [[nodiscard]] const type &operator*() const {RC_ASSERT(exists); return *std::launder(reinterpret_cast<type *>(target_ptr()));}

            // If not null, dereferences this. Otherwise returns an invalid reference.
            [[nodiscard]] type &maybe_deref()
            {
                if (*this)
                {
                    return **this;
                }
                else
                {
                    #ifdef __clang__
                    #pragma clang diagnostic push
                    #pragma clang diagnostic ignored "-Wnull-dereference"
                    #endif
                    // If sanitizers complain about this, we'll have to come up with something else.
                    return *(type *)nullptr;
                    #ifdef __clang__
                    #pragma clang diagnostic pop
                    #endif
                }
            }
        };
    }

    // Whether `T` is a template argument of `coro<??>`.
    template <typename T>
    concept tag = requires
    {
        typename T::_rcoro_marker_t;
        typename T::_rcoro_data_t;
        typename T::_rcoro_lambda_t;
    };

    // The number of variables in a coroutine.
    template <tag T>
    constexpr int num_vars = detail::NumVars<typename T::_rcoro_marker_t>::value;

    // The name of a coroutine variable `V`.
    template <tag T, int V>
    constexpr const_string var_name = detail::VarDescFor<typename T::_rcoro_marker_t, V>::name;

    // The type of a coroutine variable `V`.
    template <tag T, int V>
    using var_type = typename detail::VarDescFor<typename T::_rcoro_marker_t, V>::type;

    // The offset of variable `V` in the stack frame.
    // Different variables can overlap if `var_lifetime_overlaps_var` is false for them.
    template <tag T, int V>
    constexpr std::size_t var_offset = detail::VarOffset<typename T::_rcoro_marker_t, V>::value;

    // Returns the stack frame size and alignment of the coroutine.
    template <tag T>
    constexpr std::size_t frame_size = detail::FrameSize<typename T::_rcoro_marker_t>::value;
    template <tag T>
    constexpr std::size_t frame_alignment = detail::FrameAlignment<typename T::_rcoro_marker_t>::value;

    // Whether variables `A` and `B` have overlapping lifetime.
    template <tag T, int A, int B>
    constexpr bool var_lifetime_overlaps_var = detail::VarVarReach<typename T::_rcoro_marker_t, A, B>::value;


    template <tag T>
    class coro
    {
        typename T::_rcoro_data_t data;

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
        [[nodiscard]] constexpr bool empty()                   const noexcept {return data._rcoro_state == detail::State::null;}
        // Not empty and not finished yet.
        [[nodiscard]] constexpr bool unfinished()              const noexcept {return unfinished_paused() || unfinished_executing();}
        // Was paused and can be resumed.
        [[nodiscard]] constexpr bool unfinished_paused()       const noexcept {return data._rcoro_state == detail::State::paused;}
        // Was unpaused and is now executing.
        [[nodiscard]] constexpr bool unfinished_executing()    const noexcept {return data._rcoro_state == detail::State::executing;}
        // Finished running, either normally or because of an exception.
        [[nodiscard]] constexpr bool finished()                const noexcept {return finished_successfully() || finished_with_exception();}
        // Finished running normally.
        [[nodiscard]] constexpr bool finished_successfully()   const noexcept {return data._rcoro_state == detail::State::finished_ok;}
        // Finished running because of an exception.
        [[nodiscard]] constexpr bool finished_with_exception() const noexcept {return data._rcoro_state == detail::State::finished_exception;}

        // Runs a single step of the coroutine. Returns the new value of `unfinished()`.
        // Does nothing when applied to a not `unfinished()` coroutine.
        bool resume()
        {
            data._rcoro_state = detail::State::executing;

            bool had_exception = true;
            struct Guard
            {
                coro &self;
                bool &had_exception;
                ~Guard()
                {
                    if (had_exception)
                        self.data._rcoro_state = detail::State::finished_exception;
                }
            };
            Guard guard{*this, had_exception};

            detail::coro_pos_t jump_to = data._rcoro_pos;

            typename T::_rcoro_lambda_t{}(data, jump_to);

            had_exception = false;

            if (data._rcoro_state == detail::State::executing)
                data._rcoro_state = detail::State::paused; // Otherwise it should be `State::finished_ok`.
            return unfinished();
        }
    };
}

// Declare a coroutine-friendly variable.
// Usage: `RC_VAR(name, type);` or `VAR(name, type) = init;` or `VAR(name, type)(init...);`.
#define RC_VAR(name, .../*type*/) )(var,name,__VA_ARGS__)(code,
// Pause a coroutine. `ident` is a unique identifier for this yield point.
#define RC_YIELD(ident) )(yield,ident)(code,
#define RCORO(...) \
    [&]{ \
        /* A marker for stateful templates. */\
        struct _rcoro_Marker \
        { \
            /* The variable descriptions. */\
            using _rcoro_vars [[maybe_unused]] = ::rcoro::detail::TypeList<void SF_FOR_EACH(DETAIL_RCORO_VARDESC_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__))>; \
        }; \
        /* Can't create templates at function scope, so must use a template lambda. */\
        auto _rcoro_data_lambda = []<bool _rcoro_FirstPass = false>() \
        { \
            struct _rcoro_Data \
            { \
                /* The stack frame, storing all variables. */\
                /* Note that copying this is a no-op, the actual copying is performed by our optionals defined below. */\
                RC_NO_UNIQUE_ADDRESS ::rcoro::detail::FrameStorage<_rcoro_FirstPass, _rcoro_Marker> _rcoro_frame; \
                /* The current yield point. */\
                ::rcoro::detail::coro_pos_t _rcoro_pos = -1; \
                /* The state enum. */\
                ::rcoro::detail::State _rcoro_state = ::rcoro::detail::State::null; \
                /* The stored variables. */\
                SF_FOR_EACH(DETAIL_RCORO_STATE_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
            }; \
            return _rcoro_Data{}; \
        }; \
        /* Fallback marker variables, used when checking reachibility from yield points. */\
        SF_FOR_EACH(DETAIL_RCORO_MARKERVARS_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
        auto _rcoro_lambda = []<bool _rcoro_FirstPass = false>(auto &RC_RESTRICT _rcoro_data, ::rcoro::detail::coro_pos_t _rcoro_jump_to) \
        { \
            if (_rcoro_jump_to != ::rcoro::detail::coro_pos_t(-1)) \
                goto _rcoro_label_i; \
            SF_FOR_EACH(DETAIL_RCORO_CODEGEN_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, DETAIL_RCORO_CODEGEN_LOOP_FINAL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
            _rcoro_data._rcoro_state = ::rcoro::detail::State::finished_ok; \
        }; \
        /* The first pass call, to calculate the stack frame layout. */\
        /* It also duplicates all of our warnings. */\
        /* GCC doesn't have a reasonable pragma to disable them, and Clang's pragma doesn't seem to work in a macro (hmm). */\
        if (false) \
        { \
            auto _rcoro_fake_data = _rcoro_data_lambda.operator()<true>(); \
            _rcoro_lambda.operator()<true>(_rcoro_fake_data, -1); \
        } \
        struct _rcoro_Types \
        { \
            using _rcoro_marker_t = _rcoro_Marker; \
            using _rcoro_data_t = decltype(_rcoro_data_lambda()); \
            using _rcoro_lambda_t = decltype(_rcoro_lambda); \
        }; \
        /* Analyze `_rcoro_data_t`, and save offsets of our optionals into a stateful template. The optionals themselves then consume this information. */\
        SF_FOR_EACH(DETAIL_RCORO_OPTOFFSETS_LOOP_BODY, DETAIL_RCORO_LOOP_STEP, SF_NULL, DETAIL_RCORO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
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
    (void)::rcoro::detail::VarVarReachWriter<true, _rcoro_Marker, varindex, ::std::array<bool, varindex>{DETAIL_RCORO_EXPAND_MARKERS markers}>{}; \
    /* Determine the stack frame offset for this variable. */\
    (void)::rcoro::detail::VarOffsetWriterAuto<true, _rcoro_Marker, varindex>{}; \
  SF_CAT(_rcoro_label_, ident):\
    /* If we're not jumping, initialize the variable. */\
    if (_rcoro_jump_to == ::rcoro::detail::coro_pos_t(-1)) \
        _rcoro_data.DETAIL_RCORO_STORAGE_VAR_NAME(ident, name).emplace(); \
    /* Create a reference as a fancy name for our storage variable. */\
    auto &name = _rcoro_data.DETAIL_RCORO_STORAGE_VAR_NAME(ident, name).maybe_deref(); \
    /* Jump to the next macro, if necessary. */\
    if (_rcoro_jump_to != ::rcoro::detail::coro_pos_t(-1)) \
    { \
        /* Jump to the next location. */\
        goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    /* Construct the variable. */\
    /* Prepare for a possible assignment following this macro. */\
    /* Note that we don't directly use `name` here, to avoid silencing the "unused variable" warning. */\
    ::rcoro::detail::Sink{} = *_rcoro_data.DETAIL_RCORO_STORAGE_VAR_NAME(ident, name)
#define DETAIL_RCORO_CODEGEN_LOOP_BODY_yield(ident, yieldindex, varindex, markers, name) \
    _rcoro_data._rcoro_pos = yieldindex; \
    return; \
  SF_CAT(_rcoro_label_, ident):\
    if (_rcoro_jump_to != ::rcoro::detail::coro_pos_t(-1)) \
    { \
        if (_rcoro_jump_to == yieldindex) \
            /* Finish jumping. */\
            _rcoro_jump_to = ::rcoro::detail::coro_pos_t(-1); \
        else \
            /* Jump to the next location. */\
            goto SF_CAT(_rcoro_label_, SF_CAT(ident, i)); \
    } \
    else \
    { \
        /* Record the current position. */\
        _rcoro_data._rcoro_pos = yieldindex; \
    }
// The code inserted after the loop.
#define DETAIL_RCORO_CODEGEN_LOOP_FINAL(n, d) DETAIL_RCORO_CALL(DETAIL_RCORO_CODEGEN_LOOP_FINAL_, DETAIL_RCORO_IDENTITY d)
#define DETAIL_RCORO_CODEGEN_LOOP_FINAL_(ident, yieldindex, varindex, markers) SF_CAT(_rcoro_label_, ident):


// The loop body to generate the state struct members.
#define DETAIL_RCORO_STATE_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_STATE_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_STATE_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_STATE_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) ::rcoro::detail::Optional<_rcoro_FirstPass, _rcoro_Marker, varindex> DETAIL_RCORO_STORAGE_VAR_NAME(ident, name);
#define DETAIL_RCORO_STATE_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)

// The loop body to generate helper variables to analyze which variables are visible from which yield points.
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_MARKERVARS_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) [[maybe_unused]] static constexpr bool DETAIL_RCORO_MARKER_VAR_NAME(ident) = false;
#define DETAIL_RCORO_MARKERVARS_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)

// The loop body to save the offsets of our optionals in the data structure into a stateful template.
#define DETAIL_RCORO_OPTOFFSETS_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_OPTOFFSETS_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_OPTOFFSETS_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_OPTOFFSETS_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) \
    (void)::rcoro::detail::OptionalFrameOffsetWriter<_rcoro_Marker, varindex, \
        ::std::ptrdiff_t(offsetof(_rcoro_Types::_rcoro_data_t, _rcoro_frame)) \
        - ::std::ptrdiff_t(offsetof(_rcoro_Types::_rcoro_data_t, DETAIL_RCORO_STORAGE_VAR_NAME(ident, name))) \
    >{};
#define DETAIL_RCORO_OPTOFFSETS_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)

// The loop body to generate variable descriptions for reflection.
#define DETAIL_RCORO_VARDESC_LOOP_BODY(n, d, kind, ...) DETAIL_RCORO_CALL(SF_CAT(DETAIL_RCORO_VARDESC_LOOP_BODY_, kind), DETAIL_RCORO_IDENTITY d, __VA_ARGS__)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_code(ident, yieldindex, varindex, markers, ...)
#define DETAIL_RCORO_VARDESC_LOOP_BODY_var(ident, yieldindex, varindex, markers, name, ...) , ::rcoro::detail::VarDesc<__VA_ARGS__, #name>
#define DETAIL_RCORO_VARDESC_LOOP_BODY_yield(ident, yieldindex, varindex, markers, ...)




// VAR(x) = 1

// auto& x = y = 1

#include <iostream>

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
    auto x = RCORO(
        std::cout << 1 << '\n';
        RC_YIELD();
        std::cout << 2 << '\n';
        RC_YIELD();
        std::cout << 3 << '\n';
        {
            RC_VAR(unreachable, int) = 0;
            (void)unreachable;
        }
        RC_VAR(i, int) = 0;
        for (; i < 5; i++)
        {
            RC_YIELD();
            RC_VAR(j, char) = 0;
            std::cout << i * 10 << '\n';
            RC_VAR(k, short) = 0;
            RC_VAR(l, int) = 0;
        }
    );

    using tag = decltype(x)::tag;

    std::cout << "frame_size = " << rcoro::frame_size<tag> << '\n';
    std::cout << "frame_alignment = " << rcoro::frame_alignment<tag> << '\n';
    std::cout << "num_vars = " << rcoro::num_vars<tag> << '\n';
    []<std::size_t ...I>(std::index_sequence<I...>){
        ([]{
            std::cout << "* var " << I << '\n';
            std::cout << "  name = " << rcoro::var_name<tag, I>.view() << '\n';
            std::cout << "  type = " << type_name<rcoro::var_type<tag, I>>() << '\n';
            std::cout << "  reach = ";
            constexpr auto i = I;
            []<std::size_t ...J>(std::index_sequence<J...>){
                ([]{
                    std::cout << rcoro::var_lifetime_overlaps_var<tag, i, J>;
                }(), ...);
            }(std::make_index_sequence<i>{});
            std::cout << '\n';

            std::cout << "  offset = " << rcoro::var_offset<tag, I> << '\n';
            std::cout << '\n';
        }(), ...);
    }(std::make_index_sequence<rcoro::num_vars<tag>>{});


    std::cout << "-\n";
    while (x.resume())
    {
        std::cout << "---\n";
    }
}
