// #include <iostream>

#include <memory>
#include <utility>

#include <macro_sequence_for.h>

namespace seco
{
    namespace detail
    {
        // Used to store the yield point index.
        using coro_pos_t = unsigned short;

        // Used internally when constructing a coroutine wrapper.
        struct ConstructCoroTag {explicit ConstructCoroTag() = default;};

        enum class State : unsigned char
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

        // Our own optional, that can be dereferenced without UB even when empty (but still returning a dangling reference).
        template <typename T>
        class Optional
        {
            union {T value_storage;};
            bool exists = false;

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
                    operator*().T::~T();
                    exists = false;
                }
            }

            template <typename ...P>
            constexpr void emplace(P &&... params)
            {
                reset();
                ::new((void *)&value_storage) T(std::forward<P>(params)...);
                exists = true;
            }

            [[nodiscard]]       T &operator*()       {return *(exists ? std::launder(&value_storage) : &value_storage);} // Ugh.
            [[nodiscard]] const T &operator*() const {return *(exists ? std::launder(&value_storage) : &value_storage);}
        };
    }

    template <typename T, typename L>
    class coro
    {
        T data;

        union
        {
            #ifdef _MSC_VER
            [[msvc::no_unique_address]]
            #else
            [[no_unique_address]]
            #endif
            L lambda;
        };

      public:
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
        [[nodiscard]] constexpr bool empty()                   const noexcept {return data._seco_state = detail::State::null;}
        // Not empty and not finished yet.
        [[nodiscard]] constexpr bool unfinished()              const noexcept {return unfinished_paused() || unfinished_executing();}
        // Was paused and can be resumed.
        [[nodiscard]] constexpr bool unfinished_paused()       const noexcept {return data._seco_state = detail::State::paused;}
        // Was unpaused and is now executing.
        [[nodiscard]] constexpr bool unfinished_executing()    const noexcept {return data._seco_state = detail::State::executing;}
        // Finished running, either normally or because of an exception.
        [[nodiscard]] constexpr bool finished()                const noexcept {return finished_successfully() || finished_with_exception();}
        // Finished running normally.
        [[nodiscard]] constexpr bool finished_successfully()   const noexcept {return data._seco_state = detail::State::finished_ok;}
        // Finished running because of an exception.
        [[nodiscard]] constexpr bool finished_with_exception() const noexcept {return data._seco_state = detail::State::finished_exception;}

        // Runs a single step of the coroutine. Returns the new value of `unfinished()`.
        // Does nothing when applied to a not `unfinished()` coroutine.
        bool resume()
        {
            data._seco_now_jumping = true;
            data._seco_state = detail::State::executing;
            bool had_exception = true;

            struct Guard
            {
                coro &self;
                bool &had_exception;
                ~Guard()
                {
                    if (had_exception)
                        self._seco_state = detail::State::finished_exception;
                }
            };
            Guard guard{*this, had_exception};

            lambda(data);

            had_exception = false;

            if (data._seco_state == detail::State::executing)
                data._seco_state = detail::State::paused; // Otherwise it should be `State::finished_ok`.
            return finished();
        }
    };
}

// Declare a coroutine-friendly variable.
// Usage: `VAR(name, type);` or `VAR(name, type) = init;`.
#define VAR(name, .../*type*/) )(var,name,__VA_ARGS__)(code,
// Pause a coroutine. `ident` is a unique identifier for this yield point.
#define YIELD(ident) )(yield,ident)(code,
#define CORO(...) \
    [&]{ \
        struct _seco_Data \
        { \
            ::seco::detail::coro_pos_t _seco_pos = -1; /* The current yield point. */\
            ::seco::detail::State _seco_state = ::seco::detail::State::null; /* The state enum. */\
            bool _seco_now_jumping = false; /* Whether we're currently in the process of navigating to the resumption point. */\
            /* The stored variables: */\
            SF_FOR_EACH(IMPL_SECO_STATE_LOOP_BODY, IMPL_SECO_LOOP_STEP, SF_NULL, IMPL_SECO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
        }; \
        [[maybe_unused]] [](_seco_Data &_seco_data){ \
            goto _seco_label_i; \
            SF_FOR_EACH(IMPL_SECO_CODEGEN_LOOP_BODY, IMPL_SECO_LOOP_STEP, IMPL_SECO_CODEGEN_LOOP_FINAL, IMPL_SECO_LOOP_INIT_STATE, (code,__VA_ARGS__)) \
            _seco_data._seco_state = ::seco::detail::State::finished_ok; \
        }; \
    }();

#define IMPL_SECO_IDENTITY(...) __VA_ARGS__
#define IMPL_SECO_CALL(m, ...) m(__VA_ARGS__)

// Generate an internal name for a coroutine variable. `name` is the user-friendly name, and `ident` is a unique identifier.
#define IMPL_SECO_STORAGE_VAR_NAME(ident, name) SF_CAT(_seco_var_, SF_CAT(ident, SF_CAT(_, name)))

// The initial state for the macro loops.
// (0) is a unique identifier consisting of repeated `i`s, and (1) is an index.
#define IMPL_SECO_LOOP_INIT_STATE (i,0)

// An universal step function for our loops.
#define IMPL_SECO_LOOP_STEP(n, d, kind, ...) SF_CAT(IMPL_SECO_LOOP_STEP_, kind) d
#define IMPL_SECO_LOOP_STEP_code(ident, index)  (       ident    , index  )
#define IMPL_SECO_LOOP_STEP_var(ident, index)   (SF_CAT(ident, i), index+1)
#define IMPL_SECO_LOOP_STEP_yield(ident, index) (SF_CAT(ident, i), index+1)

// The loop body for the function body generation.
#define IMPL_SECO_CODEGEN_LOOP_BODY(n, d, kind, ...) IMPL_SECO_CALL(SF_CAT(IMPL_SECO_CODEGEN_LOOP_BODY_, kind), IMPL_SECO_IDENTITY d, __VA_ARGS__)
#define IMPL_SECO_CODEGEN_LOOP_BODY_code(ident, index, ...) __VA_ARGS__
#define IMPL_SECO_CODEGEN_LOOP_BODY_var(ident, index, name, ...) \
    /* Make sure we're not preceded by an if/for/while/etc. */\
    /* The test is performed by having two separate statements, where the latter needs the former to be visible to compile. */\
    /* Note that only the former variable needs a truly unique name. The latter variable just needs something unique per scope. */\
    /* Note that `[[maybe_unused]]` is not strictly necessary, but it removes an extra useless warning when this check fails. */\
    /* The second variable also serves as a scope checker for yields. */\
    [[maybe_unused]] static constexpr bool SF_CAT(_seco_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))) = true; \
    [[maybe_unused]] static constexpr bool SF_CAT(_seco_visibility_check_, name) = SF_CAT(_seco_, SF_CAT(ident, SF_CAT(_NeedBracesAroundDeclarationOf_, name))); \
  SF_CAT(_seco_label_, ident):\
    /* Create a reference as a fancy name for our storage variable. */\
    auto &name = *_seco_data.IMPL_SECO_STORAGE_VAR_NAME(ident, name); \
    /* Jump to the next macro, if necessary. */\
    IMPL_SECO_CODEGEN_JUMP_POINT(ident, index)\
    /* Prepare for a possible assignment following this macro. */\
    /* Note that we don't directly use `name` here, to avoid silencing the "unused variable" macro. */\
    ::seco::detail::Sink{} = (*_seco_data.IMPL_SECO_STORAGE_VAR_NAME(ident, name))
#define IMPL_SECO_CODEGEN_LOOP_BODY_yield(ident, index, name) \
    _seco_data._seco_pos = index; \
    return; \
  SF_CAT(_seco_label_, ident):\
    IMPL_SECO_CODEGEN_JUMP_POINT(ident, index)
// The code inserted after the loop.
#define IMPL_SECO_CODEGEN_LOOP_FINAL(n, d) IMPL_SECO_CALL(IMPL_SECO_CODEGEN_LOOP_FINAL_, IMPL_SECO_IDENTITY d)
#define IMPL_SECO_CODEGEN_LOOP_FINAL_(ident, index) SF_CAT(_seco_label_, ident):

// Performs a conditional `goto` jump to the next variable declaration or yield point.
#define IMPL_SECO_CODEGEN_JUMP_POINT(ident, index) \
    if (_seco_data._seco_now_jumping) \
    { \
        if (_seco_data._seco_pos == index) \
            _seco_data._seco_now_jumping = false; \
        else \
            goto SF_CAT(_seco_label_, SF_CAT(ident, i)); \
    } \
    else \
    { \
        _seco_data._seco_pos = index; \
    }

// The loop body to generate the state struct members.
#define IMPL_SECO_STATE_LOOP_BODY(n, d, kind, ...) IMPL_SECO_CALL(SF_CAT(IMPL_SECO_STATE_LOOP_BODY_, kind), IMPL_SECO_IDENTITY d, __VA_ARGS__)
#define IMPL_SECO_STATE_LOOP_BODY_code(ident, index, ...)
#define IMPL_SECO_STATE_LOOP_BODY_var(ident, index, name, ...) ::seco::detail::Optional<__VA_ARGS__> IMPL_SECO_STORAGE_VAR_NAME(ident, name);
#define IMPL_SECO_STATE_LOOP_BODY_yield(ident, index, ...)




// VAR(x) = 1

// auto& x = y = 1


int main()
{
    CORO(
        VAR(x, int);

        // if (true)
            VAR(y, int) = 1;
    );
}
