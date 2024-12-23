#pragma once

/**
// Example usage:

// for exposition of captured state
struct stateful {
    stateful() { std::cout << "stateful()\n"; }
    stateful(const stateful&) = delete;
    stateful(stateful&&) = delete;
    ~stateful() { std::cout << "~stateful()\n"; }
};

// the matching is done inside the function, argument type is used to demonstrate the types
static std::string match(const std::variant<int, double, std::string, std::vector<int>> &v) {
    // pattern must be invoked before the end of the full expression,
    // because it stores the callables by references,
    // so they will be destroyed at the end of the full expression.
    using namespace pattern_matching;
    return v | matched_in_place(
        // matches `int`
        [s = stateful()](int i) { return std::to_string(i); }
        // only matches `double`, since `int` has been matched above
        , m_if<std::is_fundamental>([](double d) { return std::to_string(d); })
        // matches `std::string` (full `std::basic_string` specialization), can also be just
        // [](const std::string &s) { return s; }
        , m_is<std::string>([](const auto &s) { return s; })
        // matches `std::vector<int, Allocator>`, can also be 
        // m_is<std::vector, int, ignored>, can also be
        // m_is<std::vector>
        , m_is<std::vector, int>([](const auto& v) {
            std::cout << "vector called\n";
            int sum = 0; 
            for (int i: v) sum += i; 
            return std::to_string(sum);
        })
        // by default the matching is exhaustive: an unmatched type trigges a compile-time error.
        // in order to have a "default" case `::m_any` can be used to catch all the unmatched types.
        // m_any([](auto) -> std::string { return {}; })
    );
}
*/

#include <type_traits>
#include <tuple>

namespace pattern_matching {
/// placeholder to ignore types
struct ignored {};

namespace detail {
namespace {
template<typename... Ts>
struct make_void { typedef void type; };

template<typename... Ts>
using void_t = typename make_void<Ts...>::type;

template <typename T, template <typename> class...>
struct _t_foldl;

template <typename T, template <typename> class Pred>
struct _t_foldl<T, Pred> {
    using type = typename Pred<T>::type;
};

template <typename T, template <typename> class Left,
    template <typename> class ... Right>
struct _t_foldl<T, Left, Right...> : _t_foldl<typename Left<T>::type, Right...> {};

template <typename T, template <typename> class ... List>
using t_foldl = typename _t_foldl<T, List...>::type;

template <typename L, typename R>
struct is_same : std::is_same<L, R> {};

template <typename L>
struct is_same<L, ignored> : std::true_type {};

template <typename R>
struct is_same<ignored, R> : std::true_type {};

template <typename Arg, typename T>
struct is_same_type : is_same<t_foldl<Arg, std::remove_reference, std::remove_const>, T> {};

template <typename, typename ...>
struct _is_subset_of;

template <typename ... List>
struct list_t;

template <typename ... Of>
struct _is_subset_of<list_t<>, Of...> : std::true_type {};

// gcc will not compile without it
template <>
struct _is_subset_of<list_t<>> : std::true_type {};

template <typename ... T>
struct _is_subset_of<list_t<T...>> : std::false_type {};

template <typename T, typename ... Ts, typename Of, typename ... Ofs>
struct _is_subset_of<list_t<T, Ts...>, Of, Ofs...> : 
    std::conditional<is_same<T, Of>::value, 
        _is_subset_of<list_t<Ts...>, Ofs...>, 
        std::false_type>::type {};

template <typename ... T>
struct list_t {
    template <typename ... Of>
    struct is_subset_of : _is_subset_of<list_t<T...>, Of...> {};
};

template <typename, template <typename...> class, typename ...>
struct is_template : std::false_type {};

template <template <typename...> class Templ, typename ... Args, typename ... SpecArgs>
struct is_template<Templ<Args...>, Templ, SpecArgs...> 
    : list_t<SpecArgs...>::template is_subset_of<Args...> {};

template <template <typename, typename...> class Templ, typename ... SpecArgs>
struct is_template_bind {
    template <typename T>
    using pred = is_template<T, Templ, SpecArgs...>;
};

template <typename, typename = void>
struct is_non_template_callable : std::false_type{};

template <typename C>
struct is_non_template_callable<C, void_t<decltype(&C::operator())>> : std::true_type {};

template <typename T, typename = decltype(&T::operator())>
struct callable_arg;

template <typename T, typename Arg, typename Ret>
struct callable_arg<T, Ret(T::*)(Arg)> {
    using type = Arg;
};

template <typename T, typename Arg, typename Ret>
struct callable_arg<T, Ret(T::*)(Arg) const> {
    using type = Arg;
};

template <typename T>
using callable_arg_t = typename callable_arg<T>::type;

template <typename>
using _pred_always_true = std::true_type;

template <typename UnmatchedType, std::size_t I, bool Matched>
constexpr static std::size_t _find_match(std::integral_constant<bool, Matched>) noexcept {
    static_assert(((UnmatchedType*)nullptr, Matched), "unmatched Type");
    return I;
}

template <typename T, std::size_t I, typename ...>
constexpr static std::size_t _find_match(std::true_type) noexcept { return I; }

template <typename T, std::size_t I, typename Match, typename ... Matches>
constexpr static std::size_t _find_match(std::false_type) noexcept {
    return _find_match<T, I + 1, Matches...>(typename Match::template matched<T>{});
}

template <typename T, typename ... Matches>
constexpr static std::size_t find_match() noexcept {
    return _find_match<T, std::size_t(-1), Matches...>(std::false_type{});
}

template <template <typename> class Pred, typename Callable>
struct _match {
    template <typename T>
    using matched = std::integral_constant<bool, Pred<T>{}>;
    Callable _callable;
};

template <template <typename, typename...> class Pred, typename ... Args>
struct _bind {
    template <typename T>
    using pred = Pred<T, Args...>;
};

template <template <typename, typename...> class Pred, typename ... Args, typename Callable>
constexpr auto _m_if(Callable &&c) noexcept 
-> _match<_bind<Pred, Args...>::template pred, Callable&&> {
    return _match<_bind<Pred, Args...>::template pred, Callable&&>{std::forward<Callable>(c)};
}

template <template <typename> class Pred, typename Callable>
constexpr auto _to_match(_match<Pred, Callable> &&m) noexcept 
-> _match<Pred, Callable> { 
    return std::move(m);
}

template <typename Callable,
    typename = typename std::enable_if<is_non_template_callable<Callable>::value>::type,
    typename ArgT = callable_arg_t<Callable>>
constexpr auto _to_match(Callable &&c) noexcept 
-> decltype(_m_if<is_same, ArgT>(std::forward<Callable>(c))) {
    return _m_if<is_same, ArgT>(std::forward<Callable>(c));
}

template <typename Callable,
    typename = typename std::enable_if<!is_non_template_callable<Callable>::value>::type>
constexpr void _to_match(Callable &&) noexcept {
    static_assert(((Callable*)nullptr, false), "Callables with multiple or template operator() must be wrapped");
}

namespace stub {
namespace {
    struct noop {};
    template <typename Variant>
    void visit(noop, Variant&&);
} // namespace
} // namespace stub

} // namespace
} // namespace detail

////////////////////////////////////////////////////////////////////////////////////////////////////
/// interface
////////////////////////////////////////////////////////////////////////////////////////////////////

/// match using Callable if `Pred<T, Args...>::value` is `true`
template <template <typename, typename...> class Pred, typename ... Args, typename Callable>
constexpr auto m_if(Callable &&c) noexcept 
-> decltype(detail::_m_if<Pred, Args...>(std::forward<Callable>(c))) {
    return detail::_m_if<Pred, Args...>(std::forward<Callable>(c));
}

/// match specialization of `Templ<T, SpecArgs...>` using Callable
template <template <typename, typename...> class Templ, typename ... SpecArgs, 
    typename Callable>
constexpr auto m_is(Callable &&c) noexcept 
-> decltype(detail::_m_if<detail::is_template_bind<Templ, SpecArgs...>::template pred>(std::forward<Callable>(c))) {
    return detail::_m_if<detail::is_template_bind<Templ, SpecArgs...>::template pred>(std::forward<Callable>(c));
}
    
/// match type T using Callable
template <typename T, typename Callable>
constexpr auto m_is(Callable &&c) noexcept 
-> decltype(detail::_m_if<detail::is_same_type, T>(std::forward<Callable>(c))) {
    return detail::_m_if<detail::is_same_type, T>(std::forward<Callable>(c));
}

template <typename Callable>
constexpr auto m_any(Callable &&c) noexcept 
-> decltype(m_if<detail::_pred_always_true>(std::forward<Callable>(c))) {
    return m_if<detail::_pred_always_true>(std::forward<Callable>(c));
}

template <typename...>
struct in_place_visitor_t;

template <template <typename> class... Pred, typename... Callable>
struct in_place_visitor_t<detail::_match<Pred, Callable>...> {
    // todo: static_assert `m_any` is at the end (if provided)
    std::tuple<detail::_match<Pred, Callable>...> _matches;

    // TODO: decltype(auto) return equivalent for C++11
    /// invoke the callable from the matches set matched by T
    template <typename MatchedType>
    constexpr decltype(auto) operator()(MatchedType &&v) && {
        return std::get<
            detail::find_match<typename std::decay<MatchedType>::type, 
            /*Matches*/detail::_match<Pred, Callable>...>()>(_matches)._callable(std::forward<MatchedType>(v));
    }
};

// TODO: decltype(auto) return equivalent for C++11
template <typename Variant, typename ... Matches>
constexpr decltype(auto) operator|(Variant&& v, in_place_visitor_t<Matches...> &&p) {
    using detail::stub::visit;
    return visit(std::move(p), std::forward<Variant>(v));
}

struct matched_in_place_t {
    template <typename ... Matches>
    constexpr auto operator()(Matches &&... matches) const noexcept 
    -> in_place_visitor_t<decltype(detail::_to_match(std::forward<Matches>(matches)))...> {
        return {{detail::_to_match(std::forward<Matches>(matches))...}};
    }
} const matched_in_place{};

} // namespace pattern_matching
