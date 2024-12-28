#include <eld/pattern_matching.hpp>

#include <mpark/variant.hpp>

#include <iostream>
#include <vector>
#include <string>

// todo: modify the example to work with C++11 (or at least 14)

// for exposition of captured state
struct stateful {
    stateful() { std::cout << "stateful()\n"; }
    stateful(const stateful&) = delete;
    // mandatory copy elision only supported from C++17
    //stateful(stateful&&) noexcept = delete;
    stateful(stateful&&) noexcept = default;
    ~stateful() { std::cout << "~stateful()\n"; }
};

// todo: C++11-friendly example (no auto args, no capture-initializations)
// the matching is done inside the function, argument type is used to demonstrate the types
template <template <typename...> class Variant>
static std::string match_t(const Variant<int, double, std::string, std::vector<int>> &v) {
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

// todo: checks for callable traits of in_place_visitor_t
int main() {
    const auto match =
        [](const auto& v) {
            // use another std::variant-like template, i.e. mpark::variant
            return match_t<mpark::variant>(v);
        };
    
    std::cout 
      << match("oceanic")
      << '\n' << match(815) 
      << '\n' << match(23.42)
      << '\n' << match(std::vector<int>{4, 8, 15, 16, 23, 42})
      << '\n';
}

