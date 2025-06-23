#pragma once

#if __cplusplus < 202302L
    #error out of date c++ version, compile with -stdc++=2c
#elif defined(__clang__) && __clang_major__ < 19
    #error out of date clang, compile with latest version
#elif !defined(__clang__) && defined(__GNUC__) && __GNUC__ < 14
    #error out of date g++, compile with latest version
#elif defined(_MSC_VER)
    #error msvc does not yet support the latest c++ features
#else

#include <algorithm>
#include <array>
#include <bitset>
#include <cinttypes>
#include <functional>
#include <iterator>
#include <numeric>
#include <ranges>
#include <type_traits>
#include <utility>
#include <vector>

namespace pmg {
    namespace detail {
        template <typename tp_type_t>
        struct array_size_impl : std::integral_constant<std::size_t, 0> {};
        template <typename tp_type_t, std::size_t tp_size_t>
        struct array_size_impl<tp_type_t[tp_size_t]> : std::integral_constant<std::size_t, tp_size_t> {};
        template <typename tp_type_t, std::size_t tp_size_t>
        struct array_size_impl<std::array<tp_type_t, tp_size_t>> : std::integral_constant<std::size_t, tp_size_t> {};
        template <typename tp_type_t>
        auto constexpr array_size = array_size_impl<tp_type_t>::value;

        template <std::size_t tp_max_code_length>
        using optimal_bitmask_type = std::conditional_t<
        std::cmp_less_equal(tp_max_code_length, 8), std::uint8_t, std::conditional_t<
            std::cmp_less_equal(tp_max_code_length, 16), std::uint16_t, std::conditional_t<
                std::cmp_less_equal(tp_max_code_length, 32), std::uint32_t, std::conditional_t<
                    std::cmp_less_equal(tp_max_code_length, 64), std::uint64_t, std::bitset<tp_max_code_length>>
        >>>;
        template <std::size_t tp_value>
        using optimal_unsigned_integer_t = std::conditional_t<
            std::in_range<std::uint8_t>(tp_value), std::uint8_t, std::conditional_t<
                std::in_range<std::uint16_t>(tp_value), std::uint16_t, std::conditional_t<
                    std::in_range<std::uint32_t>(tp_value), std::uint32_t, std::uint64_t>
        >>;
        template <std::size_t tp_length>
        auto constexpr length_capacity = 1 << tp_length;
        template <typename tp_type_t, std::size_t tp_size, bool tp_resize>
        auto constexpr optimal_vector = []([[maybe_unused]] std::size_t p_size) {
            if constexpr (tp_size) {
                if constexpr (tp_resize)
                    return std::array<tp_type_t, tp_size>{}; //should be return std::conditional_t<tp_resize, std::array<tp_type_t, tp_size>, std::inplace_vector<tp_type_t>>(tp_size);
                else {
                    auto l_result = std::vector<tp_type_t>{}; //remove this branch when the above is available
                    l_result.reserve(p_size);
                    return l_result;
                }
            }
            else {
                if constexpr (tp_resize)
                    return std::vector<tp_type_t>(p_size);
                else {
                    auto l_result = std::vector<tp_type_t>{};
                    l_result.reserve(p_size);
                    return l_result;
                }
            }
        };
        auto constexpr pow = [](std::size_t a, std::size_t b) {
            auto l_result = std::size_t(1);
            for (auto _ : std::views::iota(decltype(b){0}, b))
                l_result *= a;
            return l_result;
        };
        auto constexpr drop_last_if_range_size_is_odd = [](auto&& p_range) { return std::views::take(p_range, std::ranges::ssize(p_range) - (std::ranges::ssize(p_range) % 2 != 0)); };
    }
    template <
        typename tp_in_t,
        typename tp_out_t
    >
    using package_merge_result = std::ranges::in_out_result<
        tp_in_t,
        tp_out_t
    >;
    namespace detail {
        template <
            std::size_t tp_max_code_length,
            std::size_t tp_max_frequency_if_known,
            std::size_t tp_size_if_known
        >
        requires (
            !tp_size_if_known ||
            std::cmp_less_equal(
                tp_size_if_known,
                length_capacity<tp_max_code_length>
            )
        )
        struct package_merge_fn {
            template <
                std::ranges::sized_range    tp_sized_range_t,
                std::random_access_iterator tp_random_access_iterator_t,
                typename                    tp_projection_t = std::identity
            >
            requires (
                std::integral<
                    std::remove_cvref_t<
                        std::invoke_result_t<
                            tp_projection_t,
                            std::ranges::range_reference_t<tp_sized_range_t>
                        >
                    >
                > &&
                std::integral<std::iter_difference_t<tp_random_access_iterator_t>>
            )
            auto constexpr operator()[[maybe_unused]] (
                tp_sized_range_t&&          p_range,
                tp_random_access_iterator_t p_result,
                tp_projection_t             p_projection = {}
            )
            const
            -> package_merge_result<
                std::ranges::subrange<
                    std::ranges::iterator_t<tp_sized_range_t>,
                    std::ranges::sentinel_t<tp_sized_range_t>
                >,
                tp_random_access_iterator_t
            > {
                using return_type = package_merge_result<
                    std::ranges::subrange<
                        std::ranges::iterator_t<tp_sized_range_t>,
                        std::ranges::sentinel_t<tp_sized_range_t>
                    >,
                    tp_random_access_iterator_t
                >;
                auto constexpr static l_size_if_known = tp_size_if_known ? tp_size_if_known : array_size<std::remove_cvref_t<tp_sized_range_t>>;
                using l_optimal_bitmask_t   = optimal_bitmask_type<tp_max_code_length>;
                using l_optimal_code_size_t = optimal_unsigned_integer_t<tp_max_code_length>;
                using l_optimal_frequency_t = std::conditional_t<std::cmp_equal(tp_max_frequency_if_known, std::numeric_limits<std::size_t>::max()), std::size_t, optimal_unsigned_integer_t<pow(tp_max_frequency_if_known, tp_max_code_length)>>;
                using l_optimal_size_t      = std::conditional_t<std::cmp_equal(l_size_if_known, 0), std::size_t, optimal_unsigned_integer_t<l_size_if_known>>;
                if constexpr (!l_size_if_known)
                    if (std::cmp_greater(std::ranges::size(p_range), length_capacity<tp_max_code_length>))
                        return return_type{
                            std::ranges::subrange{
                                std::ranges::end(p_range),
                                std::ranges::end(p_range)
                            },
                            std::move(p_result)
                        };
                if (std::ranges::empty(p_range))
                    return return_type{
                        std::ranges::subrange{p_range},
                        std::move(p_result)
                    };
                if (std::cmp_equal(std::ranges::size(p_range), 1)) {
                    *p_result++ = 1;
                    return return_type{
                        std::ranges::subrange{p_range},
                        std::move(p_result)
                    };
                }
                auto const l_histogram_size = std::ranges::size(p_range);
                auto const l_max_capacity   = 2 * l_histogram_size;
                auto l_is_merged            = optimal_vector<l_optimal_bitmask_t, l_size_if_known * 2, true>(l_max_capacity);
                auto l_current_source       = optimal_vector<l_optimal_frequency_t, l_size_if_known * 2, false>(l_max_capacity);
                auto l_previous_source      = optimal_vector<l_optimal_frequency_t, l_size_if_known * 2, false>(l_max_capacity);
                auto l_current              = std::addressof(l_current_source);
                auto l_previous             = std::addressof(l_previous_source);
                auto l_depth                = l_optimal_code_size_t{0};
                std::ranges::transform(p_range, std::back_inserter(l_previous_source), p_projection);
                for (; std::cmp_not_equal(l_depth, tp_max_code_length - 1); std::ranges::swap(l_current, l_previous), l_current->clear()) {
                    auto l_merges = *l_previous | std::views::pairwise_transform(std::plus<l_optimal_frequency_t>{}) | std::views::stride(2);
                    auto l_first1 = std::ranges::begin(p_range);
                    auto l_first2 = std::ranges::begin(l_merges);
                    auto l_last1  = std::ranges::end(p_range);
                    auto l_last2  = std::ranges::end(l_merges);
                    auto l_out    = std::back_inserter(*l_current);
                    *l_out++ = static_cast<std::ranges::range_value_t<decltype(*l_current)>>(std::invoke(p_projection, *l_first1++));
                    *l_out++ = static_cast<std::ranges::range_value_t<decltype(*l_current)>>(std::invoke(p_projection, *l_first1++));
                    for (; l_first1 != l_last1 && l_first2 != l_last2; ++l_out) {
                        if (std::cmp_less_equal(std::invoke(p_projection, *l_first1), *l_first2))
                            *l_out = static_cast<l_optimal_frequency_t>(std::invoke(p_projection, *l_first1++));
                        else {
                            l_is_merged[std::ranges::size(*l_current)] |= static_cast<std::ranges::range_value_t<decltype(l_is_merged)>>(1 << l_depth);
                            *l_out = *l_first2++;
                        }
                    }
                    for (; l_first1 != l_last1; *l_out++ = static_cast<l_optimal_frequency_t>(std::invoke(p_projection, *l_first1++)));
                    for (; l_first2 != l_last2; *l_out++ = static_cast<l_optimal_frequency_t>(*l_first2++))
                        l_is_merged[std::ranges::size(*l_current)] |= static_cast<std::ranges::range_value_t<decltype(l_is_merged)>>(1 << l_depth);
                    ++l_depth;
                    if (std::cmp_greater_equal(std::ranges::size(*l_previous), l_max_capacity - 2) && std::ranges::equal(drop_last_if_range_size_is_odd(*l_current), drop_last_if_range_size_is_odd(*l_previous)))
                        break;
                }
                auto l_analyze_count = std::uintmax_t{l_max_capacity - 2};
                for (*p_result = *std::ranges::next(p_result) = l_depth; auto const i : std::views::iota(decltype(l_depth){0}, l_depth)) {
                    auto l_merged_count = l_optimal_size_t{0};
                    for (auto l_out = std::ranges::next(p_result, 2); auto j : std::views::iota(decltype(l_analyze_count){0}, l_analyze_count) | std::views::drop(2))
                        if (!(l_is_merged[static_cast<std::size_t>(j)] & 1 << (l_depth - 1) >> i))
                            ++*l_out++;
                        else ++l_merged_count;
                    l_analyze_count = l_merged_count * 2;
                }
                for (auto const _ : std::views::iota(decltype(l_analyze_count){0}, l_analyze_count))
                    ++*p_result++;
                return return_type{
                    std::ranges::subrange{p_range},
                    std::move(p_result)
                };
            }
            template <
                std::input_iterator                                  tp_input_iterator_t,
                std::sentinel_for<tp_input_iterator_t>               tp_sentinel_iterator_t,
                std::input_iterator                                  tp_random_access_iterator_t,
                typename                                             tp_projection_t = std::identity
            >
            requires (
                std::integral<
                    std::remove_cvref_t<
                        std::invoke_result_t<
                            tp_projection_t,
                            std::iter_reference_t<tp_input_iterator_t>
                        >
                    >
                > &&
                std::integral<std::iter_value_t<tp_random_access_iterator_t>>
            )
            auto constexpr operator()[[maybe_unused]] (
                tp_input_iterator_t          p_first,
                tp_sentinel_iterator_t       p_last,
                tp_random_access_iterator_t  p_result,
                tp_projection_t              p_projection = {}
            )
            const
            -> package_merge_result<
                std::ranges::subrange<
                    tp_input_iterator_t,
                    tp_sentinel_iterator_t
                >,
                tp_random_access_iterator_t
            > {
                return (*this)(
                    std::ranges::subrange{
                        std::move(p_first),
                        std::move(p_last)
                    },
                    std::move(p_result),
                    std::move(p_projection)
                );
            }
        };
    }
    template <
        std::size_t tp_max_code_length,
        std::size_t tp_max_frequency_if_known = std::numeric_limits<std::size_t>::max(),
        std::size_t tp_size_if_known = 0
    >
    requires requires {
        typename detail::package_merge_fn<
            tp_max_code_length,
            tp_max_frequency_if_known,
            tp_size_if_known
        >;
    }
    auto constexpr package_merge = detail::package_merge_fn<
        tp_max_code_length,
        tp_max_frequency_if_known,
        tp_size_if_known
    >{};
}
#endif
