#ifndef LFL_INDEX_TUPLE_HPP
#define LFL_INDEX_TUPLE_HPP
#include <cstddef>

namespace lfl {
	// index_t
	using index_t = std::ptrdiff_t;

	// index_tuple
	template<lfl::index_t... Index>
	struct index_tuple { };

	namespace {
		// index_tuple_build
		template<class IndexTuple, lfl::index_t Step, bool Even>
		struct index_tuple_build;

		template<lfl::index_t... Index, lfl::index_t Step>
		struct index_tuple_build<lfl::index_tuple<Index...>, Step, true>
		{
			using type = lfl::index_tuple<Index..., ( Index + Step )...>;
		};

		template<lfl::index_t... Index, lfl::index_t Step>
		struct index_tuple_build<lfl::index_tuple<Index...>, Step, false>
		{
			using type = lfl::index_tuple<Index..., ( Index + Step )...,Step * 2>;
		};

		// index_tuple_impl
		template<std::size_t N>
		struct index_tuple_impl : public lfl::index_tuple_build<typename lfl::index_tuple_impl<N / 2>::type, N/2, N % 2 == 0> { };

		template<>
		struct index_tuple_impl<0>
		{
			using type = lfl::index_tuple<>;
		};

	}

	// make_index_tuple
	template<std::size_t N>
	using make_index_tuple = typename lfl::index_tuple_impl<N>::type;

} // lfl

#endif // LFL_INDEX_TUPLE_HPP
