#ifndef LFL_VOID_TUPLE_HPP
#define LFL_VOID_TUPLE_HPP
#include <cstddef>

namespace lfl {
	using index_t = std::ptrdiff_t;

	// void_tuple
	template<class... Voids>
	struct void_tuple { };

	namespace {
		// void_tuple_build
		template<class VoidTuple, lfl::index_t Step, bool Even>
		struct void_tuple_build;

		template<class... Voids, lfl::index_t Step>
		struct void_tuple_build<lfl::void_tuple<Voids...>, Step, true>
		{
			using type = lfl::void_tuple<Voids..., Voids...>;
		};

		template<class... Voids, lfl::index_t Step>
		struct void_tuple_build<lfl::void_tuple<Voids...>, Step, false>
		{
			using type = lfl::void_tuple<Voids..., Voids..., void>;
		};

		// void_tuple_impl
		template<std::size_t N>
		struct void_tuple_impl : public lfl::void_tuple_build<typename lfl::void_tuple_impl<N/2>::type, N/2, N%2 == 0> { };

		template<>
		struct void_tuple_impl<0>
		{
			using type = lfl::void_tuple<>;
		};

	} // detail

	// make_void_tuple
	template<std::size_t N>
	using make_void_tuple = typename lfl::void_tuple_impl<N>::type;

} // lfl

#endif // LFL_VOID_TUPLE_HPP
