#ifndef LFL_TYPE_TUPLE_HPP
#define LFL_TYPE_TUPLE_HPP
#include "index_tuple.hpp"
#include "void_tuple.hpp"

namespace lfl {

	namespace detail {
		template<lfl::index_t N, class T>
		struct indexed_type {};

		template<lfl::index_t N, class T>
		T get( indexed_type<N, T> );

		template<class IndexTuple, class... Ts>
		struct flat_tuple;
		template<lfl::index_t... Index, class... Ts>
		struct flat_tuple<lfl::index_tuple<Index...>, Ts...>
			: indexed_type<Index, Ts>... {};
	} // namespace detail

	template<class... Ts>
	struct type_tuple {
		
		template<class... Voids, class... Us>
		static auto tail_impl2( Voids*..., Us*... )
			-> type_tuple<Us...>;

		template<class... Voids>
		static auto tail_impl( lfl::void_tuple<Voids...> )
			-> decltype( tail_impl2<Voids...>( static_cast<Ts*>(nullptr)... ) );

		using tail = decltype( tail_impl( lfl::make_void_tuple<(sizeof...(Ts)+7)/8>{} ) ); 


		template<lfl::index_t... Index>
		static auto head_impl( lfl::index_tuple<Index...> )
			-> type_tuple<decltype( detail::get<Index>( detail::flat_tuple<lfl::make_index_tuple<sizeof...(Ts)>, Ts...>{} ) )...>;

		using head = decltype( head_impl( lfl::make_index_tuple<(sizeof...(Ts)+7)/8>{} ) );

	};

} // namespace lfl

#endif // LFL_TYPE_TUPLE_HPP

