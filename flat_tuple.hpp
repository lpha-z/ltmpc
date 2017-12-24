#ifndef LFL_FLAT_TUPLE_HPP
#define LFL_FLAT_TUPLE_HPP
#include "index_tuple.hpp"
#include "void_tuple.hpp"

namespace lfl {

	namespace {
		template<lfl::index_t N, class T>
		struct indexed_type {};

		template<lfl::index_t N, class T>
		T get( indexed_type<N,T> );
	}

	template<class,class...>
	struct flat_tuple;
	template<lfl::index_t... Index, class... Ts>
	struct flat_tuple<lfl::index_tuple<Index...>, Ts...>
		: indexed_type<Index, Ts>... {};


	namespace {
		template<class... Voids, class... Ts>
		lfl::flat_tuple<lfl::make_index_tuple<sizeof...(Ts)>, Ts...> tail_impl2( Voids*..., Ts*... );
	//	template<class... Voids, class... Ts>
	//	auto tail_impl( lfl::void_tuple<Voids...>, Ts*... )
	//		-> decltype( lfl::tail_impl2<Voids...>( static_cast<Ts*>(nullptr)... ) );
		template<class... Ts, class... Voids>
		auto tail_impl( lfl::void_tuple<Voids...> )
			-> decltype( lfl::tail_impl2<Voids...>( static_cast<Ts*>(nullptr)... ) );
	}
	template<lfl::index_t... Index, class... Ts>
	auto tail( lfl::flat_tuple<lfl::index_tuple<Index...>, Ts...> )
	//	-> decltype( lfl::tail_impl( lfl::make_void_tuple<(sizeof...(Ts)+3)/4>{}, std::declval<Ts*>()... ) ); 
		-> decltype( lfl::tail_impl<Ts...>( lfl::make_void_tuple<(sizeof...(Ts)+3)/4>{} ) );

	namespace {
		template<lfl::index_t... nIndex, lfl::index_t... Index, class... Ts>
		auto head_impl( lfl::flat_tuple<lfl::index_tuple<Index...>, Ts...> tuple, lfl::index_tuple<nIndex...> )
			-> lfl::flat_tuple<lfl::index_tuple<nIndex...>, decltype(lfl::get<nIndex>(tuple))...>;
	}
	template<lfl::index_t... Index, class... Ts>
	auto head( lfl::flat_tuple<lfl::index_tuple<Index...>, Ts...> tuple )
		-> decltype( lfl::head_impl( tuple, lfl::make_index_tuple<(sizeof...(Ts)+3)/4>{} ) );

} // namespace lfl

#endif // LFL_FLAT_TUPLE_HPP

