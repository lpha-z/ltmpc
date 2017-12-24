#include "ltmpc.hpp"
#include "Dumper.hpp"

void dump_impl( parser::detail::type_stack<> ) { /*do nothing*/ }

template<class... T, class U>
void dump_impl( parser::detail::type_stack<U, T...> ) {
	dump_impl( parser::detail::type_stack<T...>{} );
	std::cout << parser::detail::dumper::to_string(U{},0,true) << std::endl;
}

template<class... T>
void dump( parser::detail::type_stack<T...> st ) {
	std::cout << __PRETTY_FUNCTION__ << std::endl;
	dump_impl( st );
}

void dump_impl( type_analysis::type_tuple<> ) { /*do nothing*/ }
template<class... T, class U>
void dump_impl( type_analysis::type_tuple<U, T...> ) {
	dump_impl( type_analysis::type_tuple<T...>{} );
	std::cout << parser::detail::dumper::to_string(U{},0,true) << std::endl;
}

template<class... T>
void dump( type_analysis::type_tuple<T...> st ) {
	dump_impl( st );
}

template<class SP>
void dump_impl( location::GlobalEnvironment<SP> ) { /*do nothing*/ }
template<class SP, class... T, class U>
void dump_impl( location::GlobalEnvironment<SP, U, T...> ) {
	std::cout << parser::detail::dumper::to_string(U{},0,true) << std::endl;
	dump_impl( location::GlobalEnvironment<SP, T...>{} );
}

template<class T>
void dump( T ) {
	std::cout << __PRETTY_FUNCTION__ << std::endl;
}

template<class SP, class... T>
void dump( location::GlobalEnvironment<SP, T...> st ) {
	dump( SP{} );
	dump_impl( st );
}


int main() {
	dump( ParserResult{} );
	dump( TypeAnalysisResult{} );
	dump( LocateResult{} );
}

