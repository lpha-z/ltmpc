#ifndef LTMPC_LEXER_HPP
#define LTMPC_LEXER_HPP
#include <type_traits>
#include "flat_tuple.hpp"
#include "PreLexer.hpp"

namespace lexer {

	namespace detail {
		// --- token base ---
		template<char... Chars>
		struct TokenBase {};
	}

	// --- identifier ---
	template<char... Chars>
	struct IdentifierToken : detail::TokenBase<Chars...> {};

	// --- literal ---
	template<char... Chars>
	struct IntLiteralToken : detail::TokenBase<Chars...> {};

	// --- postfix ---
	template<char... Chars>
	struct PostfixToken : detail::TokenBase<Chars...> {};

	// --- string literal ---
	template<char... Chars>
	struct StringLiteral {};

	// --- char literal ---
	template<char... Chars>
	struct CharLiteral {
#ifdef Werror
		static_assert(sizeof...(Chars)==1, "Lexer warning: multi-character character constant");
#endif // Werror
	};

	// --- symbol ---
	template<char... Chars>
	struct Symbol {};

	// --- token tuple ---
	template<class... Ts>
	struct TokenTuple {};


	namespace detail {

		// --- comment begin ---
		template<char C>
		struct CommentBegin {};

		// for ASCII
		template<char C>
		struct IsAlpha : public std::enable_if<( 'a' <= C && C <= 'z' ) || ( 'A' <= C && C <= 'Z' ), std::nullptr_t> {};
		template<char C>
		struct IsNum : public std::enable_if<'0' <= C && C <= '9', std::nullptr_t> {};
		template<char C>
		struct IsHeadIdUse : public IsAlpha<C> {};
		template<>
		struct IsHeadIdUse<'_'> { using type = std::nullptr_t; };
		template<char C>
		struct IsSymbol : public std::enable_if<
				C == '(' || C == ')' || C == '{' || C == '}' || C == '[' || C == ']' || C == '"' || C == '\'' || C == '.' || C == ';' || C == ',' || C == '?' || C == ':'
				|| C == '=' || C == '|' || C == '^' || C == '&' || C == '<' || C == '>' || C == '+' || C == '-' || C == '*' || C == '/' || C == '%' || C == '~'
			, std::nullptr_t> {};
		template<char C>
		struct IsSpace : public std::enable_if<C == ' ' || C == '\n', std::nullptr_t> {};

		template<class T, class U, class V>
		struct CombTriple {};
		template<class T>
		struct CombSingle {};

		// --- EscapeChar
		template<char C>
		struct EscapeChar;
		template<> struct EscapeChar<'0'> { static const char escaped = '\0'; using EscapedString = lexer::StringLiteral<'\\','0','0','0'>; };
		template<> struct EscapeChar<'a'> { static const char escaped = '\a'; using EscapedString = lexer::StringLiteral<'\\','0','0','7'>; };
		template<> struct EscapeChar<'b'> { static const char escaped = '\b'; using EscapedString = lexer::StringLiteral<'\\','b'>; };
		template<> struct EscapeChar<'f'> { static const char escaped = '\f'; using EscapedString = lexer::StringLiteral<'\\','f'>; };
		template<> struct EscapeChar<'n'> { static const char escaped = '\n'; using EscapedString = lexer::StringLiteral<'\\','n'>; };
		template<> struct EscapeChar<'r'> { static const char escaped = '\r'; using EscapedString = lexer::StringLiteral<'\\','r'>; };
		template<> struct EscapeChar<'t'> { static const char escaped = '\t'; using EscapedString = lexer::StringLiteral<'\\','t'>; };
		template<> struct EscapeChar<'v'> { static const char escaped = '\v'; using EscapedString = lexer::StringLiteral<'\\','0','1','3'>; };
		template<> struct EscapeChar<'\\'> { static const char escaped = '\\'; using EscapedString = lexer::StringLiteral<'\\','\\'>; };
		template<> struct EscapeChar<'\''> { static const char escaped = '\''; using EscapedString = lexer::StringLiteral<'\''>; };
		template<> struct EscapeChar<'\"'> { static const char escaped = '\"'; using EscapedString = lexer::StringLiteral<'\\','\"'>; };
		template<> struct EscapeChar<'\?'> { static const char escaped = '\?'; using EscapedString = lexer::StringLiteral<'\?'>; };
		// '\xdd' istruct s not supported

		template<char...>
		struct NextEscapeChar {};

		template<char...>
		struct NextEscapeString {};

		struct NullToken {};

		// --- Lexer rule

		// ------ rule for a char

		template<char C, typename IsHeadIdUse<C>::type = nullptr>
		auto fold( prelexer::SourceChar<C> )
			-> CombSingle<lexer::IdentifierToken<C>>;

		template<char C, typename IsNum<C>::type = nullptr>
		auto fold( prelexer::SourceChar<C> )
			-> CombSingle<lexer::IntLiteralToken<C>>;

		template<char C, typename IsSymbol<C>::type = nullptr>
		auto fold( prelexer::SourceChar<C> )
			-> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<C>>, NullToken>;

		template<char C, typename IsSpace<C>::type = nullptr>
		auto fold( prelexer::SourceChar<C> )
			-> CombTriple<NullToken, lexer::TokenTuple<>, NullToken>;

		template<char C>
		auto fold( prelexer::CharLiteralChar<C> )
			-> CombSingle<lexer::CharLiteral<C>>;

		template<char C>
		auto fold( prelexer::StringLiteralChar<C> )
			-> CombSingle<lexer::StringLiteral<C>>;

		template<char C>
		auto fold( prelexer::CommentBegin<C> )
			-> CombSingle<lexer::detail::CommentBegin<C>>;

		template<char C>
		auto fold( prelexer::CommentChar<C> )
			-> CombTriple<NullToken, lexer::TokenTuple<>, NullToken>;

		template<char C>
		auto fold( prelexer::NextEscapeCharChar<C> )
			-> CombSingle<NextEscapeChar<>>;

		template<char C>
		auto fold( prelexer::NextEscapeStringChar<C> )
			-> CombSingle<NextEscapeString<>>;

		// --------- may be part of some operators
		auto fold( prelexer::SourceChar<'='> ) -> CombSingle<lexer::Symbol<'='>>;
		auto fold( prelexer::SourceChar<'|'> ) -> CombSingle<lexer::Symbol<'|'>>;
		auto fold( prelexer::SourceChar<'^'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, lexer::Symbol<'^'>>;
		auto fold( prelexer::SourceChar<'&'> ) -> CombSingle<lexer::Symbol<'&'>>;
		auto fold( prelexer::SourceChar<'<'> ) -> CombSingle<lexer::Symbol<'<'>>;
		auto fold( prelexer::SourceChar<'>'> ) -> CombSingle<lexer::Symbol<'>'>>;
		auto fold( prelexer::SourceChar<'+'> ) -> CombSingle<lexer::Symbol<'+'>>;
		auto fold( prelexer::SourceChar<'-'> ) -> CombSingle<lexer::Symbol<'-'>>;
		auto fold( prelexer::SourceChar<'*'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, lexer::Symbol<'*'>>;
		auto fold( prelexer::SourceChar<'/'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, lexer::Symbol<'/'>>;
		auto fold( prelexer::SourceChar<'%'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, lexer::Symbol<'%'>>;
		auto fold( prelexer::SourceChar<'!'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, lexer::Symbol<'!'>>;

		// --------- literal begin/end symbols
		auto fold( prelexer::SourceChar<'\''> ) -> CombSingle<lexer::Symbol<'\''>>;
		auto fold( prelexer::SourceChar<'"'> ) -> CombSingle<lexer::Symbol<'"'>>;

		// ------ token concatnation rule

		template<char... C1, char... C2>
		auto concat( lexer::IntLiteralToken<C1...>, lexer::IntLiteralToken<C2...> )
			-> CombSingle<lexer::IntLiteralToken<C1..., C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::IntLiteralToken<C1...>, lexer::detail::TokenBase<C2...> )
			-> CombSingle<lexer::PostfixToken<C1..., C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::IdentifierToken<C1...>, lexer::detail::TokenBase<C2...> )
			-> CombSingle<lexer::IdentifierToken<C1..., C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::PostfixToken<C1...>, lexer::detail::TokenBase<C2...> )
			-> CombSingle<lexer::PostfixToken<C1..., C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::IntLiteralToken<C1...>, lexer::Symbol<C2...> )
		-> CombTriple<lexer::IntLiteralToken<C1...>, lexer::TokenTuple<>, lexer::Symbol<C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::PostfixToken<C1...>, lexer::Symbol<C2...> )
			-> CombTriple<lexer::PostfixToken<C1...>, lexer::TokenTuple<>, lexer::Symbol<C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::IdentifierToken<C1...>, lexer::Symbol<C2...> )
			-> CombTriple<lexer::IdentifierToken<C1...>, lexer::TokenTuple<>, lexer::Symbol<C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::Symbol<C1...>, lexer::IntLiteralToken<C2...> )
			-> CombTriple<lexer::Symbol<C1...>, lexer::TokenTuple<>, lexer::IntLiteralToken<C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::Symbol<C1...>, lexer::PostfixToken<C2...> )
			-> CombTriple<lexer::Symbol<C1...>, lexer::TokenTuple<>, lexer::PostfixToken<C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::Symbol<C1...>, lexer::IdentifierToken<C2...> )
			-> CombTriple<lexer::Symbol<C1...>, lexer::TokenTuple<>, lexer::IdentifierToken<C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::Symbol<C1...>, lexer::Symbol<C2...> )
			-> CombTriple<lexer::Symbol<C1...>, lexer::TokenTuple<>, lexer::Symbol<C2...>>;

		template<char... C>
		auto concat( lexer::Symbol<'\''>, lexer::CharLiteral<C...> )
			-> CombTriple<NullToken, lexer::TokenTuple<>, lexer::CharLiteral<C...>>;

		template<char... C>
		auto concat( lexer::Symbol<'\''>, lexer::detail::NextEscapeChar<C...> )
			-> CombTriple<NullToken, lexer::TokenTuple<>, lexer::detail::NextEscapeChar<C...>>;

		template<char... C>
		auto concat( lexer::CharLiteral<C...>, lexer::Symbol<'\''> )
			-> CombTriple<lexer::CharLiteral<C...>, lexer::TokenTuple<>, NullToken>;

		template<char... C1, char... C2>
		auto concat( lexer::CharLiteral<C1...>, lexer::CharLiteral<C2...> )
			-> CombSingle<lexer::CharLiteral<C1..., C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::CharLiteral<C1...>, lexer::detail::NextEscapeChar<C2...> )
			-> CombSingle<lexer::detail::NextEscapeChar<C1..., C2...>>;

		template<char... C1, char C, char... C2>
		auto concat( lexer::detail::NextEscapeChar<C1...>, lexer::CharLiteral<C, C2...> )
			-> CombSingle<lexer::CharLiteral<C1..., EscapeChar<C>::escaped, C2...>>;

		template<char... C1, char C, char... C2>
		auto concat( lexer::detail::NextEscapeChar<C1...>, lexer::detail::NextEscapeChar<C2...> )
			-> CombSingle<lexer::detail::NextEscapeChar<C1..., EscapeChar<C>::escaped, C2...>>;

		template<char... C>
		auto concat( lexer::Symbol<'"'>, lexer::StringLiteral<C...> )
			-> CombTriple<NullToken, lexer::TokenTuple<>, lexer::StringLiteral<C...>>;

		template<char... C>
		auto concat( lexer::Symbol<'"'>, lexer::detail::NextEscapeString<C...> )
			-> CombTriple<NullToken, lexer::TokenTuple<>, lexer::detail::NextEscapeString<C...>>;

		template<char... C>
		auto concat( lexer::StringLiteral<C...>, lexer::Symbol<'"'> )
			-> CombTriple<lexer::StringLiteral<C...>, lexer::TokenTuple<>, NullToken>;

		template<char... C1, char... C2>
		auto concat( lexer::StringLiteral<C1...>, lexer::StringLiteral<C2...> )
			-> CombSingle<lexer::StringLiteral<C1..., C2...>>;

		template<char... C1, char... C2>
		auto concat( lexer::StringLiteral<C1...>, lexer::detail::NextEscapeString<C2...> )
			-> CombSingle<lexer::detail::NextEscapeString<C1..., C2...>>;

		template<char... C1, char... C2, char... C3>
		auto concat( lexer::StringLiteral<C1...>, lexer::StringLiteral<C2...>, lexer::StringLiteral<C3...> )
			-> CombSingle<lexer::StringLiteral<C1..., C2..., C3...>>;

		template<char... C1, char C, char... C2>
		auto concat( lexer::detail::NextEscapeString<C1...>, lexer::StringLiteral<C, C2...> )
			-> decltype( concat( lexer::StringLiteral<C1...>{}, typename EscapeChar<C>::EscapedString{}, lexer::StringLiteral<C2...>{} ) );

		template<char... C1, char... C2, char... C3>
		auto concat( lexer::StringLiteral<C1...>, lexer::StringLiteral<C2...>, lexer::detail::NextEscapeString<C3...> )
			-> CombSingle<lexer::detail::NextEscapeString<C1..., C2..., C3...>>;

		template<char... C1, char C, char... C2>
		auto concat( lexer::detail::NextEscapeString<C1...>, lexer::detail::NextEscapeString<C, C2...> )
			-> decltype( concat( lexer::StringLiteral<C1...>{}, typename EscapeChar<C>::EscapedString{}, lexer::detail::NextEscapeString<C2...>{} ) );

		auto concat( lexer::Symbol<'/'>, lexer::detail::CommentBegin<'/'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, NullToken>;
		auto concat( lexer::Symbol<'/'>, lexer::detail::CommentBegin<'*'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, NullToken>;


		auto concat( lexer::Symbol<'"'>, lexer::Symbol<'"'> ) -> CombTriple<NullToken, lexer::TokenTuple<StringLiteral<>>, NullToken>;
		auto concat( lexer::Symbol<'<'>, lexer::Symbol<'<'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, lexer::Symbol<'<','<'>>;
		auto concat( lexer::Symbol<'<'>, lexer::Symbol<'='> ) -> CombTriple<lexer::Symbol<'<','='>, lexer::TokenTuple<>, NullToken>;
		auto concat( lexer::Symbol<'>'>, lexer::Symbol<'>'> ) -> CombTriple<NullToken, lexer::TokenTuple<>, lexer::Symbol<'>','>'>>;
		auto concat( lexer::Symbol<'>'>, lexer::Symbol<'='> ) -> CombTriple<lexer::Symbol<'>','='>, lexer::TokenTuple<>, NullToken>;
		auto concat( lexer::Symbol<'+'>, lexer::Symbol<'+'> ) -> CombTriple<lexer::Symbol<'+','+'>, lexer::TokenTuple<>, NullToken>;
		auto concat( lexer::Symbol<'-'>, lexer::Symbol<'-'> ) -> CombTriple<lexer::Symbol<'-','-'>, lexer::TokenTuple<>, NullToken>;
		auto concat( lexer::Symbol<'&'>, lexer::Symbol<'&'> ) -> CombTriple<lexer::Symbol<'&','&'>, lexer::TokenTuple<>, NullToken>;
		auto concat( lexer::Symbol<'|'>, lexer::Symbol<'|'> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'|','|'>>, NullToken>;
		auto concat( lexer::Symbol<'='>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'=','='>>, NullToken>;
		auto concat( lexer::Symbol<'!'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'!','='>>, NullToken>;
		auto concat( lexer::Symbol<'|'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'|','='>>, NullToken>;
		auto concat( lexer::Symbol<'^'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'^','='>>, NullToken>;
		auto concat( lexer::Symbol<'&'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'&','='>>, NullToken>;
		auto concat( lexer::Symbol<'+'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'+','='>>, NullToken>;
		auto concat( lexer::Symbol<'-'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'-','='>>, NullToken>;
		auto concat( lexer::Symbol<'*'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'*','='>>, NullToken>;
		auto concat( lexer::Symbol<'/'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'/','='>>, NullToken>;
		auto concat( lexer::Symbol<'%'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'%','='>>, NullToken>;
		auto concat( lexer::Symbol<'<','<'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'<','<','='>>, NullToken>;
		auto concat( lexer::Symbol<'<'>, lexer::Symbol<'<','='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'<','<','='>>, NullToken>;
		auto concat( lexer::Symbol<'>','>'>, lexer::Symbol<'='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'>','>','='>>, NullToken>;
		auto concat( lexer::Symbol<'>'>, lexer::Symbol<'>','='> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'>','>','='>>, NullToken>;
		auto concat( lexer::Symbol<'&'>, lexer::Symbol<'&','&'> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'&','&'>, lexer::Symbol<'&'>>, NullToken>;
		auto concat( lexer::Symbol<'+'>, lexer::Symbol<'+','+'> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'+','+'>, lexer::Symbol<'+'>>, NullToken>;
		auto concat( lexer::Symbol<'-'>, lexer::Symbol<'-','-'> ) -> CombTriple<NullToken, lexer::TokenTuple<lexer::Symbol<'-','-'>, lexer::Symbol<'-'>>, NullToken>;

		// ------ 'conquer' phase rule

		template<class T1, class T2>
		auto merge( CombSingle<T1>, CombSingle<T2> )
			-> decltype( concat( T1{}, T2{} ) );

		// 返り型のdecltypeの中に自分自身に似た関数呼び出しがある場合は、再帰的テンプレートインスタンス化を防ぐため、else側でもSFINAEで切らないといけない
		template<class T3, class T4, class... T5, class T6, typename std::enable_if<!std::is_same<T4, NullToken>::value, std::nullptr_t>::type = nullptr>
		auto merge( CombSingle<T3>, CombTriple<T4, lexer::TokenTuple<T5...>, T6> )
			-> decltype( merge( concat( T3{}, T4{} ), CombTriple<NullToken, lexer::TokenTuple<T5...>, T6>{} ) );
		template<class T3, class... T5, class T6>
		auto merge( CombSingle<T3>, CombTriple<NullToken, lexer::TokenTuple<T5...>, T6> )
			-> CombTriple<T3, lexer::TokenTuple<T5...>, T6>;

		template<class T1, class... T2, class T3, class T4, class... T5, class T6>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, T3>, CombTriple<T4, lexer::TokenTuple<T5...>, T6> )
			-> decltype( merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>{}, concat( T3{}, T4{} ), CombTriple<NullToken, lexer::TokenTuple<T5...>, T6>{} ) );
		template<class T1, class... T2, class T4, class... T5, class T6>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombTriple<T4, lexer::TokenTuple<T5...>, T6> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T4, T5...>, T6>;
		template<class T1, class... T2, class T3, class... T5, class T6>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, T3>, CombTriple<NullToken, lexer::TokenTuple<T5...>, T6> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T3, T5...>, T6>;
		template<class T1, class... T2, class... T5, class T6>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombTriple<NullToken, lexer::TokenTuple<T5...>, T6> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T5...>, T6>;

		template<class T1, class... T2, class T3, class T4, typename std::enable_if<!std::is_same<T3, NullToken>::value, std::nullptr_t>::type = nullptr>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, T3>, CombSingle<T4> )
			-> decltype( merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>{}, concat( T3{}, T4{} ) ) );
		template<class T1, class... T2, class T4>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombSingle<T4> )
			-> CombTriple<T1, lexer::TokenTuple<T2...>, T4>;


		template<class T1, class... T2, class T3, class... T4, class T5, class... T6, class T7>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombTriple<T3, lexer::TokenTuple<T4...>, T5>, CombTriple<NullToken, lexer::TokenTuple<T6...>, T7> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T3, T4..., T5, T6...>, T7>;
		template<class T1, class... T2, class... T4, class T5, class... T6, class T7>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombTriple<NullToken, lexer::TokenTuple<T4...>, T5>, CombTriple<NullToken, lexer::TokenTuple<T6...>, T7> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T4..., T5, T6...>, T7>;
		template<class T1, class... T2, class T3, class... T4, class... T6, class T7>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombTriple<T3, lexer::TokenTuple<T4...>, NullToken>, CombTriple<NullToken, lexer::TokenTuple<T6...>, T7> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T3, T4..., T6...>, T7>;
		template<class T1, class... T2, class... T4, class... T6, class T7>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombTriple<NullToken, lexer::TokenTuple<T4...>, NullToken>, CombTriple<NullToken, lexer::TokenTuple<T6...>, T7> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T4..., T6...>, T7>;
		template<class T1, class... T2, class T4, class... T6, class T7>
		auto merge( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken>, CombSingle<T4>, CombTriple<NullToken, lexer::TokenTuple<T6...>, T7> )
			-> CombTriple<T1, lexer::TokenTuple<T2..., T4, T6...>, T7>;

		// ------ final promotion

		template<class T1, class... T2, class T3>
		auto promoteTokenTuple( CombTriple<T1, lexer::TokenTuple<T2...>, T3> )
			-> lexer::TokenTuple<T1, T2..., T3>;
		template<class... T2, class T3>
		auto promoteTokenTuple( CombTriple<NullToken, lexer::TokenTuple<T2...>, T3> )
			-> lexer::TokenTuple<T2..., T3>;
		template<class T1, class... T2>
		auto promoteTokenTuple( CombTriple<T1, lexer::TokenTuple<T2...>, NullToken> )
			-> lexer::TokenTuple<T1, T2...>;
		template<class... T2>
		auto promoteTokenTuple( CombTriple<NullToken, lexer::TokenTuple<T2...>, NullToken> )
			-> lexer::TokenTuple<T2...>;

		// そもそもこれが選ばれるとvalidなソースコードではなさそう
		template<class T1>
		auto promoteTokenTuple( CombSingle<T1> )
			-> lexer::TokenTuple<T1>;

		template<class FlatTuple>
		struct Fold;
		template<class T>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, T>> {
			using type = decltype( fold( T{} ) );
		};
		template<lfl::index_t... Index, class... Ts>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<Index...>, Ts...>> {
			using tuple = lfl::flat_tuple<lfl::index_tuple<Index...>, Ts...>;
			using type = decltype( merge( typename Fold<decltype(head( tuple{} ))>::type {}, typename Fold<decltype(tail( tuple{} ))>::type {} ) );
		};

		template<class... ColoredChars>
		using Result = decltype( promoteTokenTuple( typename Fold<lfl::flat_tuple<lfl::make_index_tuple<sizeof...(ColoredChars)>, ColoredChars...>>::type {} ) );

	} // namespace detail

	// --- interface ---

	template<class... ColoredChars>
	using Result = detail::Result<ColoredChars...>;

} // namespace lexer

#endif // LTMPC_LEXER_HPP
