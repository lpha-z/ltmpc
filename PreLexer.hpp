#ifndef LTMPC_PRELEXER_HPP
#define LTMPC_PRELEXER_HPP
#include <type_traits>
#include "flat_tuple.hpp"

namespace prelexer {

	// --- interface ---
	// ------ Char Class ---
	template<char C>
	struct SourceChar {};

	template<char C>
	struct CharLiteralChar {};

	template<char C>
	struct NextEscapeCharChar {};

	template<char C>
	struct NextEscapeStringChar {};

	template<char C>
	struct StringLiteralChar {};

	template<char C>
	struct CommentBegin {};

	template<char C>
	struct CommentChar {};

	// ------ char_tuple ---
	template<class... Ts>
	struct char_tuple {};

	namespace detail {

		// --- Text Char ---
		template<char C>
		struct TextChar {};

		template<class... Ts, class... Us>
		auto merge( char_tuple<Ts...>, char_tuple<Us...> )
			-> char_tuple<Ts..., Us...>;

		// --- State ---
		template<bool Slash>
		struct Normal;
		template<bool NextEscape>
		struct InCharLiteral;
		template<bool NextEscape>
		struct InStringLiteral;
		template<bool Asterisk>
		struct InComment;

		// --- linear fold (2 return types) ---

		template<class FlatTuple, class State>
		struct Fold {
			using head_result = Fold<decltype( head( FlatTuple {} ) ), State>;
			using tail_result = Fold<decltype( tail( FlatTuple {} ) ), typename head_result::next_state>;
			using char_type = decltype( merge( typename head_result::char_type {}, typename tail_result::char_type {} ) );
			using next_state = typename tail_result::next_state;
		};

		template<char C, bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<C>>, Normal<b>> {
			using char_type = char_tuple<SourceChar<C>>;
			using next_state = Normal<false>;
		};

		template<bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'\''>>, Normal<b>> {
			using char_type = char_tuple<SourceChar<'\''>>;
			using next_state = InCharLiteral<false>;
		};

		template<char C, bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<C>>, InCharLiteral<b>> {
			using char_type = char_tuple<CharLiteralChar<C>>;
			using next_state = InCharLiteral<false>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'\\'>>, InCharLiteral<false>> {
			using char_type = char_tuple<NextEscapeCharChar<'\\'>>;
			using next_state = InCharLiteral<true>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'\''>>, InCharLiteral<true>> {
			using char_type = char_tuple<CharLiteralChar<'\''>>;
			using next_state = InCharLiteral<false>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'\''>>, InCharLiteral<false>> {
			using char_type = char_tuple<SourceChar<'\''>>;
			using next_state = Normal<false>;
		};

		template<bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'"'>>, Normal<b>> {
			using char_type = char_tuple<SourceChar<'"'>>;
			using next_state = InStringLiteral<false>;
		};

		template<char C, bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<C>>, InStringLiteral<b>> {
			using char_type = char_tuple<StringLiteralChar<C>>;
			using next_state = InStringLiteral<false>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'\\'>>, InStringLiteral<false>> {
			using char_type = char_tuple<NextEscapeStringChar<'\\'>>;
			using next_state = InStringLiteral<true>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'"'>>, InStringLiteral<false>> {
			using char_type = char_tuple<SourceChar<'"'>>;
			using next_state = Normal<false>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'"'>>, InStringLiteral<true>> {
			using char_type = char_tuple<StringLiteralChar<'"'>>;
			using next_state = InStringLiteral<false>;
		};

		template<bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'/'>>, Normal<b>> {
			using char_type = char_tuple<SourceChar<'/'>>;
			using next_state = Normal<true>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'*'>>, Normal<true>> {
			using char_type = char_tuple<CommentBegin<'*'>>;
			using next_state = InComment<false>;
		};

		template<char C, bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<C>>, InComment<b>> {
			using char_type = char_tuple<CommentChar<C>>;
			using next_state = InComment<false>;
		};

		template<bool b>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'*'>>, InComment<b>> {
			using char_type = char_tuple<CommentChar<'*'>>;
			using next_state = InComment<true>;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'/'>>, InComment<true>> {
			using char_type = char_tuple<CommentChar<'/'>>;
			using next_state = Normal<false>;
		};

#ifdef USE_CPP_STYLE_COMMENT
		struct InCppComment;

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'/'>>, Normal<true>> {
			using char_type = char_tuple<CommentBegin<'/'>>;
			using next_state = InCppComment;
		};

		template<char C>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<C>>, InCppComment> {
			using char_type = char_tuple<CommentChar<C>>;
			using next_state = InCppComment;
		};

		template<>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, TextChar<'\n'>>, InCppComment> {
			using char_type = char_tuple<CommentChar<'\n'>>;
			using next_state = Normal<false>;
		};

#endif // USE_CPP_STYLE_COMMENT

		namespace diagnostics {

			template<class T>
			struct Diagnostics {
				static_assert( !std::is_same<T, detail::Normal<true>>::value, "PreLexer error: no newline at end of source code" );
				static_assert( !std::is_same<T, detail::InCharLiteral<true>>::value, "PreLexer error: missing terminating ' character" );
				static_assert( !std::is_same<T, detail::InCharLiteral<false>>::value, "PreLexer error: missing terminating ' character" );
				static_assert( !std::is_same<T, detail::InStringLiteral<true>>::value, "PreLexer error: missing terminating \" character" );
				static_assert( !std::is_same<T, detail::InStringLiteral<false>>::value, "PreLexer error: missing terminating \" character" );
				static_assert( !std::is_same<T, detail::InComment<true>>::value, "PreLexer error: no newline at end of source code" );
				static_assert( !std::is_same<T, detail::InComment<false>>::value, "PreLexer error: unterminated /* comment" );
#ifdef USE_CPP_STYLE_COMMENT
				static_assert( !std::is_same<T, detail::InCppComment>::value, "PreLexer error: no newline at end of source code" );
#endif // USE_CPP_STYLE_COMMENT
			};

		} // namespace diagnostics

		template<char... Chars>
		struct Result {
			using fold_result = detail::Fold<lfl::flat_tuple<lfl::make_index_tuple<sizeof...(Chars)>, TextChar<Chars>...>, Normal<false>>;
			using type = typename fold_result::char_type;
			
#ifdef DIAGNOSTICS
			diagnostics::Diagnostics<typename fold_result::next_state> diag{};
#endif // DIAGNOSTICS
		};

	} // namespace detail

	// --- interface ---

	template<char... Chars>
	using Result = typename detail::Result<Chars...>::type;

} // namespace prelexer

#endif // LTMPC_PRELEXER_HPP
