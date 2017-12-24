#ifndef LTMPC_LTMPC_HPP
#define LTMPC_LTMPC_HPP
#include "type_tuple.hpp"
#include "PreLexer.hpp"
#include "Lexer.hpp"
#include "Parser.hpp"
#include "TypeAnalysis.hpp"
#include "Location.hpp"
#include "CodeEmitter.hpp"

constexpr const char source_code[] =
#include "8ccNQ.c.inc"

template<lfl::index_t... Index>
auto usePreLexer( lfl::index_tuple<Index...> )
	-> typename prelexer::Result<source_code[Index]...>;

template<class... ColoredChars>
auto useLexer( prelexer::char_tuple<ColoredChars...> )
	-> typename lexer::Result<ColoredChars...>;

template<class... T>
auto useParser( lexer::TokenTuple<T...> )
	-> typename parser::Result<lfl::type_tuple<T...>>;

template<class... AST>
auto useTypeAnalysis( parser::detail::type_stack<AST...> )
	-> typename type_analysis::Result<AST...>;

template<class... AST>
auto locate( type_analysis::type_tuple<AST...> )
	-> typename location::Result<AST...>;

template<class... AST>
auto codeEmit( location::GlobalEnvironment<AST...> )
	-> typename code_emitter::Result<AST...>;

using PreLexerResult = decltype(usePreLexer(lfl::make_index_tuple<sizeof source_code - 1>{}));
using LexerResult = decltype(useLexer(PreLexerResult{}));
using ParserResult = decltype(useParser(LexerResult{}));
using TypeAnalysisResult = decltype(useTypeAnalysis(ParserResult{}));
using LocateResult = decltype(locate(TypeAnalysisResult{}));
using FinalResult = decltype(codeEmit(LocateResult{}));

#include <iostream>
template<class>
struct LTMPC;
template<char... Chars>
struct LTMPC<code_emitter::Code<Chars...>> {
	static void print() {
		const char asm_code[] = { Chars..., '\0' };
		std::cout<< asm_code << std::endl;
	}
};


#endif // LTMPC_LTMPC_HPP
