#include <iostream>
#include <string>
#include "Parser.hpp"

namespace parser {

	namespace detail {

		namespace dumper {
			std::string to_string( lr_stack_bottom, int, bool ) { return ""; }

			template<char... Chars>
			std::string to_string( Name<Chars...>, int i, bool b ) { return std::string(b?i*5:0,' ') + std::string { Chars... }; }

			template<char... Chars>
			std::string to_string( IntLiteral<Chars...>, int i, bool b ) { return std::string(b?i*5:0,' ') + std::to_string( IntLiteral<Chars...>::value ); }

			template<char... Chars>
			std::string to_string( CharLiteral<Chars...>, int i, bool b ) { return std::string(b?i*5:0, ' ') + std::string { '\'', Chars..., '\'' }; }

			template<char... Chars>
			std::string to_string( StringLiteral<Chars...>, int i, bool b ) { return std::string(b?i*5:0,' ') + std::string { '\"', Chars..., '\"' }; }

			template<class T, class... Ts>
			std::string Node_to_string(int i, bool b, std::string s);

			template<class FuncName, class... Args>
			std::string to_string( Func_Node<FuncName, Args...>, int i, bool b );

			template<class ArrayName, class Subscript>
			std::string to_string( Subscript_Node<ArrayName, Subscript>, int i, bool b );

			std::string to_string( type_int ) { return "int"; }
			std::string to_string( type_char ) { return "char"; }
			std::string to_string( type_void ) { return "void"; }
			template<char... Chars>
			std::string to_string( Name<Chars...>, bool ) { return std::string { Chars... } + " is "; }
			std::string to_string( NullDeclarator, bool isCast ) { return isCast?"cast as ":"<arg>:"; }

			template<class T, class Ty>
			std::string to_string( Declare<T, Ty>, int i, bool b, bool isCast = false );
			template<class T>
			std::string to_string( Array<T, ArraySize<>>, bool );
			template<class T, std::size_t N>
			std::string to_string( Array<T, ArraySize<N>>, bool );
			template<class T, class... PL>
			std::string to_string( Function<T, ParameterList<PL...>>, bool );
			template<class T>
			std::string to_string( DirectDeclarator<T, Pointer<0>>, bool );
			template<class T, std::size_t N>
			std::string to_string( DirectDeclarator<T, Pointer<N>>, bool );



			template<class T>
			std::string to_string( Array<T, ArraySize<>>, bool isCast ) { return to_string( T{}, isCast ) + "X elements array of "; }
			template<class T, std::size_t N>
			std::string to_string( Array<T, ArraySize<N>>, bool isCast ) { return to_string( T{}, isCast ) + std::to_string(N) + " elements array of "; }
			template<class T, class PL1, class... PL>
			std::string to_string( Function<T, ParameterList<PL1, PL...>>, bool isCast ) { return to_string( T{}, isCast ) + "function(" + (to_string( PL1{}, 0, false ) + ... + (", "+to_string( PL{}, 0, false ))) + ") returns "; }
			template<class T>
			std::string to_string( Function<T, ParameterList<>>, bool isCast ) { return to_string( T{}, isCast ) + "function(any) returns "; }
			template<class T>
			std::string to_string( Function<T, ParameterList<Declare<DirectDeclarator<NullDeclarator, Pointer<0>>, type_void>>>, bool isCast ) { return to_string( T{}, isCast ) + "function(no-arg) returns "; }

			template<class T>
			std::string to_string( DirectDeclarator<T, Pointer<0>>, bool isCast ) { return to_string( T{}, isCast ); }
			template<class T, std::size_t N>
			std::string to_string( DirectDeclarator<T, Pointer<N>>, bool isCast ) { return to_string( DirectDeclarator<T, Pointer<N-1>>{}, isCast ) + "pointer to "; }

			template<class T, class Ty>
			std::string to_string( Declare<T, Ty>, int i, bool b, bool isCast ) { return std::string(b?i*5:0,' ') + to_string( T{}, isCast ) + to_string( Ty{} ); }



			template<class Ty, class T, class Ty2, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, type_analysis::IntegralPromotion<type_analysis::Typed<Ty2, T, Lval>>, false>, int i, bool b );
			template<class Ty, class T, class Ty2, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, type_analysis::ArrayDecay<type_analysis::Typed<Ty2, T, Lval>>, false>, int i, bool b );
			template<class Ty, class T, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, T, Lval>, int i, bool b );
			template<class Ty, class T, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, Term_Node<T>, Lval>, int i, bool b );
			template<class Ty, std::size_t... N>
			std::string to_string( type_analysis::ArrayOf<Ty, N...> );
			template<class Ty>
			std::string to_string( type_analysis::PtrTo<Ty> );
			template<class RetTy>
			std::string to_string( type_analysis::Function<RetTy> );
			template<class RetTy, class Ty, class... Tys>
			std::string to_string( type_analysis::Function<RetTy, Ty, Tys...> );
			std::string to_string( type_analysis::type_int ) { return "int"; }
			std::string to_string( type_analysis::type_char ) { return "char"; }
			std::string to_string( type_analysis::type_void ) { return "void"; }
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::ForStatement_Node<T...>>, int i, bool b );
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::DoStatement_Node<T...>>, int i, bool b );
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::WhileStatement_Node<T...>>, int i, bool b );
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::IfStatement_Node<T...>>, int i, bool b );
			template<class Label, class T>
			std::string to_string( type_analysis::Labeled<Label, T>, int i, bool b );




			template<std::ptrdiff_t N>
			std::string to_string( location::SetSP_Node<location::FramePosition<N>>, int i, bool b ) { return std::string(b?i*5:0,' ') + "esp = ebp" + (N<0?"":"+") + std::to_string(N); }
			std::string to_string( location::Return_Node, int i, bool b ) { return std::string(b?i*5:0,' ')+"ret"; };
			template<std::ptrdiff_t N>
			std::string to_string( location::FramePosition<N>, int i, bool b ) { return std::string(b?i*5:0,' ') + "[ebp" + (N<0?"":"+") + std::to_string(N) + "]"; }
			template<std::size_t N>
			std::string to_string( location::StringRef<N>, int i, bool b ) { return std::string(b?i*5:0,' ') + "StringRef<" + std::to_string(N) + ">"; }
			template<char... Chars>
			std::string to_string( type_analysis::Identifier<Chars...>, int i, bool b ) { return std::string(b?i*5:0,' ') + std::string { '.', Chars... }; }


			std::string to_string( NullStatement, int i, bool b ) { return std::string(b?i*5:0,' ') + "nullStatement"; }
			std::string to_string( BreakStatement, int i, bool b ) { return std::string(b?i*5:0,' ') + "breakStatement"; }
			std::string to_string( ContinueStatement, int i, bool b ) { return std::string(b?i*5:0,' ') + "continueStatement"; }
			std::string to_string( NullExpr, int i, bool b ) { return std::string(b?i*5:0,' ') + "<nullExpr>"; }
			template<class T>
			std::string to_string( ReturnStatement<T>, int i, bool b ) { return Node_to_string<T>( i, b,  "returnStatement" ); }
			std::string to_string( ReturnVoidStatement, int i, bool b ) { return std::string(b?i*5:0,' ') + "returnStatement[ <void> ]"; }
			template<class... T>
			std::string to_string( DeclareStatement_Node<T...>, int i, bool b ) { return Node_to_string<T...>( i, b, "declareStatement" ); }
			template<class... T>
			std::string to_string( ForStatement_Node<T...>, int i, bool b ) { return Node_to_string<T...>( i, b, "forStatement" ); }
			template<class... T>
			std::string to_string( DoStatement_Node<T...>, int i, bool b ) { return Node_to_string<T...>( i, b, "doStatement" ); }
			template<class... T>
			std::string to_string( WhileStatement_Node<T...>, int i, bool b ) { return Node_to_string<T...>( i, b, "whileStatement" ); }
			template<class... T>
			std::string to_string( IfStatement_Node<T...>, int i, bool b ) { return Node_to_string<T...>( i, b, "ifStatement" ); }
			template<class... T>
			std::string to_string( BraceStatement_Node<T...>, int i, bool b ) { return Node_to_string<T...>( i, b, "braceStatement" ); }
			template<>
			std::string to_string( BraceStatement_Node<>, int i, bool b ) { return std::string(b?i*5:0,' ') + "BraceStatement[null]"; }
			template<class T>
			std::string to_string( ExprStatement_Node<T>, int i, bool b ) { return Node_to_string<T>( i, b, "exprStatement" ); }
			template<class T>
			std::string to_string( Statement_Node<T>, int i, bool b ) { return to_string( T{}, i, b ); }
			template<class DD, class Ty, class St>
			std::string to_string( FunctionDefinition<DD, Ty, St>, int i, bool b ) { return std::string(b?i*5:0,' ') + to_string( Declare<DD, Ty>{}, i, b ) + " -> " + to_string( St{}, i, b ); }

			template<class T1, class T2>
			std::string to_string( CommaExpr_Node<T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "CommaExpr" ); }
			template<class T>
			std::string to_string( CommaExpr_Node<T>, int i, bool b ) { return Node_to_string<T>( i, b, "CommaExpr" ); }

			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::NormalAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Assign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::OrAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "OrAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::XorAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "XorAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::AndAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "AndAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::LeftShiftAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "LeftShiftAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::RightShiftAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "RightShiftAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::PlusAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "PlusAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::MinusAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "MinusAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::MulAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "MulAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::DivAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "DivAssign" ); }
			template<class T1, class T2>
			std::string to_string( Assign_Node<AssignOpType::ModAssign, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "ModAssign" ); }

			template<class T1, class T2, class T3>
			std::string to_string( Conditional_Node<T1, T2, T3>, int i, bool b ) { return Node_to_string<T1, T2, T3>( i, b, "Conditional" ); }

			template<class T>
			std::string to_string( Cond_Node<T>, int i, bool b ) { return to_string(T{}); }

			template<class T1, class T2>
			std::string to_string( LogicOr_Node<T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "LogicOr" ); }

			template<class T1, class T2>
			std::string to_string( LogicAnd_Node<T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "LogicAnd" ); }

			template<class T1, class T2>
			std::string to_string( BitOr_Node<T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "BitOr" ); }

			template<class T1, class T2>
			std::string to_string( BitXor_Node<T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "BitXor" ); }

			template<class T1, class T2>
			std::string to_string( BitAnd_Node<T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "BitAnd" ); }

			template<class T1, class T2>
			std::string to_string( EqComp_Node<EqCompOpType::Equal, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Equal" ); }

			template<class T1, class T2>
			std::string to_string( EqComp_Node<EqCompOpType::NotEqual, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "NotEqual" ); }

			template<class T1, class T2>
			std::string to_string( Compare_Node<CompareOpType::LessThan, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "LessThan" ); }

			template<class T1, class T2>
			std::string to_string( Compare_Node<CompareOpType::EqualOrLessThan, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Eq-LessThan" ); }

			template<class T1, class T2>
			std::string to_string( Compare_Node<CompareOpType::GraterThan, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "GraterThan" ); }

			template<class T1, class T2>
			std::string to_string( Compare_Node<CompareOpType::EqualOrGraterThan, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Eq-GraterThan" ); }

			template<class T1, class T2>
			std::string to_string( Shift_Node<ShiftOpType::LeftShift, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "LeftShift" ); }

			template<class T1, class T2>
			std::string to_string( Shift_Node<ShiftOpType::RightShift, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "RightShift" ); }

			template<class T1, class T2>
			std::string to_string( Plus_Node<PlusOpType::Plus, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Plus" ); }

			template<class T1, class T2>
			std::string to_string( Plus_Node<PlusOpType::Minus, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Minus" ); }

			template<class T1, class T2>
			std::string to_string( Mult_Node<MultOpType::Mult, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Mult" ); }

			template<class T1, class T2>
			std::string to_string( Mult_Node<MultOpType::Divide, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Div" ); }

			template<class T1, class T2>
			std::string to_string( Mult_Node<MultOpType::Modulo, T1, T2>, int i, bool b ) { return Node_to_string<T1, T2>( i, b, "Mod" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::Plus, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryPlus" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::Minus, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryMinus" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::Increment, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryIncr" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::Decrement, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryDecr" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::AddressOf, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryAddrOf" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::Dereference, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryDeref" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::LogicalNot, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryLogicNot" ); }

			template<class T>
			std::string to_string( Unary_Node<PrefixUnaryOpType::BitNot, T>, int i, bool b ) { return Node_to_string<T>( i, b, "UnaryBitNot" ); }

			template<class T, class Ty>
			std::string to_string( Cast_Node<Ty, T>, int i, bool b ) { return Node_to_string<T>( i, b, to_string(Ty{},i,false,true) ); }


			template<class T>
			std::string to_string( SuffixUnary_Node<SuffixUnaryOpType::Increment, T>, int i, bool b ) { return Node_to_string<T>( i, b, "PostIncr" ); }

			template<class T>
			std::string to_string( SuffixUnary_Node<SuffixUnaryOpType::Decrement, T>, int i, bool b ) { return Node_to_string<T>( i, b, "PostDecr" ); }

			template<class T>
			std::string to_string( Term_Node<T>, int i, bool b ) { return std::string(b?i*5:0,' ')+to_string(T{},i,b); }

			template<class T, class... Ts>
			std::string Node_to_string(int i, bool b, std::string s) { 
				std::string indentx(b?i*5:0,' ');
				std::string indent(i*5,' ');
				//if( sizeof...(Ts) == 0 ) {
				//	return indentx+s+"[" + to_string(T{},i,false) + "]";
				//} else {
					return indentx+s+"[\n" + to_string(T{},i+1,true) + ("\n" + ... + (to_string(Ts{},i+1,true)+"\n")) + indent+"]";
				//}
			}

			template<class FuncName, class... Args>
			std::string to_string( Func_Node<FuncName, Args...>, int i, bool b ) {
				std::string indentX(b?i*5:0,' ');
				std::string indent(i*5,' ');
				if( sizeof...(Args) == 0 ) {
					return indentX+"funcname: " + to_string(FuncName{},i,false) + "\n" + indent + "       (no args)";
				} else {
					auto s = indentX+"funcname: " + to_string(FuncName{},i,false);
					return ((s+"(\n") + ... + (to_string(Args{},i+1,true)+"\n")) + indent+")";
				}
			}

			template<class ArrayName, class Subscript>
			std::string to_string( Subscript_Node<ArrayName, Subscript>, int i, bool b ) {
				std::string indentX(b?i*5:0,' ');
				std::string indent(i*5,' ');
				auto s = indentX+"array: " + to_string(ArrayName{},i,false);
				return (s+"[\n") + (to_string(Subscript{},i+1,true)+"\n") + indent+"]";
			}

			template<class Ty, class T, class Ty2, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, type_analysis::IntegralPromotion<type_analysis::Typed<Ty2, T, Lval>>, false>, int i, bool b ) {
				return to_string(T{},i,b) + " :: " + to_string(Ty2{}) + "-promote->" + to_string(Ty{}) + " (R-value)";
			}

			template<class Ty, class T, class Ty2, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, type_analysis::ArrayDecay<type_analysis::Typed<Ty2, T, Lval>>, false>, int i, bool b ) {
				return to_string(T{},i,b) + " :: " + to_string(Ty2{}) + " -decay-> " + to_string(Ty{}) + " (R-value)";
			}

			template<class Ty, class T, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, T, Lval>, int i, bool b ) {
				return to_string(T{},i,b) + " :: " + to_string(Ty{}) + (Lval?" (L-value)":" (R-value)");
			}

			template<class Ty, class T, bool Lval>
			std::string to_string( type_analysis::Typed<Ty, Term_Node<T>, Lval>, int i, bool b ) {
				return to_string(T{},i,b);
			}

			template<class Ty, std::size_t... N>
			std::string to_string( type_analysis::ArrayOf<Ty, N...> ) { return (std::to_string(N)+...+"") + std::string(" elements array of ") + to_string(Ty{}); }
			template<class Ty>
			std::string to_string( type_analysis::PtrTo<Ty> ) { return "ptr to " + to_string(Ty{}); }
			template<class RetTy>
			std::string to_string( type_analysis::Function<RetTy> ) { return "function(any) returns " + to_string(RetTy{}); }
			template<class RetTy, class Ty, class... Tys>
			std::string to_string( type_analysis::Function<RetTy, Ty, Tys...> ) { return "function( " + (to_string(Ty{}) + ... + (", "+to_string(Tys{}))) + " ) returns " + to_string(RetTy{}); }


			template<char... Chars>
			std::string to_string( type_analysis::Identifier<Chars...> ) { return std::string{ Chars... }; }
			template<class Name>
			std::string to_string( type_analysis::LoopNestId<Name> ) { return "Label: " + to_string( Name{} ); }
			template<class Name, std::size_t N, std::size_t... Ns>
			std::string to_string( type_analysis::LoopNestId<Name, N, Ns...> ) { return to_string( type_analysis::LoopNestId<Name, Ns...>{} ) + "_" + std::to_string(N); }
			template<class Loop>
			std::string to_string( type_analysis::IfNestId<Loop> ) { return to_string( Loop{} ); }
			template<class Loop, std::size_t... Ns>
			std::string to_string( type_analysis::IfNestId<Loop, 1, Ns...> );
			template<class Loop, std::size_t... Ns>
			std::string to_string( type_analysis::IfNestId<Loop, 0, Ns...> ) { return to_string( type_analysis::IfNestId<Loop, Ns...>{} ) + "_then"; }
			template<class Loop, std::size_t... Ns>
			std::string to_string( type_analysis::IfNestId<Loop, 1, Ns...> ) { return to_string( type_analysis::IfNestId<Loop, Ns...>{} ) + "_else"; }
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::ForStatement_Node<T...>>, int i, bool b ) { return to_string( parser::ForStatement_Node<T...>{}, i, b ) + " <- " + to_string( Label{} ); }
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::DoStatement_Node<T...>>, int i, bool b ) { return to_string( parser::DoStatement_Node<T...>{}, i, b ) + " <- " + to_string( Label{} ); }
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::WhileStatement_Node<T...>>, int i, bool b ) { return to_string( parser::WhileStatement_Node<T...>{}, i, b ) + " <- " + to_string( Label{} ); }
			template<class Label, class... T>
			std::string to_string( type_analysis::Labeled<Label, parser::IfStatement_Node<T...>>, int i, bool b ) { return to_string( parser::IfStatement_Node<T...>{}, i, b ) + "<- " + to_string( Label{} ); }
			template<class Label, class T>
			std::string to_string( type_analysis::Labeled<Label, T>, int i, bool b ) { return to_string( T{}, i, b ) + " -> " + to_string( Label{} ); }

			template<class T, std::size_t N, std::size_t M>
			std::string to_string( location::GlobalVariable<T, N, M>, int i, bool b ) { return to_string( T{} ) + " : size=" + std::to_string(N) + ", align=" + std::to_string(M); }

			template<class T, class St>
			std::string to_string( location::Function<T, St>, int i, bool b ) { return to_string( T{} ) + " -> " + to_string( St{}, i, b ); }
		} // namespace dumper

	} // namespace detail

} // namespace parser

