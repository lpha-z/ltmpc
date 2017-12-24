#ifndef LTMPC_PARSER_HPP
#define LTMPC_PARSER_HPP
#include "type_tuple.hpp"
#include "Lexer.hpp"

namespace parser {

	namespace detail {
		template<class T>
		struct lr_element {};

		struct lr_postfix_pos {};

		template<class T>
		struct lr_suffix_pos : lr_element<T> {};

		struct lr_stack_bottom : lr_suffix_pos<lr_stack_bottom> {};
	}

	// --- Statement ---

	template<class T>
	struct Statement : detail::lr_suffix_pos<T> {};

	struct NullStatement : Statement<NullStatement> {};
	struct BreakStatement : Statement<BreakStatement> {};
	struct ContinueStatement : Statement<ContinueStatement> {};
	template<class T>
	struct ReturnStatement : Statement<ReturnStatement<T>> {};
	struct ReturnVoidStatement : Statement<ReturnVoidStatement> {};
	template<class T>
	struct Statement_Node : Statement<Statement_Node<T>> {};

	template<class...>
	struct IfStatement_Node;
	template<>
	struct IfStatement_Node<> : detail::lr_suffix_pos<IfStatement_Node<>> {};
	template<class Cond>
	struct IfStatement_Node<Cond> : detail::lr_suffix_pos<IfStatement_Node<Cond>> {};
	template<class Cond, class St>
	struct IfStatement_Node<Cond, St> : Statement<IfStatement_Node<Cond, St>> {};
	template<class Cond, class St1, class St2>
	struct IfStatement_Node<Cond, St1, St2> : Statement<IfStatement_Node<Cond, St1, St2>> {};

	template<class...>
	struct ForStatement_Node;
	template<>
	struct ForStatement_Node<> : detail::lr_suffix_pos<ForStatement_Node<>> {};
	template<class Init>
	struct ForStatement_Node<Init> : detail::lr_suffix_pos<ForStatement_Node<Init>> {};
	template<class Init, class Cond>
	struct ForStatement_Node<Init, Cond> : detail::lr_suffix_pos<ForStatement_Node<Init, Cond>> {};
	template<class Init, class Cond, class After>
	struct ForStatement_Node<Init, Cond, After> : detail::lr_suffix_pos<ForStatement_Node<Init, Cond, After>> {};
	template<class Init, class Cond, class After, class St>
	struct ForStatement_Node<Init, Cond, After, St> : Statement<ForStatement_Node<Init, Cond, After, St>> {};

	template<class...>
	struct WhileStatement_Node;
	template<>
	struct WhileStatement_Node<> : detail::lr_suffix_pos<WhileStatement_Node<>> {};
	template<class Cond>
	struct WhileStatement_Node<Cond> : detail::lr_suffix_pos<WhileStatement_Node<Cond>> {};
	template<class Cond, class St>
	struct WhileStatement_Node<Cond, St> : Statement<WhileStatement_Node<Cond, St>> {};

	template<class...>
	struct DoStatement_Node;
	template<>
	struct DoStatement_Node<> : detail::lr_suffix_pos<DoStatement_Node<>> {};
	template<class St>
	struct DoStatement_Node<St> : detail::lr_element<DoStatement_Node<St>> {};
	template<class St, class Cond>
	struct DoStatement_Node<St, Cond> : Statement<DoStatement_Node<St, Cond>> {};

	template<class... T>
	struct BraceStatement_Node : detail::lr_suffix_pos<BraceStatement_Node<T...>> {};

	template<class... T>
	struct DeclareStatement_Node : detail::lr_suffix_pos<DeclareStatement_Node<T...>> {};

	template<class T>
	struct ExprStatement_Node : Statement<ExprStatement_Node<T>> {};

	template<class...>
	struct FunctionDefinition;
	template<class DD, class Ty>
	struct FunctionDefinition<DD, Ty> : detail::lr_suffix_pos<FunctionDefinition<DD, Ty>> {};
	template<class DD, class Ty, class St>
	struct FunctionDefinition<DD, Ty, St> : detail::lr_suffix_pos<FunctionDefinition<DD, Ty, St>> {};

	// --- Expr ---

	// (expr) --.
	// func() --.
	//  var   ---`--> unary -> term -> mult -> plus -> bitand -> bitxor -> bitor -> logicand -> logicor -> assign -> commaExpr

	template<class T>
	struct Expr : detail::lr_postfix_pos {};
	struct NullExpr {};

	template<class...>
	struct CommaExpr_Node;
	template<class T1, class T2>
	struct CommaExpr_Node<T1, T2> : Expr<CommaExpr_Node<T1, T2>> {};
	template<class T>
	struct CommaExpr_Node<T>      : Expr<T> {};

	enum class AssignOpType {
		None,
		NormalAssign,
		OrAssign,
		XorAssign,
		AndAssign,
		LeftShiftAssign,
		RightShiftAssign,
		PlusAssign,
		MinusAssign,
		MulAssign,
		DivAssign,
		ModAssign,
	};

	template<AssignOpType, class...>
	struct Assign_Node;
	template<AssignOpType Op, class T1, class T2>
	struct Assign_Node<Op, T1, T2>            : CommaExpr_Node<Assign_Node<Op, T1, T2>> {};
	template<class T>
	struct Assign_Node<AssignOpType::None, T> : CommaExpr_Node<T> {};

	template<class T>
	struct Cond_Node : Assign_Node<AssignOpType::None, T> {};

	template<class...>
	struct LogicOr_Node;
	template<class T1, class T2>
	struct LogicOr_Node<T1, T2> : Cond_Node<LogicOr_Node<T1, T2>> {};
	template<class T>
	struct LogicOr_Node<T>      : Cond_Node<T> {};

	template<class...>
	struct LogicAnd_Node;
	template<class T1, class T2>
	struct LogicAnd_Node<T1, T2> : LogicOr_Node<LogicAnd_Node<T1, T2>> {};
	template<class T>
	struct LogicAnd_Node<T>      : LogicOr_Node<T> {};

	template<class...>
	struct BitOr_Node;
	template<class T1, class T2>
	struct BitOr_Node<T1, T2> : LogicAnd_Node<BitOr_Node<T1, T2>> {};
	template<class T>
	struct BitOr_Node<T>      : LogicAnd_Node<T> {};

	template<class...>
	struct BitXor_Node;
	template<class T1, class T2>
	struct BitXor_Node<T1, T2> : BitOr_Node<BitXor_Node<T1, T2>> {};
	template<class T>
	struct BitXor_Node<T>      : BitOr_Node<T> {};

	template<class...>
	struct BitAnd_Node;
	template<class T1, class T2>
	struct BitAnd_Node<T1, T2> : BitXor_Node<BitAnd_Node<T1, T2>> {};
	template<class T>
	struct BitAnd_Node<T>      : BitXor_Node<T> {};

	enum class EqCompOpType {
		None,
		Equal,
		NotEqual,
	};

	template<EqCompOpType, class...>
	struct EqComp_Node;
	template<EqCompOpType Op, class T1, class T2>
	struct EqComp_Node<Op, T1, T2>            : BitAnd_Node<EqComp_Node<Op, T1, T2>> {};
	template<class T>
	struct EqComp_Node<EqCompOpType::None, T> : BitAnd_Node<T> {};

	enum class CompareOpType {
		None,
		LessThan,
		EqualOrLessThan,
		GraterThan,
		EqualOrGraterThan,
	};

	template<CompareOpType, class...>
	struct Compare_Node;
	template<CompareOpType Op, class T1, class T2>
	struct Compare_Node<Op, T1, T2>             : EqComp_Node<EqCompOpType::None, Compare_Node<Op, T1, T2>> {};
	template<class T>
	struct Compare_Node<CompareOpType::None, T> : EqComp_Node<EqCompOpType::None, T> {};
	enum class ShiftOpType {
		None,
		LeftShift,
		RightShift,
	};

	template<ShiftOpType, class...>
	struct Shift_Node;
	template<ShiftOpType Op, class T1, class T2>
	struct Shift_Node<Op, T1, T2>           : Compare_Node<CompareOpType::None, Shift_Node<Op, T1, T2>> {};
	template<class T>
	struct Shift_Node<ShiftOpType::None, T> : Compare_Node<CompareOpType::None, T> {};

	enum class PlusOpType {
		None,
		Plus,
		Minus,
	};

	template<PlusOpType, class...>
	struct Plus_Node;
	template<PlusOpType Op, class T1, class T2>
	struct Plus_Node<Op, T1, T2>          : Shift_Node<ShiftOpType::None, Plus_Node<Op, T1, T2>> {};
	template<class T>
	struct Plus_Node<PlusOpType::None, T> : Shift_Node<ShiftOpType::None, T> {};

	enum class MultOpType {
		None,
		Mult,
		Divide,
		Modulo,
	};

	template<MultOpType, class...>
	struct Mult_Node;
	template<MultOpType Op, class T1, class T2>
	struct Mult_Node<Op, T1, T2>          : Plus_Node<PlusOpType::None, Mult_Node<Op, T1, T2>> {};
	template<class T>
	struct Mult_Node<MultOpType::None, T> : Plus_Node<PlusOpType::None, T> {};

	template<class Ty, class T>
	struct Cast_Node : Mult_Node<MultOpType::None, Cast_Node<Ty, T>> {};

	enum class PrefixUnaryOpType {
		None,
		Plus,
		Minus,
		Increment,
		Decrement,
		LogicalNot,
		BitNot,
		AddressOf,
		Dereference,
	};

	template<PrefixUnaryOpType Op, class T>
	struct Unary_Node : Mult_Node<MultOpType::None, Unary_Node<Op, T>> {};

	template<class T>
	struct Unary_Node<PrefixUnaryOpType::None, T> : Mult_Node<MultOpType::None, T> {};

	enum class SuffixUnaryOpType {
		Increment,
		Decrement,
	};

	template<SuffixUnaryOpType Op, class T>
	struct SuffixUnary_Node : Unary_Node<PrefixUnaryOpType::None, SuffixUnary_Node<Op, T>> {};

	template<class T>
	struct Term_Node : Unary_Node<PrefixUnaryOpType::None, T> {};

	template<char... Chars>
	struct Name {};

	template<char...>
	struct IntLiteral;
	template<char C, char... Chars>
	struct IntLiteral<C, Chars...> {
		static const unsigned long long value = IntLiteral<Chars...>::value + IntLiteral<Chars...>::base * (C - '0');
		static const unsigned long long base = IntLiteral<Chars...>::base * 10;
	};
	template<>
	struct IntLiteral<> {
		static const unsigned long long value = 0;
		static const unsigned long long base  = 1;
	};

	template<char...>
	struct CharLiteral;
	template<char C, char... Chars>
	struct CharLiteral<C, Chars...> {
		static const unsigned long long value = CharLiteral<Chars...>::value + CharLiteral<Chars...>::base * C;
		static const unsigned long long  base = CharLiteral<Chars...>::base * 256;
	};
	template<>
	struct CharLiteral<> {
		static const unsigned long long value = 0;
		static const unsigned long long base  = 1;
	};

	template<char... Chars>
	struct StringLiteral {};

	// Func_Node<FuncName, Args...> require ')'; so there are no implicit conversion to TermNode<...>
	template<class FuncName, class... Args>
	struct Func_Node : detail::lr_suffix_pos<Func_Node<FuncName, Args...>> {};

	template<class ArrayName, class... Subscript>
	struct Subscript_Node : detail::lr_suffix_pos<Subscript_Node<ArrayName, Subscript...>> {};

	template<class Condition, class... Exprs>
	struct Conditional_Node : detail::lr_suffix_pos<Conditional_Node<Condition, Exprs...>> {};
	template<class Condition, class Then, class Else>
	struct Conditional_Node<Condition, Then, Else> : Cond_Node<Conditional_Node<Condition, Then, Else>> {};

	// --- Type ---

	struct type_int {};
	struct type_char {};
	struct type_void {};

	template<class Ty>
	struct DeclareSpecifier {};

	template<class DD, class Ty>
	struct Declare {};

	template<std::size_t N>
	struct Pointer {};

	template<class DD, class Ptr>
	struct DirectDeclarator {};

	template<class DD, class PL>
	struct Function {};

	template<class...>
	struct ParameterList {};

	template<class ArrayName, class Size>
	struct Array {};

	template<std::size_t...>
	struct ArraySize;
	template<>
	struct ArraySize<> : detail::lr_suffix_pos<ArraySize<>> {};
	template<std::size_t N>
	struct ArraySize<N> {};

	struct NullDeclarator {};

	namespace detail {
		template<class... T>
		struct type_stack {};

		template<class T>
		struct lr_comma_rop : lr_suffix_pos<T> {};
		template<class T>
		struct lr_assign_rop : lr_comma_rop<T> {};
		template<class T>
		struct lr_logicor_rop : lr_assign_rop<T> {};
		template<class T>
		struct lr_logicand_rop : lr_logicor_rop<T> {};
		template<class T>
		struct lr_bitor_rop : lr_logicand_rop<T> {};
		template<class T>
		struct lr_bitxor_rop : lr_bitor_rop<T> {};
		template<class T>
		struct lr_bitand_rop : lr_bitxor_rop<T> {};
		template<class T>
		struct lr_eqcomp_rop : lr_bitand_rop<T> {};
		template<class T>
		struct lr_compare_rop : lr_eqcomp_rop<T> {};
		template<class T>
		struct lr_shift_rop : lr_compare_rop<T> {};
		template<class T>
		struct lr_plus_rop : lr_shift_rop<T> {};
		template<class T>
		struct lr_mult_rop : lr_plus_rop<T> {};
		template<class T>
		struct lr_suffix_unary_rop : lr_mult_rop<T> {};

		struct lr_comma      : lr_comma_rop<lr_comma> {};
		template<AssignOpType Op>
		struct lr_assign     : lr_assign_rop<lr_assign<Op>> {};
		struct lr_logicor    : lr_logicor_rop<lr_logicor> {};
		struct lr_logicand   : lr_logicand_rop<lr_logicand> {};
		struct lr_bitor      : lr_bitor_rop<lr_bitor> {};
		struct lr_bitxor     : lr_bitxor_rop<lr_bitxor> {};
		struct lr_bitand     : lr_bitand_rop<lr_bitand> {};
		struct lr_equal      : lr_eqcomp_rop<lr_equal> {};
		struct lr_notequal   : lr_eqcomp_rop<lr_notequal> {};
		template<CompareOpType Op>
		struct lr_compare    : lr_compare_rop<lr_compare<Op>> {};
		struct lr_leftshift  : lr_shift_rop<lr_leftshift> {};
		struct lr_rightshift : lr_shift_rop<lr_rightshift> {};
		struct lr_plus       : lr_plus_rop<lr_plus> {};
		struct lr_minus      : lr_plus_rop<lr_minus> {};
		struct lr_mult       : lr_mult_rop<lr_mult> {};
		struct lr_div        : lr_mult_rop<lr_div> {};
		struct lr_mod        : lr_mult_rop<lr_mod> {};
		struct lr_open_paren : lr_suffix_pos<lr_open_paren>{};

		struct lr_unary_plus        : lr_suffix_unary_rop<lr_unary_plus> {};
		struct lr_unary_minus       : lr_suffix_unary_rop<lr_unary_minus> {};
		struct lr_unary_increment   : lr_suffix_unary_rop<lr_unary_increment> {};
		struct lr_unary_decrement   : lr_suffix_unary_rop<lr_unary_decrement> {};
		struct lr_unary_logicalnot  : lr_suffix_unary_rop<lr_unary_logicalnot> {};
		struct lr_unary_bitnot      : lr_suffix_unary_rop<lr_unary_bitnot> {};
		struct lr_unary_addressof   : lr_suffix_unary_rop<lr_unary_addressof> {};
		struct lr_unary_dereference : lr_suffix_unary_rop<lr_unary_dereference> {};

		template<class Ty>
		struct lr_cast : lr_suffix_unary_rop<lr_cast<Ty>> {};

		struct lr_condelse : lr_assign_rop<lr_condelse> {};

		template<class T>
		struct Token {};

		struct lr_if {};
		struct lr_else : lr_suffix_pos<lr_else> {};
		struct lr_while {};
		struct lr_for {};
		struct lr_break {};
		struct lr_continue {};
		struct lr_return : lr_suffix_pos<lr_return> {};

		template<class>
		struct HasName;
		template<>
		struct HasName<NullDeclarator> {};
		template<char... Chars>
		struct HasName<Name<Chars...>> { using type = std::nullptr_t; };
		template<class T, class Ptr>
		struct HasName<DirectDeclarator<T, Ptr>> : HasName<T> {};
		template<class T, class PL>
		struct HasName<Function<T, PL>> : HasName<T> {};
		template<class T, class Sz>
		struct HasName<Array<T, Sz>> : HasName<T> {};

		template<class>
		struct HasNoName;
		template<>
		struct HasNoName<NullDeclarator> { using type = std::nullptr_t; };
		template<char... Chars>
		struct HasNoName<Name<Chars...>> {};
		template<class T, class Ptr>
		struct HasNoName<DirectDeclarator<T, Ptr>> : HasNoName<T> {};
		template<class T, class PL>
		struct HasNoName<Function<T, PL>> : HasNoName<T> {};
		template<class T, class Sz>
		struct HasNoName<Array<T, Sz>> : HasNoName<T> {};

		template<class>
		struct IsFuncDeclare;
		template<char... Chars, class... Params>
		struct IsFuncDeclare<Function<DirectDeclarator<Name<Chars...>, Pointer<0>>, ParameterList<Params...>>> { using type = std::nullptr_t; };
		template<class T, class Ptr>
		struct IsFuncDeclare<DirectDeclarator<T, Ptr>> { using type = typename IsFuncDeclare<T>::type; };
		template<class T, class PL>
		struct IsFuncDeclare<Function<T, PL>> { using type = typename IsFuncDeclare<T>::type; };
		template<class T, class Sz>
		struct IsFuncDeclare<Array<T, Sz>> { using type = typename IsFuncDeclare<T>::type; };

		// LR parser rules

		// --- Reduce rules ---
		template<class Op>
		struct Reduce;

		template<AssignOpType Op>
		struct Reduce<lr_assign<Op>> {
			template<class... LRStack, class T1, class T2>
			auto operator() ( Assign_Node<AssignOpType::None, T1>, Cond_Node<T2>, LRStack... )
				-> type_stack<Assign_Node<Op, T2, T1>, LRStack...>;

			template<class... LRStack, AssignOpType Op2, class T1, class T2, class T3>
			auto operator() ( Assign_Node<Op2, T1, T2>, Cond_Node<T3>, LRStack... )
				-> type_stack<Assign_Node<Op, T3, Assign_Node<Op2, T1, T2>>, LRStack...>;
		};

		template<template<class...>class Node>
		struct BinaryReduce {
			template<class... LRStack, class T1, class T2>
			auto operator() ( Node<T1>, Node<T2>, LRStack... )
				-> type_stack<Node<T2, T1>, LRStack...>;

			template<class... LRStack, class T1, class T2, class T3>
			auto operator() ( Node<T1>, Node<T2, T3>, LRStack... )
				-> type_stack<Node<Node<T2, T3>, T1>, LRStack...>;
		};

		template<>
		struct Reduce<lr_logicor> : BinaryReduce<LogicOr_Node> {};

		template<>
		struct Reduce<lr_logicand> : BinaryReduce<LogicAnd_Node> {};

		template<>
		struct Reduce<lr_bitor> : BinaryReduce<BitOr_Node> {};

		template<>
		struct Reduce<lr_bitxor> : BinaryReduce<BitXor_Node> {};

		template<>
		struct Reduce<lr_bitand> : BinaryReduce<BitAnd_Node> {};

		template<>
		struct Reduce<lr_comma> : BinaryReduce<CommaExpr_Node> {};

		template<>
		struct Reduce<lr_condelse> {
			template<class... LRStack, class Cond, class Then, class T>
			auto operator() ( Cond_Node<T>, Conditional_Node<Cond, Then>, LRStack... )
				-> type_stack<Conditional_Node<Cond, Then, T>, LRStack...>;
		};

		template<class OpTypeEnum, template<OpTypeEnum, class...>class Node, OpTypeEnum OpType>
		struct MultiBinaryReduce {
			template<class... LRStack, class T1, class T2>
			auto operator() ( Node<OpTypeEnum::None, T1>, Node<OpTypeEnum::None, T2>, LRStack... )
				-> type_stack<Node<OpType, T2, T1>, LRStack...>;

			template<class... LRStack, class T1, OpTypeEnum Op, class T2, class T3>
			auto operator() ( Node<OpTypeEnum::None, T1>, Node<Op, T2, T3>, LRStack... )
				-> type_stack<Node<OpType, Node<Op, T2, T3>, T1>, LRStack...>;
		};

		template<>
		struct Reduce<lr_equal> : MultiBinaryReduce<EqCompOpType, EqComp_Node, EqCompOpType::Equal> {};

		template<>
		struct Reduce<lr_notequal> : MultiBinaryReduce<EqCompOpType, EqComp_Node, EqCompOpType::NotEqual> {};

		template<CompareOpType Op>
		struct Reduce<lr_compare<Op>> : MultiBinaryReduce<CompareOpType, Compare_Node, Op> {};

		template<>
		struct Reduce<lr_leftshift> : MultiBinaryReduce<ShiftOpType, Shift_Node, ShiftOpType::LeftShift> {};

		template<>
		struct Reduce<lr_rightshift> : MultiBinaryReduce<ShiftOpType, Shift_Node, ShiftOpType::RightShift> {};

		template<>
		struct Reduce<lr_plus> : MultiBinaryReduce<PlusOpType, Plus_Node, PlusOpType::Plus> {};

		template<>
		struct Reduce<lr_minus> : MultiBinaryReduce<PlusOpType, Plus_Node, PlusOpType::Minus> {};

		template<>
		struct Reduce<lr_mult> : MultiBinaryReduce<MultOpType, Mult_Node, MultOpType::Mult> {};

		template<>
		struct Reduce<lr_div> : MultiBinaryReduce<MultOpType, Mult_Node, MultOpType::Divide> {};

		template<>
		struct Reduce<lr_mod> : MultiBinaryReduce<MultOpType, Mult_Node, MultOpType::Modulo> {};

		template<PrefixUnaryOpType OpType>
		struct UnaryReduce {
			template<class... LRStack, PrefixUnaryOpType Op2, class T>
			auto operator() ( Unary_Node<Op2, T>, LRStack... )
				-> type_stack<Unary_Node<OpType, Unary_Node<Op2, T>>, LRStack...>;

			template<class... LRStack, class T>
			auto operator() ( Unary_Node<PrefixUnaryOpType::None, T>, LRStack... )
				-> type_stack<Unary_Node<OpType, T>, LRStack...>;
		};

		template<>
		struct Reduce<lr_unary_plus> : UnaryReduce<PrefixUnaryOpType::Plus> {};

		template<>
		struct Reduce<lr_unary_minus> : UnaryReduce<PrefixUnaryOpType::Minus> {};

		template<>
		struct Reduce<lr_unary_increment> : UnaryReduce<PrefixUnaryOpType::Increment> {};

		template<>
		struct Reduce<lr_unary_decrement> : UnaryReduce<PrefixUnaryOpType::Decrement> {};

		template<>
		struct Reduce<lr_unary_logicalnot> : UnaryReduce<PrefixUnaryOpType::LogicalNot> {};

		template<>
		struct Reduce<lr_unary_bitnot> : UnaryReduce<PrefixUnaryOpType::BitNot> {};

		template<>
		struct Reduce<lr_unary_addressof> : UnaryReduce<PrefixUnaryOpType::AddressOf> {};

		template<>
		struct Reduce<lr_unary_dereference> : UnaryReduce<PrefixUnaryOpType::Dereference> {};

		template<class Ty>
		struct Reduce<lr_cast<Ty>> {
			template<class... LRStack, PrefixUnaryOpType Op, class T>
			auto operator() ( Unary_Node<Op, T>, LRStack... )
				-> type_stack<Cast_Node<Ty, Unary_Node<Op, T>>, LRStack...>;

			template<class... LRStack, class T>
			auto operator() ( Unary_Node<PrefixUnaryOpType::None, T>, LRStack... )
				-> type_stack<Cast_Node<Ty, T>, LRStack...>;
		};

		// tuple expand
		template<class T, class... LRStack>
		static auto expand_move_to( type_stack<LRStack...> )
			-> decltype( T::move_to( LRStack{}... ) );

		// --- Token shift/reduce rules ---

		template<template<class>class ROP, char... OpChar>
		struct ReduceToken {
			template<class... LRStack, class T, class Op>
			static auto move_to( T node, ROP<Op>, LRStack... stack )
				-> decltype( expand_move_to<Token<lexer::Symbol<OpChar...>>>( Reduce<Op>{}( node, stack... ) ) );
		};

		struct ReduceStatement;
		// ADLによる定義前再帰用
		template<class... LRStack, class T = ReduceStatement>
		auto reduceStatement( LRStack... stack )
			-> decltype( T::reduce( stack... ) );

		struct ReduceStatement {
			// (},Statement:If:S) -> (,IfStatement:S)
			template<class... LRStack, class Cond, class T>
			static auto reduce( Statement<T>, IfStatement_Node<Cond>, LRStack... stack )
				-> decltype( reduceStatement( IfStatement_Node<Cond, T>{}, stack... ) );

			// (},Statement:Else:If:S) -> (,IfElseStatement:S)
			template<class... LRStack, class Cond, class St, class T>
			static auto reduce( Statement<T>, lr_else, Statement<St>, IfStatement_Node<Cond>, LRStack... stack )
				-> decltype( reduceStatement( IfStatement_Node<Cond, St, T>{}, stack... ) );

			// (},Statement:For:S) -> (,ForStatement:S)
			template<class... LRStack, class Init, class Cond, class After, class T>
			static auto reduce( Statement<T>, ForStatement_Node<Init, Cond, After>, LRStack... stack )
				-> decltype( reduceStatement( ForStatement_Node<Init, Cond, After, T>{}, stack... ) );

			// (},Statement:While:S) -> (,WhileStatement:S)
			template<class... LRStack, class Cond, class T>
			static auto reduce( Statement<T>, WhileStatement_Node<Cond>, LRStack... stack )
				-> decltype( reduceStatement( WhileStatement_Node<Cond, T>{}, stack... ) );

			// (},Statement:Do:S) -> (,DoStatement:S)
			template<class... LRStack, class T>
			static auto reduce( Statement<T>, DoStatement_Node<>, LRStack... )
				-> type_stack<DoStatement_Node<T>, LRStack...>;

			// (},Statement:Brace:S) -> (,BraceStatement:S)
			template<class... LRStack, class T1, class... T2>
			static auto reduce( Statement<T1>, BraceStatement_Node<T2...>, LRStack... )
				-> type_stack<BraceStatement_Node<T2..., T1>, LRStack...>;

			// (},Statement:For:S) -> (,For:S)
			template<class... LRStack, class T>
			static auto reduce( Statement<ExprStatement_Node<T>>, ForStatement_Node<>, LRStack... )
				-> type_stack<ForStatement_Node<T>, LRStack...>;
			template<class... LRStack>
			static auto reduce( Statement<NullStatement>, ForStatement_Node<>, LRStack... )
				-> type_stack<ForStatement_Node<NullExpr>, LRStack...>;
			template<class... LRStack, class T, class Init>
			static auto reduce( Statement<ExprStatement_Node<T>>, ForStatement_Node<Init>, LRStack... )
				-> type_stack<ForStatement_Node<Init, T>, LRStack...>;
			template<class... LRStack, class Init>
			static auto reduce( Statement<NullStatement>, ForStatement_Node<Init>, LRStack... )
				-> type_stack<ForStatement_Node<Init, NullExpr>, LRStack...>;

			// (},Statement:FuncDef:S) -> (,FuncDef:S)
			template<class... LRStack, class T, class DD, class Ty>
			static auto reduce( Statement<T>, FunctionDefinition<DD, Ty>, LRStack... )
				-> type_stack<FunctionDefinition<DD, Ty, T>, LRStack...>;
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Cannot_Declare_Void_Variable {
			static_assert( N != nullptr, "error: cannot declare void variable" );
		};

		template<class... LRStack, class T, std::size_t N, std::size_t M, class Ty>
		auto reduceDeclare( DirectDeclarator<T, Pointer<N>>, Pointer<M>, DeclareSpecifier<Ty>, LRStack... )
			-> type_stack<Declare<DirectDeclarator<T, Pointer<N+M>>, Ty>, LRStack...>;

		template<class... LRStack, char... Chars>
		auto reduceDeclare( DirectDeclarator<Name<Chars...>, Pointer<0>>, Pointer<0>, DeclareSpecifier<type_void>, LRStack... )
			-> Error_Cannot_Declare_Void_Variable<>;

		template<class... LRStack, std::size_t M, class Ty>
		auto reduceDeclare( Pointer<M>, DeclareSpecifier<Ty>, LRStack... )
			-> type_stack<Declare<DirectDeclarator<NullDeclarator, Pointer<M>>, Ty>, LRStack...>;

		template<class T, class... lrElem>
		struct SuffixPosReduceStatement {
			// (T,S) -> (,lrElem:S)
			template<class... LRStack, class U>
			static auto move_to( lr_suffix_pos<U>, LRStack... )
				-> type_stack<lrElem..., U, LRStack...>;

			// suffix_pos: reduceStatement
			template<class... LRStack, class U>
			static auto move_to( Statement<U> st, LRStack... stack )
				-> decltype( expand_move_to<T>( reduceStatement( st, stack... ) ) );
		};


		template<char... Chars>
		struct Token<lexer::IdentifierToken<Chars...>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<Chars...>>, Term_Node<Name<Chars...>>> {
			// (ID,S) -> (,Term:S)
			using SuffixPosReduceStatement<Token<lexer::IdentifierToken<Chars...>>, Term_Node<Name<Chars...>>>::move_to;

			// type
			template<class... LRStack, std::size_t N>
			static auto move_to( Pointer<N>, LRStack... )
				-> type_stack<DirectDeclarator<Name<Chars...>, Pointer<0>>, Pointer<N>, LRStack...>;
		};

		template<char... Chars>
		struct Token<lexer::CharLiteral<Chars...>> : SuffixPosReduceStatement<Token<lexer::CharLiteral<Chars...>>, Term_Node<CharLiteral<Chars...>>> {};

		template<char... Chars>
		struct Token<lexer::IntLiteralToken<Chars...>> : SuffixPosReduceStatement<Token<lexer::IntLiteralToken<Chars...>>, Term_Node<IntLiteral<Chars...>>> {};

		template<char... Chars>
		struct Token<lexer::StringLiteral<Chars...>> : SuffixPosReduceStatement<Token<lexer::StringLiteral<Chars...>>, Term_Node<StringLiteral<Chars...>>> {};

		template<>
		struct Token<lexer::IdentifierToken<'i','f'>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<'i','f'>>, lr_if> {};

		template<>
		struct Token<lexer::IdentifierToken<'e','l','s','e'>> {
			template<class... LRStack, class T, class Cond>
			static auto move_to( Statement<T>, IfStatement_Node<Cond>, LRStack... )
				-> type_stack<lr_else, Statement<T>, IfStatement_Node<Cond>, LRStack...>;
		};

		template<>
		struct Token<lexer::IdentifierToken<'f','o','r'>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<'f','o','r'>>, lr_for> {};

		template<>
		struct Token<lexer::IdentifierToken<'d','o'>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<'d','o'>>, DoStatement_Node<>> {};

		template<>
		struct Token<lexer::IdentifierToken<'w','h','i','l','e'>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<'w','h','i','l','e'>>, lr_while> {
			// do-while
			template<class... LRStack, class T>
			static auto move_to( DoStatement_Node<T>, LRStack... )
				-> type_stack<lr_while, DoStatement_Node<T>, LRStack...>;

			using SuffixPosReduceStatement<Token<lexer::IdentifierToken<'w','h','i','l','e'>>, lr_while>::move_to;
		};

		template<>
		struct Token<lexer::IdentifierToken<'b','r','e','a','k'>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<'b','r','e','a','k'>>, lr_break> {};

		template<>
		struct Token<lexer::IdentifierToken<'c','o','n','t','i','n','u','e'>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<'c','o','n','t','i','n','u','e'>>, lr_continue> {};

		template<>
		struct Token<lexer::IdentifierToken<'r','e','t','u','r','n'>> : SuffixPosReduceStatement<Token<lexer::IdentifierToken<'r','e','t','u','r','n'>>, lr_return> {};

		template<class Ty, char... TyName>
		struct ReduceType : SuffixPosReduceStatement<Token<lexer::IdentifierToken<TyName...>>, Pointer<0>, DeclareSpecifier<Ty>, DeclareStatement_Node<>> {
			// declare specifier(type)
			using SuffixPosReduceStatement<Token<lexer::IdentifierToken<TyName...>>, Pointer<0>, DeclareSpecifier<Ty>, DeclareStatement_Node<>>::move_to;

			// declare specifier(type) in paramList
			template<class... LRStack>
			static auto move_to( Pointer<0>, lr_open_paren, LRStack... )
				-> type_stack<Pointer<0>, DeclareSpecifier<Ty>, ParameterList<>, LRStack...>;

			template<class... LRStack, class... T>
			static auto move_to( ParameterList<T...>, LRStack... )
				-> type_stack<Pointer<0>, DeclareSpecifier<Ty>, ParameterList<T...>, LRStack...>;

			// cast
			template<class... LRStack>
			static auto move_to( lr_open_paren, LRStack... )
				-> type_stack<Pointer<0>, DeclareSpecifier<Ty>, lr_open_paren, LRStack...>;
		};

		template<>
		struct Token<lexer::IdentifierToken<'i','n','t'>> : ReduceType<type_int, 'i','n','t'> {};

		template<>
		struct Token<lexer::IdentifierToken<'c','h','a','r'>> : ReduceType<type_char, 'c','h','a','r'> {};

		template<>
		struct Token<lexer::IdentifierToken<'v','o','i','d'>> : ReduceType<type_void, 'v','o','i','d'> {};

		template<>
		struct Token<lexer::Symbol<'('>> : SuffixPosReduceStatement<Token<lexer::Symbol<'('>>, lr_open_paren> {
			// ((,ID:S) -> (,Func<ID>:S)  // f(
			template<class... LRStack, class T>
			static auto move_to( Term_Node<T>, LRStack... )
				-> type_stack<Func_Node<Term_Node<T>>, LRStack...>;

			// ((,op:S) -> (,(:op:S)  // +(
			using SuffixPosReduceStatement<Token<lexer::Symbol<'('>>, lr_open_paren>::move_to;

			// ((,if:S) -> (,IfStatement:S)
			template<class... LRStack>
			static auto move_to( lr_if, LRStack... )
				-> type_stack<IfStatement_Node<>, LRStack...>;
			template<class... LRStack>
			static auto move_to( lr_for, LRStack... )
				-> type_stack<ForStatement_Node<>, LRStack...>;
			template<class... LRStack>
			static auto move_to( lr_while, LRStack... )
				-> type_stack<WhileStatement_Node<>, LRStack...>;

			// type (func or paren)
			template<class... LRStack, std::size_t N>
			static auto move_to( Pointer<N>, LRStack... )
				-> type_stack<Pointer<0>, lr_open_paren, Pointer<N>, LRStack...>;
			template<class... LRStack, class T, std::size_t N>
			static auto move_to( DirectDeclarator<T, Pointer<N>>, LRStack... )
				-> type_stack<Pointer<0>, lr_open_paren, DirectDeclarator<T, Pointer<N>>, LRStack...>;
		};


		template<>
		struct Token<lexer::Symbol<')'>> : ReduceToken<lr_comma_rop, ')'> {
			// (),Func<f>:S) -> (,Term<Func<f>>:S)
			template<class... LRStack, class T, class U>
			static auto move_to( Func_Node<T>, lr_element<U>, LRStack... )
				-> type_stack<Term_Node<Func_Node<T>>, U, LRStack...>;

			// (),Expr:Func<f>:S) -> (,Term<Func<f,Expr>>:S)
			template<class... LRStack, class T1, class... T2>
			static auto move_to( Expr<T1>, Func_Node<T2...>, LRStack... )
				-> type_stack<Term_Node<Func_Node<T2..., T1>>, LRStack...>;

			// (),Expr:(:S) -> (,Term<Expr>:S)
			template<class... LRStack, class T>
			static auto move_to( Expr<T>, lr_open_paren, LRStack... )
				-> type_stack<Term_Node<T>, LRStack...>;

			template<class... LRStack, class T>
			static auto move_to( Expr<T>, IfStatement_Node<>, LRStack... )
				-> type_stack<IfStatement_Node<T>, LRStack...>;
			template<class... LRStack, class T, class Init, class Cond>
			static auto move_to( Expr<T>, ForStatement_Node<Init, Cond>, LRStack... )
				-> type_stack<ForStatement_Node<Init, Cond, T>, LRStack...>;
			template<class... LRStack, class Init, class Cond>
			static auto move_to( ForStatement_Node<Init, Cond>, LRStack... )
				-> type_stack<ForStatement_Node<Init, Cond, NullExpr>, LRStack...>;
			template<class... LRStack, class T>
			static auto move_to( Expr<T>, WhileStatement_Node<>, LRStack... )
				-> type_stack<WhileStatement_Node<T>, LRStack...>;

			using ReduceToken<lr_comma_rop, ')'>::move_to;

			// type

			// reduce->DirectDeclarator(paren)
			template<class... LRStack, class T, std::size_t N, std::size_t M>
			static auto move_to( DirectDeclarator<T, Pointer<N>>, Pointer<M>, lr_open_paren, LRStack... )
				-> type_stack<DirectDeclarator<T, Pointer<N+M>>, LRStack...>;
			template<class... LRStack, std::size_t M>
			static auto move_to( Pointer<M>, lr_open_paren, LRStack... )
				-> type_stack<DirectDeclarator<NullDeclarator, Pointer<M>>, LRStack...>;

			// reduce->DirectDeclarator(func)
			// func()
			template<class... LRStack, class T, std::size_t N, std::size_t M>
			static auto move_to( Pointer<0>, lr_open_paren, DirectDeclarator<T, Pointer<N>>, Pointer<M>, LRStack... )
				-> type_stack<DirectDeclarator<Function<DirectDeclarator<T, Pointer<N>>, ParameterList<>>, Pointer<0>>, Pointer<M>, LRStack...>;
			template<class... LRStack, std::size_t M>
			static auto move_to( Pointer<0>, lr_open_paren, Pointer<M>, LRStack... )
				-> type_stack<DirectDeclarator<Function<NullDeclarator, ParameterList<>>, Pointer<0>>, Pointer<M>, LRStack...>;

			// func(Ty...)
			template<class... LRStack, class T, class Ty, class... PL, class T2, std::size_t N, std::size_t M>
			static auto move_to( Declare<T, Ty>, ParameterList<PL...>, DirectDeclarator<T2, Pointer<N>>, Pointer<M>, LRStack... )
				-> type_stack<DirectDeclarator<Function<DirectDeclarator<T2, Pointer<N>>, ParameterList<PL..., Declare<T, Ty>>>, Pointer<0>>, Pointer<M>, LRStack...>;

			// cast
			template<class... LRStack, class T, class Ty, typename HasNoName<T>::type = nullptr>
			static auto move_to( Declare<T, Ty>, lr_open_paren, LRStack... )
				-> type_stack<lr_cast<Declare<T, Ty>>, LRStack...>;

			template<class... LRStack>
			static auto move_to( LRStack... stack )
				-> decltype( expand_move_to<Token<lexer::Symbol<')'>>>( reduceDeclare( stack... ) ) );
		};


		template<>
		struct Token<lexer::Symbol<'['>> {
			// ([,ID:S) -> (,Subscript<ID>:S)  // a[
			template<class... LRStack, class T>
			static auto move_to( Term_Node<T>, LRStack... )
				-> type_stack<Subscript_Node<Term_Node<T>>, LRStack...>;

			// type (array)
			template<class... LRStack, class T, std::size_t N, std::size_t M>
			static auto move_to( DirectDeclarator<T, Pointer<N>>, Pointer<M>, LRStack... )
				-> type_stack<ArraySize<>, DirectDeclarator<T, Pointer<N>>, Pointer<M>, LRStack...>;
			template<class... LRStack, class T, std::size_t M>
			static auto move_to( Pointer<M>, LRStack... )
				-> type_stack<ArraySize<>, NullDeclarator, Pointer<M>, LRStack...>;
		};

		template<>
		struct Token<lexer::Symbol<']'>> : ReduceToken<lr_comma_rop, ']'> {
			// (],Expr:Subscript<a>:S) -> (,Term<Subscript<a,Expr>>:S)
			template<class... LRStack, class T1, class T2>
			static auto move_to( Expr<T1>, Subscript_Node<T2>, LRStack... )
				-> type_stack<Term_Node<Subscript_Node<T2, T1>>, LRStack...>;

			using ReduceToken<lr_comma_rop, ']'>::move_to;

			// Error
			template<class T = void>
			struct Error_multidimensional_array_must_have_size {
				static_assert( !std::is_same<T, void>::value, "parser error: multi dimensional array must have size for all dimensions except the first" );
			};

			// type
			// 本来constexpr
			template<class... LRStack, char... Chars, class T2, std::size_t N>
			static auto move_to( Term_Node<IntLiteral<Chars...>>, ArraySize<>, DirectDeclarator<T2, Pointer<N>>, LRStack... )
				-> type_stack<DirectDeclarator<Array<DirectDeclarator<T2, Pointer<N>>, ArraySize<IntLiteral<Chars...>::value>>, Pointer<0>>, LRStack...>;
			template<class... LRStack, class T2, std::size_t N>
			static auto move_to( ArraySize<>, DirectDeclarator<T2, Pointer<N>>, LRStack... )
				-> type_stack<DirectDeclarator<Array<DirectDeclarator<T2, Pointer<N>>, ArraySize<>>, Pointer<0>>, LRStack...>;
			template<class... LRStack, class T2, std::size_t N>
			static auto move_to( ArraySize<>, DirectDeclarator<Array<T2, ArraySize<>>, Pointer<N>>, LRStack... )
				-> Error_multidimensional_array_must_have_size<>;
		};

		template<>
		struct Token<lexer::Symbol<'{'>> : SuffixPosReduceStatement<Token<lexer::Symbol<'{'>>, BraceStatement_Node<>> {
			// ({,S) -> (,Brace:S)
			using SuffixPosReduceStatement<Token<lexer::Symbol<'{'>>, BraceStatement_Node<>>::move_to;

			// Error
			template<class T = void>
			struct Error_expected_semicolon_after_do_while_statement {
				static_assert( !std::is_same<T, void>::value, "parser error: expected ';' after do-while statement" ); 
			};
			template<class... LRStack, class Cond, class St>
			static auto move_to( WhileStatement_Node<Cond>, DoStatement_Node<St>, LRStack... )
				-> Error_expected_semicolon_after_do_while_statement<>;

			// ({,fun:S) -> (,FuncDef:S)
			template<class... LRStack, class T, class Ty, class... St, typename IsFuncDeclare<T>::type = nullptr>
			static auto move_to( Declare<T, Ty>, DeclareStatement_Node<>, LRStack... )
				-> type_stack<BraceStatement_Node<>, FunctionDefinition<T, Ty>, LRStack...>;

			template<class... LRStack>
			static auto move_to( LRStack... stack )
				-> decltype( expand_move_to<Token<lexer::Symbol<'{'>>>( reduceDeclare( stack... ) ) );
		};

		template<>
		struct Token<lexer::Symbol<'}'>> {
			// (},Brace:S) -> (,BraceStatement:S)
			template<class... LRStack, class... T>
			static auto move_to( BraceStatement_Node<T...>, LRStack... )
				-> type_stack<Statement_Node<BraceStatement_Node<T...>>, LRStack...>;

			// reduceStatement
			template<class... LRStack>
			static auto move_to( LRStack... stack )
				-> decltype( expand_move_to<Token<lexer::Symbol<'}'>>>( reduceStatement( stack... ) ) );
		};

		template<>
		struct Token<lexer::Symbol<'?'>> : ReduceToken<lr_logicor_rop, '?'> {
			// (?,CondExpr:S) -> (,Cond<CondExpr>:S)
			template<class... LRStack, class T, class U>
			static auto move_to( LogicOr_Node<T>, lr_element<U>, LRStack... )
				-> type_stack<Conditional_Node<T>, U, LRStack...>;
			template<class... LRStack, class T1, class T2, class U>
			static auto move_to( LogicOr_Node<T1, T2>, lr_element<U>, LRStack... )
				-> type_stack<Conditional_Node<LogicOr_Node<T1, T2>>, U, LRStack...>;

			using ReduceToken<lr_logicor_rop, '?'>::move_to;
		};

		template<>
		struct Token<lexer::Symbol<':'>> : ReduceToken<lr_comma_rop, ':'> {
			// (:,CommaExpr:Cond<CondExpr>:S) -> (Cond<CondExpr,CommaExpr>:S)
			template<class... LRStack, class T, class Cond>
			static auto move_to( Expr<T>, Conditional_Node<Cond>, LRStack... )
				-> type_stack<lr_condelse, Conditional_Node<Cond, T>, LRStack...>;

			using ReduceToken<lr_comma_rop, ':'>::move_to;
		};

		template<AssignOpType Op, char... OpChar>
		struct AssignTokenRule : ReduceToken<lr_assign_rop, OpChar...> {
			// (=,Cond:S) -> (,=:Cond:S)
			template<class... LRStack, class T, class U>
			static auto move_to( Cond_Node<T>, lr_element<U>, LRStack... )
				-> type_stack<lr_assign<Op>, Cond_Node<T>, U, LRStack...>;

			using ReduceToken<lr_assign_rop, OpChar...>::move_to;
		};

		template<>
		struct Token<lexer::Symbol<'='>> : AssignTokenRule<AssignOpType::NormalAssign, '='> {};

		template<>
		struct Token<lexer::Symbol<'|','='>> : AssignTokenRule<AssignOpType::OrAssign, '|','='> {};

		template<>
		struct Token<lexer::Symbol<'^','='>> : AssignTokenRule<AssignOpType::XorAssign, '^','='> {};

		template<>
		struct Token<lexer::Symbol<'&','='>> : AssignTokenRule<AssignOpType::AndAssign, '&','='> {};

		template<>
		struct Token<lexer::Symbol<'<','<','='>> : AssignTokenRule<AssignOpType::LeftShiftAssign, '<','<','='> {};

		template<>
		struct Token<lexer::Symbol<'>','>','='>> : AssignTokenRule<AssignOpType::RightShiftAssign, '>','>','='> {};

		template<>
		struct Token<lexer::Symbol<'+','='>> : AssignTokenRule<AssignOpType::PlusAssign, '+','='> {};

		template<>
		struct Token<lexer::Symbol<'-','='>> : AssignTokenRule<AssignOpType::MinusAssign, '-','='> {};

		template<>
		struct Token<lexer::Symbol<'*','='>> : AssignTokenRule<AssignOpType::MulAssign, '*','='> {};

		template<>
		struct Token<lexer::Symbol<'/','='>> : AssignTokenRule<AssignOpType::DivAssign, '/','='> {};

		template<>
		struct Token<lexer::Symbol<'%','='>> : AssignTokenRule<AssignOpType::ModAssign, '%','='> {};

		template<class BinaryElem, template<class...>class Node, template<class>class ROP, char... OpChar>
		struct BinaryTokenRule : ReduceToken<ROP, OpChar...> {
			// (&,BitAnd:S) -> (,&:BitAnd:S)
			template<class... LRStack, class... T, class U>
			static auto move_to( Node<T...>, lr_element<U>, LRStack... )
				-> type_stack<BinaryElem, Node<T...>, U, LRStack...>;

			using ReduceToken<ROP, OpChar...>::move_to;
		};

		template<class BinaryElem, class BinaryOpTypeEnum, template<BinaryOpTypeEnum, class...>class Node, template<class>class ROP, char... OpChar>
		struct MultiBinaryTokenRule : ReduceToken<ROP, OpChar...> {
			// (+,Plus:S) -> (,+:Plus:S)
			template<class... LRStack, BinaryOpTypeEnum Op, class... T, class U>
			static auto move_to( Node<Op, T...>, lr_element<U>, LRStack... )
				-> type_stack<BinaryElem, Node<Op, T...>, U, LRStack...>;

			using ReduceToken<ROP, OpChar...>::move_to;
		};

		template<class UnaryElem, char... OpChar>
		struct UnaryTokenRule : SuffixPosReduceStatement<Token<lexer::Symbol<OpChar...>>, UnaryElem> {};

		template<class UnaryElem, class BinaryElem, class BinaryOpTypeEnum, template<BinaryOpTypeEnum, class...>class Node, template<class>class ROP, char... OpChar>
		struct UnaryMultiBinaryTokenRule : UnaryTokenRule<UnaryElem, OpChar...>, MultiBinaryTokenRule<BinaryElem, BinaryOpTypeEnum, Node, ROP, OpChar...> {
			using UnaryTokenRule<UnaryElem, OpChar...>::move_to;
			using MultiBinaryTokenRule<BinaryElem, BinaryOpTypeEnum, Node, ROP, OpChar...>::move_to;
		};

		template<>
		struct Token<lexer::Symbol<'|','|'>> : BinaryTokenRule<lr_logicor, LogicOr_Node, lr_logicor_rop, '|','|'> {};

		template<>
		struct Token<lexer::Symbol<'&','&'>> : BinaryTokenRule<lr_logicand, LogicAnd_Node, lr_logicand_rop, '&','&'> {};

		template<>
		struct Token<lexer::Symbol<'|'>> : BinaryTokenRule<lr_bitor, BitOr_Node, lr_bitor_rop, '|'> {};

		template<>
		struct Token<lexer::Symbol<'^'>> : BinaryTokenRule<lr_bitxor, BitXor_Node, lr_bitxor_rop, '^'> {};

		template<>
		struct Token<lexer::Symbol<'&'>> : BinaryTokenRule<lr_bitand, BitAnd_Node, lr_bitand_rop, '&'>, UnaryTokenRule<lr_unary_addressof, '&'> {
			using BinaryTokenRule<lr_bitand, BitAnd_Node, lr_bitand_rop, '&'>::move_to;
			using UnaryTokenRule<lr_unary_addressof, '&'>::move_to;
		};

		template<>
		struct Token<lexer::Symbol<'=','='>> : MultiBinaryTokenRule<lr_equal, EqCompOpType, EqComp_Node, lr_eqcomp_rop, '=','='> {};

		template<>
		struct Token<lexer::Symbol<'!','='>> : MultiBinaryTokenRule<lr_notequal, EqCompOpType, EqComp_Node, lr_eqcomp_rop, '!','='> {};

		template<>
		struct Token<lexer::Symbol<'<'>> : MultiBinaryTokenRule<lr_compare<CompareOpType::LessThan>, CompareOpType, Compare_Node, lr_compare_rop, '<'> {};

		template<>
		struct Token<lexer::Symbol<'<','='>> : MultiBinaryTokenRule<lr_compare<CompareOpType::EqualOrLessThan>, CompareOpType, Compare_Node, lr_compare_rop, '<','='> {};

		template<>
		struct Token<lexer::Symbol<'>'>> : MultiBinaryTokenRule<lr_compare<CompareOpType::GraterThan>, CompareOpType, Compare_Node, lr_compare_rop, '>'> {};

		template<>
		struct Token<lexer::Symbol<'>','='>> : MultiBinaryTokenRule<lr_compare<CompareOpType::EqualOrGraterThan>, CompareOpType, Compare_Node, lr_compare_rop, '>','='> {};

		template<>
		struct Token<lexer::Symbol<'>','>'>> : MultiBinaryTokenRule<lr_leftshift, ShiftOpType, Shift_Node, lr_shift_rop, '>','>'> {};

		template<>
		struct Token<lexer::Symbol<'<','<'>> : MultiBinaryTokenRule<lr_rightshift, ShiftOpType, Shift_Node, lr_shift_rop, '<','<'> {};

		template<>
		struct Token<lexer::Symbol<'+'>> : UnaryMultiBinaryTokenRule<lr_unary_plus, lr_plus, PlusOpType, Plus_Node, lr_plus_rop, '+'> {};

		template<>
		struct Token<lexer::Symbol<'-'>> : UnaryMultiBinaryTokenRule<lr_unary_minus, lr_minus, PlusOpType, Plus_Node, lr_plus_rop, '-'> {};

		template<>
		struct Token<lexer::Symbol<'*'>> : UnaryMultiBinaryTokenRule<lr_unary_dereference, lr_mult, MultOpType, Mult_Node, lr_mult_rop, '*'> {
			// type: pointer
			template<class... LRStack, std::size_t N>
			static auto move_to( Pointer<N>, LRStack... )
				-> type_stack<Pointer<N+1>, LRStack...>;

			using UnaryMultiBinaryTokenRule<lr_unary_dereference, lr_mult, MultOpType, Mult_Node, lr_mult_rop, '*'>::move_to;
		};

		template<>
		struct Token<lexer::Symbol<'/'>> : MultiBinaryTokenRule<lr_div, MultOpType, Mult_Node, lr_mult_rop, '/'> {};

		template<>
		struct Token<lexer::Symbol<'%'>> : MultiBinaryTokenRule<lr_mod, MultOpType, Mult_Node, lr_mult_rop, '%'> {};

		template<>
		struct Token<lexer::Symbol<'+','+'>> : UnaryTokenRule<lr_unary_increment, '+','+'> {
			// (++,ID:S) -> (,Incr<ID>:S)
			template<class... LRStack, class T>
			static auto move_to( Term_Node<T>, LRStack... )
				-> type_stack<Term_Node<SuffixUnary_Node<SuffixUnaryOpType::Increment, T>>, LRStack...>;

			using UnaryTokenRule<lr_unary_increment, '+','+'>::move_to;
		};

		template<>
		struct Token<lexer::Symbol<'-','-'>> : UnaryTokenRule<lr_unary_decrement, '-','-'> {
			// (++,ID:S) -> (,Decr<ID>:S)
			template<class... LRStack, class T>
			static auto move_to( Term_Node<T>, LRStack... )
				-> type_stack<Term_Node<SuffixUnary_Node<SuffixUnaryOpType::Decrement, T>>, LRStack...>;

			using UnaryTokenRule<lr_unary_decrement, '-','-'>::move_to;
		};

		template<>
		struct Token<lexer::Symbol<'!'>> : UnaryTokenRule<lr_unary_logicalnot, '!'> {};

		template<>
		struct Token<lexer::Symbol<'~'>> : UnaryTokenRule<lr_unary_bitnot, '~'> {};

		template<>
		struct Token<lexer::Symbol<','>> : ReduceToken<lr_comma_rop, ','> {
			// (,,Expr:Func<f>:S) -> (,Func<f,Expr>:S)
			template<class... LRStack, class T1, class... T2>
			static auto move_to( Assign_Node<AssignOpType::None, T1>, Func_Node<T2...>, LRStack... )
				-> type_stack<Func_Node<T2..., T1>, LRStack...>;
			template<class... LRStack, AssignOpType Op, class T1, class T2, class... T3>
			static auto move_to( Assign_Node<Op, T1, T2>, Func_Node<T3...>, LRStack... )
				-> type_stack<Func_Node<T3..., Assign_Node<Op, T1, T2>>, LRStack...>;

			// (,,Expr:S) -> (,,:Expr:S)
			template<class... LRStack, class... T, class U>
			static auto move_to( CommaExpr_Node<T...>, lr_element<U>, LRStack... )
				-> type_stack<lr_comma, CommaExpr_Node<T...>, U, LRStack...>;

			using ReduceToken<lr_comma_rop, ','>::move_to;

			// type
			template<class... LRStack, class T, class Ty, class... PL>
			static auto move_to( Declare<T, Ty>, ParameterList<PL...>, LRStack... )
				-> type_stack<ParameterList<PL..., Declare<T, Ty>>, LRStack...>;

			template<class... LRStack, class T, class Ty, class... St, typename HasName<T>::type = nullptr>
			static auto move_to( Declare<T, Ty>, DeclareStatement_Node<St...>, LRStack... )
				-> type_stack<Pointer<0>, DeclareSpecifier<Ty>, DeclareStatement_Node<St..., Declare<T, Ty>>, LRStack...>;

			template<class... LRStack>
			static auto move_to( LRStack... stack )
				-> decltype( expand_move_to<Token<lexer::Symbol<','>>>( reduceDeclare( stack... ) ) );
		};

		template<>
		struct Token<lexer::Symbol<';'>> : ReduceToken<lr_comma_rop, ';'>, SuffixPosReduceStatement<Token<lexer::Symbol<';'>>, NullStatement> {
			// only semicolon
			using SuffixPosReduceStatement<Token<lexer::Symbol<';'>>, NullStatement>::move_to;

			// break;
			template<class... LRStack>
			static auto move_to( lr_break, LRStack... )
				-> type_stack<BreakStatement, LRStack...>;

			// continue;
			template<class... LRStack>
			static auto move_to( lr_continue, LRStack... )
				-> type_stack<ContinueStatement, LRStack...>;

			// return Expr;
			template<class... LRStack, class T>
			static auto move_to( Expr<T>, lr_return, LRStack... )
				-> type_stack<ReturnStatement<T>, LRStack...>;

			// return;
			template<class... LRStack>
			static auto move_to( lr_return, LRStack... )
				-> type_stack<ReturnVoidStatement, LRStack...>;

			// (;,CommaExpr:S) -> (,ExprStatement:S)
			template<class... LRStack, class T, class U>
			static auto move_to( Expr<T>, lr_element<U>, LRStack... )
				-> type_stack<ExprStatement_Node<T>, U, LRStack...>;

			// only semicolon after do-While
			template<class... LRStack, class Cond, class St>
			static auto move_to( WhileStatement_Node<Cond>, DoStatement_Node<St>, LRStack... stack )
				-> type_stack<DoStatement_Node<St, Cond>, LRStack...>;

			using ReduceToken<lr_comma_rop, ';'>::move_to;

			// type
			template<class... LRStack, class T, class Ty, class... St, typename HasName<T>::type = nullptr>
			static auto move_to( Declare<T, Ty>, DeclareStatement_Node<St...>, LRStack... )
				-> type_stack<Statement_Node<DeclareStatement_Node<St..., Declare<T, Ty>>>, LRStack...>;

			template<class... LRStack>
			static auto move_to( LRStack... stack )
				-> decltype( expand_move_to<Token<lexer::Symbol<';'>>>( reduceDeclare( stack... ) ) );

			// meaningless 'int;'
			template<class... LRStack, class Ty>
			static auto move_to( Pointer<0>, DeclareSpecifier<Ty>, DeclareStatement_Node<>, LRStack... )
				-> type_stack<LRStack...>;
		};


		// ADL(parseの定義前自己再帰)

		template<class T, class... LRStack>
		auto parse( lfl::type_tuple<T>, type_stack<LRStack...> )
			-> decltype( Token<T>::move_to( LRStack{}... ) );

		template<class T1, class T2, class... Tokens, class... LRStack>
		auto parse( lfl::type_tuple<T1, T2, Tokens...> tokens, type_stack<LRStack...> stack )
			-> decltype( parse( typename decltype(tokens)::tail{}, parse( typename decltype(tokens)::head{}, stack ) ) );

		template<class... LRStack, class T>
		auto finalize_impl( Statement<T>, LRStack... )
			-> decltype( reduceStatement( Statement<T>{}, LRStack{}... ) );
		template<class... LRStack, class U>
		auto finalize_impl( lr_element<U>, LRStack... )
			-> type_stack<U, LRStack...>;

		template<class... LRStack>
		auto finalize( type_stack<LRStack...> )
			-> decltype( finalize_impl( LRStack{}... ) );

		template<class TokenTuple>
		using Result = decltype( finalize( parse( TokenTuple{}, type_stack<lr_stack_bottom>{} ) ) );

	} // namespace detail

	template<class TokenTuple>
	using Result = parser::detail::Result<TokenTuple>;

} // namespace parser

#endif // LTMPC_PARSER_HPP

