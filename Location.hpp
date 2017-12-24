#ifndef LTMPC_LOCATION_HPP
#define LTMPC_LOCATION_HPP
#include "index_tuple.hpp"
#include "TypeAnalysis.hpp"

namespace location {

	template<std::ptrdiff_t>
	struct FramePosition {};

	template<class Name, std::size_t Size, std::size_t Align>
	struct GlobalVariable {};

	template<class Name, class St>
	struct Function {};

	template<char...>
	struct String {};

	template<class...>
	struct StringPool {};

	template<class, class...>
	struct GlobalEnvironment {};

	template<class>
	struct SetSP_Node {};

	struct Return_Node {};

	template<std::size_t>
	struct StringRef {};

	namespace detail {

		template<class...>
		struct IdentifierList {};

		template<class...>
		struct PositionList {};

		template<class, class>
		struct indexed_type {};

		template<class, class, class, class>
		struct FrameEnvironment {};
		template<class Uw, class Top, class... Ids, class... Adjs>
		struct FrameEnvironment<Uw, Top, IdentifierList<Ids...>, PositionList<Adjs...>> : indexed_type<Ids, Adjs>... {
			using top = Top;
			using unwind = Uw;
		};

		template<class DD, class... Ts, std::size_t N, lfl::index_t... Index>
		auto call_environment_impl( parser::DirectDeclarator<parser::Function<DD, parser::ParameterList<Ts...>>, parser::Pointer<N>>, lfl::index_tuple<Index...> )
			-> FrameEnvironment<FramePosition<0>, FramePosition<0>, IdentifierList<typename type_analysis::detail::ParseDeclare<Ts>::name...>, PositionList<FramePosition<4*(static_cast<std::ptrdiff_t>(Index)+2)>...>>;

		template<class DD, class... Ts, std::size_t N>
		auto call_environment( parser::DirectDeclarator<parser::Function<DD, parser::ParameterList<Ts...>>, parser::Pointer<N>> decl )
			-> decltype( call_environment_impl( decl, lfl::make_index_tuple<sizeof...(Ts)>{} ) );

		// Auto variable
		template<class Id, class Adj>
		auto getFramePosition( indexed_type<Id, Adj> )
			-> Adj;

		// Global variable/function
		template<class Id>
		auto getFramePosition( ... )
			-> Id;

		// --- type_tuple ---

		template<class... AST>
		struct type_tuple {};

		template<class... AST1, class... AST2>
		auto merge( type_tuple<AST1...>, type_tuple<AST2...> )
			-> type_tuple<AST1..., AST2...>;

		template<class AST1, class... AST2>
		auto merge( AST1, type_tuple<AST2...> )
			-> type_tuple<AST1, AST2...>;

		template<class... AST1, class AST2>
		auto merge( type_tuple<AST1...>, AST2 )
			-> type_tuple<AST1..., AST2>;

		template<class AST1, class AST2>
		auto merge( AST1, AST2 )
			-> type_tuple<AST1, AST2>;

		// --- parse_size ---

		template<std::size_t>
		struct Size {};

		template<std::size_t>
		struct Align {};

		template<std::size_t N, std::size_t M>
		auto array_size( Size<M> )
			-> Size<N*M>;

		template<class T>
		struct ParseSize;
		template<>
		struct ParseSize<type_analysis::type_char> { using size = Size<1>; using align = Align<1>; };
		template<>
		struct ParseSize<type_analysis::type_int> { using size = Size<4>; using align = Align<4>; };
		template<class T>
		struct ParseSize<type_analysis::PtrTo<T>> { using size = Size<4>; using align = Align<4>; };
		template<class T, std::size_t N>
		struct ParseSize<type_analysis::ArrayOf<T, N>> { using size = decltype( array_size<N>( typename ParseSize<T>::size{} ) ); using align = typename ParseSize<T>::align; };
		template<class T, class... Tys>
		struct ParseSize<type_analysis::Function<T, Tys...>> { using size = Size<0>; using align = Align<0>; };

		// --- add_global_environment ---

		template<class... Ts, class Name, std::size_t N, std::size_t M>
		auto add_global_environment_impl( GlobalEnvironment<Ts...>, Name, Size<N>, Align<M> )
			-> GlobalEnvironment<Ts..., GlobalVariable<Name, N, M>>;

		// function declaration (not definition)
		template<class... Ts, class Name>
		auto add_global_environment_impl( GlobalEnvironment<Ts...>, Name, Size<0>, Align<0> )
			-> GlobalEnvironment<Ts...>;

		template<class Environment, class T>
		auto add_global_environment( Environment env, T )
			-> decltype( add_global_environment_impl( env, typename T::name{}, typename ParseSize<typename T::type>::size{}, typename ParseSize<typename T::type>::align{} ) );

		template<class... Ts, class Name, class Statement>
		auto add_global_environment_function( GlobalEnvironment<Ts...>, Name, Statement )
			-> GlobalEnvironment<Ts..., Function<Name, Statement>>;

		// --- add_frame_environment ---

		template<std::ptrdiff_t N, std::size_t M>
		auto stack_alloc( FramePosition<N>, Size<M> )
			-> FramePosition<N-static_cast<std::ptrdiff_t>((M+3)/4*4)>;

		template<class Uw, class Top, class... Ids, class... Adjs, class Name, std::size_t M>
		auto add_frame_environment_impl( FrameEnvironment<Uw, Top, IdentifierList<Ids...>, PositionList<Adjs...>>, Name, Size<M> size )
			-> FrameEnvironment<Uw, decltype( stack_alloc( Top{}, size ) ), IdentifierList<Ids..., Name>, PositionList<Adjs..., decltype( stack_alloc( Top{}, size ) )>>;

		template<class Environment, class T>
		auto add_frame_environment( Environment env, T )
			-> decltype( add_frame_environment_impl( env, typename T::name{}, typename ParseSize<typename T::type>::size{} ) );

		// ----- Locate ------

		template<class T, class NowStringPool, class NowFrameEnvironment>
		struct Locate;

		template<char... Chars, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Name<Chars...>, NowStringPool, NowFrameEnvironment> {
			using LocatedAST = decltype( getFramePosition<type_analysis::Identifier<Chars...>>( NowFrameEnvironment{} ) );
			using NextStringPool = NowStringPool;
		};

		template<char... Chars, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::IntLiteral<Chars...>, NowStringPool, NowFrameEnvironment> {
			using LocatedAST = parser::IntLiteral<Chars...>;
			using NextStringPool = NowStringPool;
		};

		template<char... Chars, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::CharLiteral<Chars...>, NowStringPool, NowFrameEnvironment> {
			using LocatedAST = parser::CharLiteral<Chars...>;
			using NextStringPool = NowStringPool;
		};

		template<char... Chars, class... Strings, class NowFrameEnvironment>
		struct Locate<parser::StringLiteral<Chars...>, StringPool<Strings...>, NowFrameEnvironment> {
			using LocatedAST = StringRef<sizeof...(Strings)>;
			using NextStringPool = StringPool<Strings..., String<Chars...>>;
		};

		template<class NowStringPool, class NowFrameEnvironment>
		struct Locate<type_tuple<>, NowStringPool, NowFrameEnvironment> {
			using LocatedAST = type_tuple<>;
			using NextStringPool = NowStringPool;
		};

		template<class T, class... Ts, class NowStringPool, class NowFrameEnvironment>
		struct Locate<type_tuple<T, Ts...>, NowStringPool, NowFrameEnvironment> {
			using lhsResult = Locate<T, NowStringPool, NowFrameEnvironment>;
			using lhsStringPool = typename lhsResult::NextStringPool;
			using rhsResult = Locate<type_tuple<Ts...>, lhsStringPool, NowFrameEnvironment>;
			using LocatedAST = decltype( merge( typename lhsResult::LocatedAST{}, typename rhsResult::LocatedAST{} ) );
			using NextStringPool = typename rhsResult::NextStringPool;
		};

		// disambiguation
		template<class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<type_tuple<T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = type_tuple<typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<class... Ts>
		auto makeFunc_Node( type_tuple<Ts...> )
			-> parser::Func_Node<Ts...>;

		template<class T, class... Args, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Func_Node<T, Args...>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<type_tuple<T, Args...>, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = decltype( makeFunc_Node( typename Result::LocatedAST{} ) );
			using NextStringPool = typename Result::NextStringPool;
		};

		// disambiguation
		template<class T, class Arg, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Func_Node<T, Arg>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultArg = Locate<Arg, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = parser::Func_Node<typename ResultT::LocatedAST, typename ResultArg::LocatedAST>;
			using NextStringPool = typename ResultArg::NextStringPool;
		};

		template<class T, class U ,class V, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Conditional_Node<T, U, V>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using StringPoolU = typename ResultU::NextStringPool;
			using ResultV = Locate<V, StringPoolU, NowFrameEnvironment>;
			using LocatedAST = parser::Conditional_Node<typename ResultT::LocatedAST, typename ResultU::LocatedAST, typename ResultV::LocatedAST>;
			using NextStringPool = typename ResultV::NextStringPool;
		};

		template<template<class...>class BinaryNode, class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<BinaryNode<T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = BinaryNode<typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<parser::AssignOpType Op, class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Assign_Node<Op, T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = parser::Assign_Node<Op, typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<parser::ShiftOpType Op, class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Shift_Node<Op, T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = parser::Shift_Node<Op, typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<parser::CompareOpType Op, class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Compare_Node<Op, T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = parser::Compare_Node<Op, typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<parser::EqCompOpType Op, class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::EqComp_Node<Op, T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = parser::EqComp_Node<Op, typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<parser::PlusOpType Op, class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Plus_Node<Op, T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = parser::Plus_Node<Op, typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<parser::MultOpType Op, class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Mult_Node<Op, T, U>, NowStringPool, NowFrameEnvironment> {
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedAST = parser::Mult_Node<Op, typename ResultT::LocatedAST, typename ResultU::LocatedAST>;
			using NextStringPool = typename ResultU::NextStringPool;
		};

		template<parser::PrefixUnaryOpType Op, class T, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Unary_Node<Op, T>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = parser::Unary_Node<Op, typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		template<parser::SuffixUnaryOpType Op, class T, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::SuffixUnary_Node<Op, T>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = parser::SuffixUnary_Node<Op, typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		template<class T, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Term_Node<T>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = parser::Term_Node<typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		template<class Ty, class T, class NowStringPool, class NowFrameEnvironment>
		struct Locate<parser::Cast_Node<Ty, T>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = parser::Cast_Node<Ty, typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		template<class AST, class NowStringPool, class NowFrameEnvironment>
		struct Locate<type_analysis::IntegralPromotion<AST>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<AST, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = type_analysis::IntegralPromotion<typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		template<class AST, class NowStringPool, class NowFrameEnvironment>
		struct Locate<type_analysis::ArrayDecay<AST>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<AST, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = type_analysis::ArrayDecay<typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		template<class Ty, class AST, bool Lval, class NowStringPool, class NowFrameEnvironment>
		struct Locate<type_analysis::Typed<Ty, AST, Lval>, NowStringPool, NowFrameEnvironment> {
			using Result = Locate<AST, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = type_analysis::Typed<Ty, typename Result::LocatedAST, Lval>;
			using NextStringPool = typename Result::NextStringPool;
		};

		// ------ StatementFold ------
		template<class Statement, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold;

		// --- NullStatement ---
		template<class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::NullStatement, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using LocatedAST = parser::NullStatement;
			using NextStringPool = NowStringPool;
		};

		// --- Labeled ---
		template<class Label, class Statement, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<type_analysis::Labeled<Label, Statement>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using LocatedAST = type_analysis::Labeled<Label, typename StatementFold<Statement, NowStringPool, NowFrameEnvironment>::LocatedAST>;
			using NextStringPool = typename StatementFold<Statement, NowStringPool, NowFrameEnvironment>::NextStringPool;
		};

		// --- DeclareStatement ---
		template<class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::Statement_Node<parser::DeclareStatement_Node<>>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
		};

		template<class T, class... Ts, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::Statement_Node<parser::DeclareStatement_Node<T, Ts...>>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = typename StatementFold<parser::Statement_Node<parser::DeclareStatement_Node<Ts...>>, NowStringPool, decltype( add_frame_environment( NowFrameEnvironment{}, typename type_analysis::detail::ParseDeclare<T>{} ) )>::NextFrameEnvironment;
			using LocatedAST = SetSP_Node<typename NextFrameEnvironment::top>;
			using NextStringPool = NowStringPool;
		};

		// --- ExprStatement ---
		template<class T, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::ExprStatement_Node<T>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using Result = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = parser::ExprStatement_Node<typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		// --- BreakStatement ---
		template<class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::BreakStatement, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using LocatedAST = parser::BreakStatement;
			using NextStringPool = NowStringPool;
		};

		// --- ContinueStatement ---
		template<class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::ContinueStatement, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using LocatedAST = parser::ContinueStatement;
			using NextStringPool = NowStringPool;
		};

		// --- ReturnStatement ---
		template<class T, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::ReturnStatement<T>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using Result = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedAST = parser::ReturnStatement<typename Result::LocatedAST>;
			using NextStringPool = typename Result::NextStringPool;
		};

		// --- BraceStatement ---

		template<class... Ts>
		auto makeBraceStatement_Node( type_tuple<Ts...> )
			-> parser::Statement_Node<parser::BraceStatement_Node<Ts...>>;

		template<class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::Statement_Node<parser::BraceStatement_Node<>>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using LocatedAST_impl = type_tuple<>;
			using LocatedAST = parser::Statement_Node<parser::BraceStatement_Node<>>;
			using NextStringPool = NowStringPool;
		};

		template<class T, class... Ts, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::Statement_Node<parser::BraceStatement_Node<T, Ts...>>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using lhsResult = StatementFold<T, NowStringPool, NowFrameEnvironment>;
			using lhsStringPool = typename lhsResult::NextStringPool;
			using lhsFrameEnvironment = typename lhsResult::NextFrameEnvironment;
			using rhsResult = StatementFold<parser::Statement_Node<parser::BraceStatement_Node<Ts...>>, lhsStringPool, lhsFrameEnvironment>;
			using LocatedAST_impl = decltype( merge( typename lhsResult::LocatedAST{}, typename rhsResult::LocatedAST_impl{} ) );
			using LocatedAST = decltype( makeBraceStatement_Node( merge( LocatedAST_impl{}, SetSP_Node<typename NowFrameEnvironment::top>{} ) ) );
			using NextStringPool = typename rhsResult::NextStringPool;
		};

		// --- ForStatement ---
		template<class T, class U, class V, class W, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::ForStatement_Node<T, U, V, W>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedT = typename ResultT::LocatedAST;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedU = typename ResultU::LocatedAST;
			using StringPoolU = typename ResultU::NextStringPool;
			using ResultV = Locate<V, StringPoolU, NowFrameEnvironment>;
			using LocatedV = typename ResultV::LocatedAST;
			using StringPoolV = typename ResultV::NextStringPool;
			using ResultW = StatementFold<W, StringPoolV, NowFrameEnvironment>;
			using LocatedW = typename ResultW::LocatedAST;
			using StringPoolW = typename ResultW::NextStringPool;
			using LocatedAST = parser::ForStatement_Node<LocatedT, LocatedU, LocatedV, LocatedW>;
			using NextStringPool = StringPoolW;
		};

		// --- DoStatement ---
		template<class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::DoStatement_Node<T, U>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using ResultT = StatementFold<T, NowStringPool, NowFrameEnvironment>;
			using LocatedT = typename ResultT::LocatedAST;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedU = typename ResultU::LocatedAST;
			using StringPoolU = typename ResultU::NextStringPool;
			using LocatedAST = parser::DoStatement_Node<LocatedT, LocatedU>;
			using NextStringPool = StringPoolU;
		};

		// --- WhileStatement ---
		template<class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::WhileStatement_Node<T, U>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedT = typename ResultT::LocatedAST;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = Locate<U, StringPoolT, NowFrameEnvironment>;
			using LocatedU = typename ResultU::LocatedAST;
			using StringPoolU = typename ResultU::NextStringPool;
			using LocatedAST = parser::WhileStatement_Node<LocatedT, LocatedU>;
			using NextStringPool = StringPoolU;
		};

		// --- IfStatement ---
		template<class T, class U, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::IfStatement_Node<T, U>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedT = typename ResultT::LocatedAST;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = StatementFold<U, StringPoolT, NowFrameEnvironment>;
			using LocatedU = typename ResultU::LocatedAST;
			using StringPoolU = typename ResultU::NextStringPool;
			using LocatedAST = parser::IfStatement_Node<LocatedT, LocatedU>;
			using NextStringPool = StringPoolU;
		};

		template<class T, class U, class V, class NowStringPool, class NowFrameEnvironment>
		struct StatementFold<parser::IfStatement_Node<T, U, V>, NowStringPool, NowFrameEnvironment> {
			using NextFrameEnvironment = NowFrameEnvironment;
			using ResultT = Locate<T, NowStringPool, NowFrameEnvironment>;
			using LocatedT = typename ResultT::LocatedAST;
			using StringPoolT = typename ResultT::NextStringPool;
			using ResultU = StatementFold<U, StringPoolT, NowFrameEnvironment>;
			using LocatedU = typename ResultU::LocatedAST;
			using StringPoolU = typename ResultU::NextStringPool;
			using ResultV = StatementFold<V, StringPoolU, NowFrameEnvironment>;
			using LocatedV = typename ResultV::LocatedAST;
			using StringPoolV = typename ResultV::NextStringPool;
			using LocatedAST = parser::IfStatement_Node<LocatedT, LocatedU, LocatedV>;
			using NextStringPool = StringPoolV;
		};

		// ------ GlobalStatementFold ------
		template<class Statement, class NowEnvironment>
		struct GlobalStatementFold;

		// --- FunctionDefinition ---

		template<class... Ts>
		auto makeReturn( parser::Statement_Node<parser::BraceStatement_Node<Ts...>> )
			-> parser::Statement_Node<parser::BraceStatement_Node<Ts..., Return_Node>>;

		template<class T, class Ty, class St, class NowStringPool, class... NowGlobalEnvironment>
		struct GlobalStatementFold<parser::FunctionDefinition<T, Ty, St>, GlobalEnvironment<NowStringPool, NowGlobalEnvironment...>> {
			using FuncName = typename type_analysis::detail::ParseDeclare<parser::Declare<T, Ty>>::name;
			using CallFrameEnvironment = decltype( call_environment( T{} ) );
			using FoldResult = StatementFold<St, NowStringPool, CallFrameEnvironment>;
			using LocatedStatement = typename FoldResult::LocatedAST;
			using AddReturnStatement = decltype( makeReturn( LocatedStatement{} ) );
			using StringPoolEnvironment = GlobalEnvironment<typename FoldResult::NextStringPool, NowGlobalEnvironment...>;
			using NextEnvironment = decltype( add_global_environment_function( StringPoolEnvironment{}, FuncName{}, AddReturnStatement{} ) );
		};

		// --- DeclareStatement ---
		template<class NowEnvironment>
		struct GlobalStatementFold<parser::Statement_Node<parser::DeclareStatement_Node<>>, NowEnvironment> {
			using NextEnvironment = NowEnvironment;
		};

		template<class NowEnvironment, class T, class... Ts>
		struct GlobalStatementFold<parser::Statement_Node<parser::DeclareStatement_Node<T, Ts...>>, NowEnvironment> {
			using NextEnvironment = typename GlobalStatementFold<parser::Statement_Node<parser::DeclareStatement_Node<Ts...>>, decltype( add_global_environment( NowEnvironment{}, typename type_analysis::detail::ParseDeclare<T>{} ) )>::NextEnvironment;
			using LocatedAST = type_tuple<>;
		};

		// ------ Fold ------
		template<class AST, class NowEnvironment>
		struct Fold;

		template<class AST, class NowEnvironment>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, AST>, NowEnvironment> : GlobalStatementFold<AST, NowEnvironment> {};

		template<lfl::index_t... Index, class... AST, class NowEnvironment>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<Index...>, AST...>, NowEnvironment> {
			using tuple = lfl::flat_tuple<lfl::index_tuple<Index...>, AST...>;
			using tailResult = Fold<decltype(tail(tuple{})), NowEnvironment>;
			using headResult = Fold<decltype(head(tuple{})), typename tailResult::NextEnvironment>;
			using NextEnvironment = typename headResult::NextEnvironment;
		};

		template<class... AST>
		using Result = typename Fold<lfl::flat_tuple<lfl::make_index_tuple<sizeof...(AST)>, AST...>, GlobalEnvironment<StringPool<>>>::NextEnvironment;

	} // namespace detail

	template<class... AST>
	using Result = typename location::detail::Result<AST...>;

} // namespace location

#endif // LTMPC_LOCATION_HPP
