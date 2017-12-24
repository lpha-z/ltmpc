#ifndef LTMPC_TYPEANALYSIS_HPP
#define LTMPC_TYPEANALYSIS_HPP
#include "Parser.hpp"
#include "flat_tuple.hpp"

namespace type_analysis {

	template<class... AST>
	struct type_tuple {};

	template<class, std::size_t...>
	struct ArrayOf {};
	template<class>
	struct PtrTo {};
	template<class, class...>
	struct Function {};
	struct type_int {};
	struct type_char {};
	struct type_void {};

	template<class Ty, class AST, bool Lval>
	struct Typed {};
	template<class Ty, class AST, bool Lval>
	struct Typed<PtrTo<Ty>, AST, Lval> { using type = PtrTo<Ty>; using is_ptr = std::nullptr_t; };
	template<class AST, bool Lval>
	struct Typed<type_int, AST, Lval> { using is_integral = std::nullptr_t; };
	template<class AST, bool Lval>
	struct Typed<type_char, AST, Lval> { using is_integral = std::nullptr_t; };

	template<class T>
	struct IntegralPromotion {};
	template<class T>
	struct ArrayDecay {};

	template<class Label, class St>
	struct Labeled {};
	// TypeEnvironment
	template<char...>
	struct Identifier {};
	// NestEnvironment
	template<class Name, std::size_t... Ns>
	struct LoopNestId {};
	template<class Loop, std::size_t... Ns>
	struct IfNestId {};


	namespace detail {

		// --- type_tuple merge ---
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

		// --- IsParameterListValid ---
		template<class...>
		struct IsParameterListValid;
		template<>
		struct IsParameterListValid<> {};
		template<class... Params>
		struct IsParameterListValid<parser::Declare<parser::DirectDeclarator<parser::NullDeclarator, parser::Pointer<0>>, parser::type_void>, Params...> {
			// sizeof...(Params) == 0 is 'no-arg' declaration
			static_assert( sizeof...(Params) == 0, "type error: void type argument is invalid" );
		};
		template<char... Chars, class... Params>
		struct IsParameterListValid<parser::Declare<parser::DirectDeclarator<parser::Name<Chars...>, parser::Pointer<0>>, parser::type_void>, Params...> {
			static_assert( sizeof...(Params) && false, "type error: void type argument is invalid" );
		};
		template<class Param, class... Params>
		struct IsParameterListValid<Param, Params...> : IsParameterListValid<Params...> {};


		// --- ParseDeclare ---

		struct NullArg;

		template<class T>
		auto argDecay( T ) -> T;
		template<class Ty, std::size_t... Sz>
		auto argDecay( ArrayOf<Ty, Sz...> ) -> PtrTo<Ty>;

		template<class, class = void>
		struct ParseDeclare;
		template<char... Chars, class Ty, class... ArgTys>
		struct ParseDeclare<parser::Name<Chars...>, Function<Ty, ArgTys...>> { using name = Identifier<Chars...>; using rettype = Ty; using type = Function<Ty, ArgTys...>; using typed = Typed<name, Function<Ty, ArgTys...>, true>; };
		template<char... Chars, class Ty>
		struct ParseDeclare<parser::Name<Chars...>, Ty> { using name = Identifier<Chars...>; using type = Ty; using typed = Typed<name, Ty, true>; };
		template<class Ty>
		struct ParseDeclare<parser::NullDeclarator, Ty> { using name = NullArg; using type = Ty; };
		template<class T, class Ty>
		struct ParseDeclare<parser::DirectDeclarator<T, parser::Pointer<0>>, Ty> : ParseDeclare<T, Ty> {};
		template<class T, std::size_t N, class Ty>
		struct ParseDeclare<parser::DirectDeclarator<T, parser::Pointer<N>>, Ty> : ParseDeclare<parser::DirectDeclarator<T, parser::Pointer<N-1>>, PtrTo<Ty>> {};
		template<class T, class... PL, class Ty>
		struct ParseDeclare<parser::Function<T, parser::ParameterList<PL...>>, Ty> : ParseDeclare<T, Function<Ty, decltype( argDecay( typename ParseDeclare<PL>::type{} ) )...>> {
			IsParameterListValid<PL...> check;
		};
		template<class T, class Ty>
		struct ParseDeclare<parser::Array<T, parser::ArraySize<>>, Ty> : ParseDeclare<T, ArrayOf<Ty>> {};
		template<class T, std::size_t Sz, class Ty>
		struct ParseDeclare<parser::Array<T, parser::ArraySize<Sz>>, Ty> : ParseDeclare<T, ArrayOf<Ty, Sz>> {};

		template<class T>
		struct ParseDeclare<parser::Declare<T, parser::type_char>> : ParseDeclare<T, type_char> {};
		template<class T>
		struct ParseDeclare<parser::Declare<T, parser::type_int>> : ParseDeclare<T, type_int> {};
		template<class T>
		struct ParseDeclare<parser::Declare<T, parser::type_void>> : ParseDeclare<T, type_void> {};


		// --- TypeEnvironment ---
		template<class>
		struct Type {};

		template<class...>
		struct IdentifierList {};

		template<class...>
		struct TypeList {};

		template<class, class>
		struct indexed_type {};

		template<class, class>
		struct TypeEnvironment;
		template<class... Ids, class... Tys>
		struct TypeEnvironment<IdentifierList<Ids...>, TypeList<Tys...>> : indexed_type<Ids, Tys>... {};

		template<class... Ids, class... Tys, class Id, class Ty>
		auto add_environment( TypeEnvironment<IdentifierList<Ids...>, TypeList<Tys...>>, Id, Ty )
			-> TypeEnvironment<IdentifierList<Ids..., Id>, TypeList<Tys..., Ty>>;

		template<class... Ids, class... Tys, class DD, class... Ts, std::size_t N>
		auto add_arg_environment( TypeEnvironment<IdentifierList<Ids...>, TypeList<Tys...>>, parser::DirectDeclarator<parser::Function<DD, parser::ParameterList<Ts...>>, parser::Pointer<N>> )
			-> TypeEnvironment<IdentifierList<Ids..., typename ParseDeclare<Ts>::name...>, TypeList<Tys..., decltype( argDecay( typename ParseDeclare<Ts>::type{} ) )...>>;

		template<std::nullptr_t N = nullptr>
		struct Error_Undefined_Identifier {
			static_assert( N != nullptr, "error: undefined identifier" );
		};

		template<class Id, class Ty>
		auto getType( indexed_type<Id, Ty> ) -> Ty;
		template<class Id>
		auto getType( ... ) -> Error_Undefined_Identifier<>;


		// --- NestEnvironment ---
		struct InGlobal {};
		template<class RetTy>
		struct InFunction {};

		template<class...>
		struct NestEnvironment {};

		template<std::nullptr_t N = nullptr>
		struct Error_Cannot_Define_Function_in_Function {
			static_assert( N != nullptr, "error: cannot define function in function" );
		};

		template<class Name, class RetTy>
		auto intoFunc( Name, RetTy, NestEnvironment<InGlobal> )
			-> NestEnvironment<InFunction<RetTy>, LoopNestId<Name, 0>, IfNestId<void>>;

		auto intoFunc( ... )
			-> Error_Cannot_Define_Function_in_Function<>;

		template<class Ty, class Name, std::size_t N, std::size_t... Ns, std::size_t... Ms>
		auto nextNest( NestEnvironment<InFunction<Ty>, LoopNestId<Name, N, Ns...>, IfNestId<void, Ms...>> )
			-> NestEnvironment<InFunction<Ty>, LoopNestId<Name, N+1, Ns...>, IfNestId<void>>;

		template<class Ty, class Name, std::size_t... Ns, std::size_t... Ms>
		auto addNest( NestEnvironment<InFunction<Ty>, LoopNestId<Name, Ns...>, IfNestId<void, Ms...>> )
			-> NestEnvironment<InFunction<Ty>, LoopNestId<Name, 0, Ns...>, IfNestId<void>>;

		template<class Ty, class Name, std::size_t... Ns, std::size_t... Ms>
		auto getEndLabel( NestEnvironment<InFunction<Ty>, LoopNestId<Name, Ns...>, IfNestId<void, Ms...>> )
			-> IfNestId<LoopNestId<Name, Ns...>, Ms...>;

		template<class Ty, class Name, std::size_t... Ns, std::size_t... Ms>
		auto intoThen( NestEnvironment<InFunction<Ty>, LoopNestId<Name, Ns...>, IfNestId<void, Ms...>> )
			-> NestEnvironment<InFunction<Ty>, LoopNestId<Name, Ns...>, IfNestId<void, 0, Ms...>>;

		template<class Ty, class Name, std::size_t... Ns, std::size_t... Ms>
		auto intoElse( NestEnvironment<InFunction<Ty>, LoopNestId<Name, Ns...>, IfNestId<void, Ms...>> )
			-> NestEnvironment<InFunction<Ty>, LoopNestId<Name, Ns...>, IfNestId<void, 1, Ms...>>;



		// --- Convertible ---
		template<class T, class U>
		struct Convertible {};
		// OK: T <- T
		template<class T>
		struct Convertible<T, T> { using type = std::nullptr_t; };
		// OK: void* <- any*
		template<class T>
		struct Convertible<PtrTo<type_void>, PtrTo<T>> { using type = std::nullptr_t; };
		// NG: void* <- func*
		template<class... T>
		struct Convertible<PtrTo<type_void>, PtrTo<Function<T...>>> {};
		// OK: any* <- void*
		template<class T>
		struct Convertible<PtrTo<T>, PtrTo<type_void>> { using type = std::nullptr_t; };
		// NG: func* <- void*
		template<class... T>
		struct Convertible<PtrTo<Function<T...>>, PtrTo<type_void>> {};
		// OK: int <- char
		template<>
		struct Convertible<type_int, type_char> { using type = std::nullptr_t; };
		// OK: char <- int
		template<>
		struct Convertible<type_char, type_int> { using type = std::nullptr_t; };
		// OK: void <- any
		template<class T>
		struct Convertible<type_void, T> { using type = std::nullptr_t; };
		// OK: void <- void (disambiguation)
		template<>
		struct Convertible<type_void, type_void> { using type = std::nullptr_t; };


		// ---Error definitions ---
		template<std::nullptr_t N = nullptr>
		struct Error_Add_Ptr_And_Ptr {
			static_assert( N != nullptr, "type error: cannot add pointer and pointer" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Invalid_Ptr_Operation {
			static_assert( N != nullptr, "type error: invalid pointer operation" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Different_Ptr_Subtract {
			static_assert( N != nullptr, "type error: cannot calculate distance between two different pointer types" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Different_Ptr_Compare {
			static_assert( N != nullptr, "type error: cannot compare two different pointer types" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Cannot_Get_Rval_Address {
			static_assert( N != nullptr, "type error: cannot get address of r-value" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Cannot_Apply_Subscript {
			static_assert( N != nullptr, "type error: cannot apply subscript to non-pointer value" );
		};

		template<bool IsFunction, bool ArgMatch = true>
		struct Error_Cannot_Apply_Function {
			static_assert( IsFunction, "type error: cannot apply () to non-function value" );
			static_assert( ArgMatch, "type error: function argument(s) type missmatch" );
		};

		template<class T, class U, std::nullptr_t N = nullptr>
		struct Error_Assign_Type_Not_Convertible {
			static_assert( N != nullptr, "type error: cannot assign because the type is not convertible" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Cast_Type_Not_Convertible {
			static_assert( N != nullptr, "type error: cannot cast because the type is not convertible" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Conditional_Operator_Type_Not_Convertible {
			static_assert( N != nullptr, "type error: non convertible types in conditional operator" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Cannot_IncDec_Function_Ptr {
			static_assert( N != nullptr, "type error: cannot increment/decrement function pointer" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Cannot_IncDec_Rval {
			static_assert( N != nullptr, "type error: cannot increment/decrement r-value" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Cannot_Apply_Operator_To_Void {
			static_assert( N != nullptr, "type error: cannot apply any operator (except cast/comma; including return) to void value" );
		};

		template<std::nullptr_t N = nullptr>
		struct Error_Void_Condition {
			static_assert( N != nullptr, "type error: condition must not be void type" );
		};

		template<class T, std::nullptr_t N = nullptr>
		struct NonVoidCondition {};

		template<class AST, std::nullptr_t N>
		struct NonVoidCondition<Typed<type_void, AST, false>, N> {
			static_assert( N != nullptr, "type error: condition must not be void type" );
		};

		// --- FunctionApply ---
		template<class, class...>
		struct FunctionApply;

		template<class Ty, class ArgTy1, class ArgTy2, class... ArgTys>
		struct FunctionApply<Ty, ArgTy1, ArgTy2, ArgTys...> {
			template<class Arg, class... Args, typename Convertible<ArgTy1, Arg>::type = nullptr>
			auto operator()( Arg, Args... )
				-> decltype( FunctionApply<Ty, ArgTy2, ArgTys...>{}( Args{}... ) );
			auto operator()( ... )
				-> Error_Cannot_Apply_Function<true, false>;
		};

		template<class Ty, class ArgTy>
		struct FunctionApply<Ty, ArgTy> {
			template<class Arg, typename Convertible<ArgTy, Arg>::type = nullptr>
			auto operator()( Arg )
				-> Ty;
			auto operator()( ... )
				-> Error_Cannot_Apply_Function<true, false>;
		};

		// f(any)
		template<class Ty>
		struct FunctionApply<Ty> {
			template<class... Args>
			auto operator()( Args... )
				-> Ty;
		};

		// f(void)
		template<class Ty>
		struct FunctionApply<Ty, type_void> {
			auto operator()()
				-> Ty;
			template<class... Other>
			auto operator()( Other... )
				-> Error_Cannot_Apply_Function<true, false>;
		};


		// --- type check ---
		template<class T, class U, typename T::is_ptr = nullptr, typename U::is_ptr = nullptr>
		auto addType( T, U )
			-> Error_Add_Ptr_And_Ptr<>;

		template<class T, class U, typename T::is_ptr = nullptr, typename U::is_integral = nullptr>
		auto addType( T, U )
			-> typename T::type;

		template<class T, class U, typename T::is_integral = nullptr, typename U::is_ptr = nullptr>
		auto addType( T, U )
			-> typename U::type;

		template<class T, class U, typename T::is_integral = nullptr, typename U::is_integral = nullptr>
		auto addType( T, U )
			-> type_int;

		template<class T, class U>
		auto makeAddTyped( T, U )
			-> Typed<decltype( addType( T{}, U{} ) ), parser::Plus_Node<parser::PlusOpType::Plus, T, U>, false>;

		template<class T, class U, typename T::is_ptr = nullptr, typename U::is_ptr = nullptr>
		auto subType( T, U )
			-> Error_Different_Ptr_Subtract<>;

		template<class Ty, class T, class U>
		auto subType( Typed<PtrTo<Ty>, T, false>, Typed<PtrTo<Ty>, U, false> )
			-> type_int;

		template<class T, class U, typename T::is_integral = nullptr, typename U::is_ptr = nullptr>
		auto subType( T, U )
			-> Error_Invalid_Ptr_Operation<>;

		template<class T, class U, typename T::is_ptr = nullptr, typename U::is_integral = nullptr>
		auto subType( T, U )
			-> typename T::type;

		template<class T, class U, typename T::is_integral = nullptr, typename U::is_integral = nullptr>
		auto subType( T, U )
			-> type_int;

		template<class T, class U>
		auto makeSubTyped( T, U )
			-> Typed<decltype( subType( T{}, U{} ) ), parser::Plus_Node<parser::PlusOpType::Minus, T, U>, false>;

		template<class T, class U, class AST, typename T::is_integral = nullptr, typename U::is_integral = nullptr>
		auto integralBinaryOpTyped( T, U, AST )
			-> Typed<type_int, AST, false>;

		auto integralBinaryOpTyped( ... )
			-> Error_Invalid_Ptr_Operation<>;

		struct FailureAssign {
			template<class T, class U>
			auto operator()( T, U )
				-> Error_Assign_Type_Not_Convertible<T, U>;
		};

		struct MakeNormalAssignTyped : FailureAssign {
			template<class Ty, class AST, class Ty2, class AST2, typename Convertible<Ty, Ty2>::type = nullptr>
			auto operator()( Typed<Ty, AST, true>, Typed<Ty2, AST2, false> )
				-> Typed<Ty, parser::Assign_Node<parser::AssignOpType::NormalAssign, Typed<Ty, AST, true>, Typed<Ty2, AST2, false>>, false>;

			using FailureAssign::operator();
		};

		template<parser::AssignOpType Op>
		struct MakeIntOpAssignTyped : FailureAssign {
			template<class AST, class AST2>
			auto operator()( Typed<type_int, AST, true>, Typed<type_int, AST2, false> )
				-> Typed<type_int, parser::Assign_Node<Op, Typed<type_int, AST, true>, Typed<type_int, AST2, false>>, false>;

			template<class AST, class AST2>
			auto operator()( Typed<type_char, AST, true>, Typed<type_int, AST2, false> )
				-> Typed<type_char, parser::Assign_Node<Op, Typed<type_char, AST, true>, Typed<type_int, AST2, false>>, false>;

			using FailureAssign::operator();
		};

		template<parser::AssignOpType Op>
		struct MakePlusAssignTyped :MakeIntOpAssignTyped<Op> {
			template<class Ty, class AST, class AST2>
			auto operator()( Typed<PtrTo<Ty>, AST, true>, Typed<type_int, AST2, false> )
				-> Typed<PtrTo<Ty>, parser::Assign_Node<Op, Typed<PtrTo<Ty>, AST, true>, Typed<type_int, AST2, false>>, false>;

			template<class... Ty, class AST, class AST2>
			auto operator()( Typed<PtrTo<Function<Ty...>>, AST, true>, Typed<type_int, AST2, false> )
				-> Error_Invalid_Ptr_Operation<>;

			using MakeIntOpAssignTyped<Op>::operator();
		};

		template<parser::AssignOpType Op>
		struct MakeAssignTyped : MakeIntOpAssignTyped<Op> {};

		template<>
		struct MakeAssignTyped<parser::AssignOpType::NormalAssign> : MakeNormalAssignTyped {};

		template<>
		struct MakeAssignTyped<parser::AssignOpType::PlusAssign> : MakePlusAssignTyped<parser::AssignOpType::PlusAssign> {};

		template<>
		struct MakeAssignTyped<parser::AssignOpType::MinusAssign> : MakePlusAssignTyped<parser::AssignOpType::MinusAssign> {};

		// FIXME
		template<class T, class Ty1, class AST1, class Ty2, class AST2, typename Convertible<Ty1, Ty2>::type = nullptr>
		auto makeConditionalTyped( T, Typed<Ty1, AST1, false>, Typed<Ty2, AST2, false> )
			-> Typed<Ty1, parser::Conditional_Node<T, Typed<Ty1, AST1, false>, Typed<Ty2, AST2, false>>, false>;

		template<class T, class U, class V>
		auto makeConditionalTyped( Typed<type_void, T, false>, U, V )
			-> Error_Void_Condition<>;

		template<class T, class U, class V>
		auto makeConditionalTyped( T, U, V )
			-> Error_Conditional_Operator_Type_Not_Convertible<>;

		template<class T, class Ty, class U>
		auto makeCommaTyped( T, Typed<Ty, U, false> )
			-> Typed<Ty, parser::CommaExpr_Node<T, Typed<Ty, U, false>>, false>;

		template<template<class...>class BinaryNode, class T, class U>
		auto makeBinaryTyped( T, U )
			-> decltype( integralBinaryOpTyped( T{}, U{}, BinaryNode<T, U>{} ) );

		template<class OpType, template<OpType, class...>class BinaryNode, OpType Op, class T, class U>
		auto makeMultiBinaryTyped( T, U )
			-> decltype( integralBinaryOpTyped( T{}, U{}, BinaryNode<Op, T, U>{} ) );

		template<parser::CompareOpType Op, class Ty, class T, class U>
		auto makePtrCompareTyped( Typed<PtrTo<Ty>, T, false>, Typed<PtrTo<Ty>, U, false> )
			-> Typed<type_int, parser::Compare_Node<Op, Typed<PtrTo<Ty>, T, false>, Typed<PtrTo<Ty>, U, false>>, false>;

		template<parser::CompareOpType Op, class Ty, class T, class U>
		auto makePtrCompareTyped( Typed<PtrTo<type_void>, T, false>, Typed<PtrTo<Ty>, U, false> )
			-> Typed<type_int, parser::Compare_Node<Op, Typed<PtrTo<type_void>, T, false>, Typed<PtrTo<type_void>, U, false>>, false>;

		template<parser::CompareOpType Op, class Ty, class T, class U>
		auto makePtrCompareTyped( Typed<PtrTo<Ty>, T, false>, Typed<PtrTo<type_void>, U, false> )
			-> Typed<type_int, parser::Compare_Node<Op, Typed<PtrTo<type_void>, T, false>, Typed<PtrTo<type_void>, U, false>>, false>;

		template<parser::CompareOpType Op>
		auto makePtrCompareTyped( ... )
			-> Error_Different_Ptr_Compare<>;

		template<parser::CompareOpType Op, class Ty, class T, class Ty2, class U>
		auto makeCompareTyped( Typed<PtrTo<Ty>, T, false> t, Typed<PtrTo<Ty2>, U, false> u )
			-> decltype( makePtrCompareTyped( t, u ) );

		template<parser::CompareOpType Op, class T, class U>
		auto makeCompareTyped( T, U )
			-> decltype( makeMultiBinaryTyped<parser::CompareOpType, parser::Compare_Node, Op>( T{}, U{} ) );

		template<parser::EqCompOpType Op, class Ty, class T, class U>
		auto makePtrEqCompTyped( Typed<PtrTo<Ty>, T, false>, Typed<PtrTo<Ty>, U, false> )
			-> Typed<type_int, parser::EqComp_Node<Op, Typed<PtrTo<Ty>, T, false>, Typed<PtrTo<Ty>, U, false>>, false>;

		template<parser::EqCompOpType Op, class Ty, class T, class U>
		auto makePtrEqCompTyped( Typed<PtrTo<type_void>, T, false>, Typed<PtrTo<Ty>, U, false> )
			-> Typed<type_int, parser::EqComp_Node<Op, Typed<PtrTo<type_void>, T, false>, Typed<PtrTo<type_void>, U, false>>, false>;

		template<parser::EqCompOpType Op, class Ty, class T, class U>
		auto makePtrEqCompTyped( Typed<PtrTo<Ty>, T, false>, Typed<PtrTo<type_void>, U, false> )
			-> Typed<type_int, parser::EqComp_Node<Op, Typed<PtrTo<type_void>, T, false>, Typed<PtrTo<type_void>, U, false>>, false>;

		template<parser::EqCompOpType Op>
		auto makePtrEqCompTyped( ... )
			-> Error_Different_Ptr_Compare<>;

		template<parser::EqCompOpType Op, class Ty, class T, class Ty2, class U>
		auto makeCompareTyped( Typed<PtrTo<Ty>, T, false> t, Typed<PtrTo<Ty2>, U, false> u )
			-> decltype( makePtrEqCompTyped( t, u ) );

		template<parser::EqCompOpType Op, class T, class U>
		auto makeEqCompTyped( T, U )
			-> decltype( makeMultiBinaryTyped<parser::EqCompOpType, parser::EqComp_Node, Op>( T{}, U{} ) );

		template<class... Ty, class AST>
		auto makeAddressOf( Typed<Function<Ty...>, AST, true> )
			-> Typed<PtrTo<Function<Ty...>>, parser::Unary_Node<parser::PrefixUnaryOpType::AddressOf, Typed<Function<Ty...>, AST, true>>, false>;

		template<class Ty, class AST>
		auto makeAddressOf( Typed<Ty, AST, true> )
			-> Typed<PtrTo<Ty>, parser::Unary_Node<parser::PrefixUnaryOpType::AddressOf, Typed<Ty, AST, true>>, false>;

		auto makeAddressOf( ... )
			-> Error_Cannot_Get_Rval_Address<>;

		template<class Ty, class AST, bool Lval>
		auto makeDereference( Typed<PtrTo<Ty>, AST, Lval> )
			-> Typed<Ty, parser::Unary_Node<parser::PrefixUnaryOpType::Dereference, Typed<PtrTo<Ty>, AST, Lval>>, true>;

		template<parser::PrefixUnaryOpType Op, class Ty, class AST>
		auto makePrefixIncDec( Typed<Ty, AST, true> )
			-> Typed<Ty, parser::Unary_Node<Op, Typed<Ty, AST, true>>, false>;

		template<parser::PrefixUnaryOpType Op, class... Ty, class AST>
		auto makePrefixIncDec( Typed<PtrTo<Function<Ty...>>, AST, true> )
			-> Error_Cannot_IncDec_Function_Ptr<>;

		template<parser::PrefixUnaryOpType Op, class Ty, class AST>
		auto makePrefixIncDec( Typed<Ty, AST, false> )
			-> Error_Cannot_IncDec_Rval<>;

		template<parser::PrefixUnaryOpType Op, class Ty, class AST, bool Lval>
		auto makePrefixUnary( Typed<Ty, AST, Lval> )
			-> Typed<Ty, parser::Unary_Node<Op, Typed<Ty, AST, Lval>>, false>;

		template<parser::PrefixUnaryOpType Op, class AST>
		auto makePrefixUnary( Typed<type_void, AST, false> )
			-> Error_Cannot_Apply_Operator_To_Void<>;

		template<class Ty, class AST, bool Lval, class AST2, bool Lval2>
		auto makeSubscript( Typed<PtrTo<Ty>, AST, Lval>, Typed<type_int, AST2, Lval2> )
			-> Typed<Ty, parser::Subscript_Node<Typed<PtrTo<Ty>, AST, Lval>, Typed<type_int, AST2, Lval2>>, true>;

		template<class AST, bool Lval, class Ty2, class AST2, bool Lval2>
		auto makeSubscript( Typed<type_int, AST, Lval>, Typed<PtrTo<Ty2>, AST2, Lval2> )
			-> Typed<Ty2, parser::Subscript_Node<Typed<type_int, AST, Lval>, Typed<PtrTo<Ty2>, AST2, Lval2>>, true>;

		auto makeSubscript( ... )
			-> Error_Cannot_Apply_Subscript<>;

		template<parser::SuffixUnaryOpType Op, class Ty, class AST>
		auto makeSuffixUnary( Typed<Ty, AST, true> )
			-> Typed<Ty, parser::SuffixUnary_Node<Op, Typed<Ty, AST, true>>, false>;

		template<parser::SuffixUnaryOpType Op, class Ty, class AST>
		auto makeSuffixUnary( Typed<Ty, AST, false> )
			-> Error_Cannot_IncDec_Rval<>;

		template<parser::SuffixUnaryOpType Op, class AST>
		auto makeSufixUnary( Typed<type_void, AST, false> )
			-> Error_Cannot_Apply_Operator_To_Void<>;

		template<class Ty, class AST, bool Lval>
		auto makeTerm( Typed<Ty, AST, Lval> )
			-> Typed<Ty, parser::Term_Node<Typed<Ty, AST, Lval>>, false>;

		template<class... Ty, class AST, bool Lval, class... Tys, class... ASTs, bool... Lvals>
		auto makeFunc( Typed<PtrTo<Function<Ty...>>, AST, Lval> func, Typed<Tys, ASTs, Lvals>... args )
			-> Typed<decltype( FunctionApply<Ty...>{}( Tys{}... ) ), parser::Func_Node<decltype(func), decltype(args)...>, false>;

		auto makeFunc( ... )
			-> Error_Cannot_Apply_Function<false>;

		template<class ParserCastTy, class CastTy, class Ty, class AST, typename Convertible<CastTy, Ty>::type = nullptr>
		auto makeCast( ParserCastTy, CastTy, Typed<Ty, AST, false> )
			-> Typed<CastTy, parser::Cast_Node<ParserCastTy, Typed<Ty, AST, false>>, false>;

		auto makeCast( ... )
			-> Error_Cast_Type_Not_Convertible<>;

		// ------ Expr ------
		template<class Ty, class AST, bool Lval>
		auto promote( Typed<Ty, AST, Lval> )
			-> Typed<Ty, AST, false>;

		// Lval = true: variable  Lval = false: stringLiteral 
		template<std::size_t... Sz, class Ty, class AST, bool Lval>
		auto promote( Typed<ArrayOf<Ty, Sz...>, AST, Lval> )
			-> Typed<PtrTo<Ty>, ArrayDecay<Typed<ArrayOf<Ty, Sz...>, AST, Lval>>, false>;

		template<class AST, bool Lval>
		auto promote( Typed<type_char, AST, Lval> )
			-> Typed<type_int, IntegralPromotion<Typed<type_char, AST, false>>, false>;

		template<class AST>
		auto promote( Typed<type_void, AST, false> )
			-> Error_Cannot_Apply_Operator_To_Void<>;

		template<class... Ty, class AST, bool Lval>
		auto promote( Typed<Function<Ty...>, AST, Lval> )
			-> Typed<PtrTo<Function<Ty...>>, AST, Lval>;

		auto promote( parser::NullExpr )
			-> parser::NullExpr;

		template<class T>
		auto promote_void( T )
			-> decltype( promote( T{} ) );

		template<class AST>
		auto promote_void( Typed<type_void, AST, false> )
			-> Typed<type_void, AST, false>;


		template<class Environment>
		auto typed_expr( parser::NullExpr, Environment )
			-> parser::NullExpr;

		template<char... Chars, class Environment>
		auto typed_expr( parser::Name<Chars...>, Environment env )
			-> Typed<decltype( getType<Identifier<Chars...>>( env ) ), parser::Name<Chars...>, true>;

		template<char... Chars, class Environment>
		auto typed_expr( parser::IntLiteral<Chars...>, Environment )
			-> Typed<type_int, parser::IntLiteral<Chars...>, false>;

		template<char... Chars, class Environment>
		auto typed_expr( parser::CharLiteral<Chars...>, Environment )
			-> Typed<type_int, parser::CharLiteral<Chars...>, false>;

		template<char... Chars, class Environment>
		auto typed_expr( parser::StringLiteral<Chars...>, Environment )
			-> Typed<ArrayOf<type_char, sizeof...(Chars)+1>, parser::StringLiteral<Chars...>, false>;

		template<class T, class U, class V, class Environment>
		auto typed_expr( parser::Conditional_Node<T, U, V>, Environment env )
			-> decltype( makeConditionalTyped( promote( typed_expr( T{}, env ) ), promote_void( typed_expr( U{}, env ) ) , promote_void( typed_expr( V{}, env ) ) ) );

		template<template<class...>class BinaryNode>
		struct typed_expr_impl {
			template<class T, class U, class Environment>
			auto operator()( BinaryNode<T, U>, Environment env )
				-> decltype( makeBinaryTyped<BinaryNode>( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );
		};

		template<>
		struct typed_expr_impl<parser::CommaExpr_Node> {};

		template<>
		struct typed_expr_impl<parser::Func_Node> {};

		template<>
		struct typed_expr_impl<parser::Conditional_Node> {
			// this is a work around for clang++ bug
		};

		template<class T, class U, class Environment>
		auto typed_expr( parser::CommaExpr_Node<T, U>, Environment env )
			-> decltype( makeCommaTyped( promote_void( typed_expr( T{}, env ) ), promote_void( typed_expr( U{}, env ) ) ) );


		template<template<class...>class BinaryNode, class T, class U, class Environment>
		auto typed_expr( BinaryNode<T, U>, Environment env )
			-> decltype( typed_expr_impl<BinaryNode>{}( BinaryNode<T, U>{}, env ) );

		template<parser::EqCompOpType Op, class T, class U, class Environment>
		auto typed_expr( parser::EqComp_Node<Op, T, U>, Environment env )
			-> decltype( makeEqCompTyped<Op>( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );

		template<parser::CompareOpType Op, class T, class U, class Environment>
		auto typed_expr( parser::Compare_Node<Op, T, U>, Environment env )
			-> decltype( makeCompareTyped<Op>( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );

		template<parser::ShiftOpType Op, class T, class U, class Environment>
		auto typed_expr( parser::Shift_Node<Op, T, U>, Environment env )
			-> decltype( makeMultiBinaryTyped<parser::ShiftOpType, parser::Shift_Node, Op>( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );

		template<parser::MultOpType Op, class T, class U, class Environment>
		auto typed_expr( parser::Mult_Node<Op, T, U>, Environment env )
			-> decltype( makeMultiBinaryTyped<parser::MultOpType, parser::Mult_Node, Op>( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );

		template<parser::AssignOpType Op, class T, class U, class Environment>
		auto typed_expr( parser::Assign_Node<Op, T, U>, Environment env )
			-> decltype( MakeAssignTyped<Op>{}( typed_expr( T{}, env ), promote( typed_expr( U{}, env ) ) ) );

		template<class T, class U, class Environment>
		auto typed_expr( parser::Plus_Node<parser::PlusOpType::Plus, T, U>, Environment env )
			-> decltype( makeAddTyped( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );

		template<class T, class U, class Environment>
		auto typed_expr( parser::Plus_Node<parser::PlusOpType::Minus, T, U>, Environment env )
			-> decltype( makeSubTyped( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );

		template<class T, class Environment>
		auto typed_expr( parser::Unary_Node<parser::PrefixUnaryOpType::AddressOf, T>, Environment env )
			-> decltype( makeAddressOf( typed_expr( T{}, env ) ) );

		template<class T, class Environment>
		auto typed_expr( parser::Unary_Node<parser::PrefixUnaryOpType::Dereference, T>, Environment env )
			-> decltype( makeDereference( promote( typed_expr( T{}, env ) ) ) );

		template<class T, class Environment>
		auto typed_expr( parser::Unary_Node<parser::PrefixUnaryOpType::Increment, T>, Environment env )
			-> decltype( makePrefixIncDec<parser::PrefixUnaryOpType::Increment>( typed_expr( T{}, env ) ) );

		template<class T, class Environment>
		auto typed_expr( parser::Unary_Node<parser::PrefixUnaryOpType::Decrement, T>, Environment env )
			-> decltype( makePrefixIncDec<parser::PrefixUnaryOpType::Decrement>( typed_expr( T{}, env ) ) );

		template<parser::PrefixUnaryOpType Op, class T, class Environment, typename std::enable_if<Op != parser::PrefixUnaryOpType::None, std::nullptr_t>::type = nullptr>
		auto typed_expr( parser::Unary_Node<Op, T>, Environment env )
			-> decltype( makePrefixUnary<Op>( promote( typed_expr( T{}, env ) ) ) );

		template<class T, class U, class Environment>
		auto typed_expr( parser::Subscript_Node<T, U>, Environment env )
			-> decltype( makeSubscript( promote( typed_expr( T{}, env ) ), promote( typed_expr( U{}, env ) ) ) );

		template<parser::SuffixUnaryOpType Op, class T, class Environment>
		auto typed_expr( parser::SuffixUnary_Node<Op, T>, Environment env )
			-> decltype( makeSuffixUnary<Op>( typed_expr( T{}, env ) ) );

		template<class T, class Environment>
		auto typed_expr( parser::Term_Node<T>, Environment env )
			-> decltype( makeTerm( promote( typed_expr( T{}, env ) ) ) );

		template<class... T, class Environment>
		auto typed_expr( parser::Func_Node<T...>, Environment env )
			-> decltype( makeFunc( promote( typed_expr( T{}, env ) )... ) );

		template<class Ty, class T, class Environment>
		auto typed_expr( parser::Cast_Node<Ty, T>, Environment env )
			-> decltype( makeCast( Ty{}, typename ParseDeclare<Ty>::type{}, promote_void( typed_expr( T{}, env ) ) ) );

		// ------ Statement ------

		template<class AST, class NowEnvironment, class NowNest>
		struct StatementFold;

		template<class NowEnvironment, class NowNest>
		struct StatementFold<parser::detail::lr_stack_bottom, NowEnvironment, NowNest> {
			using TypedAST = type_tuple<>;
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
		};

		// --- DeclareStatement_Node ---
		template<class NowEnvironment, class NowNest>
		struct StatementFold<parser::Statement_Node<parser::DeclareStatement_Node<>>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
		};

		template<class T, class... Ts, class NowEnvironment, class NowNest>
		struct StatementFold<parser::Statement_Node<parser::DeclareStatement_Node<T, Ts...>>, NowEnvironment, NowNest> {
			using NextEnvironment = typename StatementFold<parser::Statement_Node<parser::DeclareStatement_Node<Ts...>>, decltype( add_environment( NowEnvironment{}, typename ParseDeclare<T>::name{}, typename ParseDeclare<T>::type{} ) ), NowNest>::NextEnvironment;
			using NextNest = NowNest;
			using TypedAST = parser::Statement_Node<parser::DeclareStatement_Node<T, Ts...>>;
		};

		// --- FunctionDefinition ---
		template<class T, class Ty, class St, class NowEnvironment, class NowNest>
		struct StatementFold<parser::FunctionDefinition<T, Ty, St>, NowEnvironment, NowNest> {
			using FuncName = typename ParseDeclare<parser::Declare<T, Ty>>::name;
			using FuncType = typename ParseDeclare<parser::Declare<T, Ty>>::type;
			using FuncRetType = typename ParseDeclare<parser::Declare<T, Ty>>::rettype;
			using NextEnvironment = decltype( add_environment( NowEnvironment{}, FuncName{}, FuncType{} ) );
			using FuncEnvironment = decltype( add_arg_environment( NextEnvironment{}, T{} ) );
			using NextNest = NowNest;
			using TypedAST = parser::FunctionDefinition<T, Ty, typename StatementFold<St, FuncEnvironment, decltype( intoFunc( FuncName{}, FuncRetType{}, NowNest{} ) )>::TypedAST>;
		};

		// --- NullStatement ---
		template<class NowEnvironment, class NowNest>
		struct StatementFold<parser::NullStatement, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
			using TypedAST = parser::NullStatement;
		};

		// --- BreakStatement ---
		template<std::nullptr_t N = nullptr>
		struct Error_Break_Statement_Not_In_Loop {
			static_assert( N != nullptr, "error: break statement can appear only in a for/while loop" );
		};

		template<class RetTy, class Name, std::size_t N, std::size_t... Ns, std::size_t... Ms, typename std::enable_if<sizeof...(Ns) != 0, std::nullptr_t>::type = nullptr>
		auto makeBreak( NestEnvironment<InFunction<RetTy>, LoopNestId<Name, N, Ns...>, IfNestId<void, Ms...>> )
			-> Labeled<LoopNestId<Name, Ns...>, parser::BreakStatement>;

		auto makeBreak( ... )
			-> Error_Break_Statement_Not_In_Loop<>;

		template<class NowEnvironment, class NowNest>
		struct StatementFold<parser::BreakStatement, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
			using TypedAST = decltype( makeBreak( NowNest{} ) );
		};

		// --- ContinueStatement ---
		template<std::nullptr_t N = nullptr>
		struct Error_Continue_Statement_Not_In_Loop {
			static_assert( N != nullptr, "error: continue statement can appear only in a for/while loop" );
		};

		template<class RetTy, class Name, std::size_t N, std::size_t... Ns, std::size_t... Ms, typename std::enable_if<sizeof...(Ns) != 0, std::nullptr_t>::type = nullptr>
		auto makeContinue( NestEnvironment<InFunction<RetTy>, LoopNestId<Name, N, Ns...>, IfNestId<void, Ms...>> )
			-> Labeled<LoopNestId<Name, Ns...>, parser::ContinueStatement>;

		auto makeContinue( ... )
			-> Error_Continue_Statement_Not_In_Loop<>;

		template<class NowEnvironment, class NowNest>
		struct StatementFold<parser::ContinueStatement, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
			using TypedAST = decltype( makeContinue( NowNest{} ) );
		};

		// --- Return Statement ---
		template<class T, class U, std::nullptr_t N = nullptr>
		struct Error_Return_Type_Not_Convertible {
			static_assert( N != nullptr, "type error: return type not convertible" );
		};

		template<class Ty, class AST, class RetTy, class Nest, class Nest2, typename Convertible<RetTy, Ty>::type = nullptr>
		auto makeReturn( Typed<Ty, AST, false>, NestEnvironment<InFunction<RetTy>, Nest, Nest2> )
			-> parser::ReturnStatement<Typed<Ty, AST, false>>;

		template<class T, class U>
		auto makeReturn( T, U )
			-> Error_Return_Type_Not_Convertible<T, U>;

		template<class T, class NowEnvironment, class NowNest>
		struct StatementFold<parser::ReturnStatement<T>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
			using TypedAST = decltype( makeReturn( promote( typed_expr( T{}, NowEnvironment{} ) ), NowNest{} ) );
		};

		template<class NowEnvironment, class NowNest>
		struct StatementFold<parser::ReturnVoidStatement, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
			using TypedAST = parser::ReturnVoidStatement;
		};


		// --- BraceStatement_Node ---
		template<class AST1, class... AST2>
		auto statement_merge( AST1, parser::Statement_Node<parser::BraceStatement_Node<AST2...>> )
			-> parser::Statement_Node<parser::BraceStatement_Node<AST1, AST2...>>;

		template<class NowEnvironment, class NowNest>
		struct StatementFold<parser::Statement_Node<parser::BraceStatement_Node<>>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
			using TypedAST = parser::Statement_Node<parser::BraceStatement_Node<>>;
		};

		template<class T, class... Ts, class NowEnvironment, class NowNest>
		struct StatementFold<parser::Statement_Node<parser::BraceStatement_Node<T, Ts...>>, NowEnvironment, NowNest> {
			using lhsResult = StatementFold<T, NowEnvironment, NowNest>;
			using TmpEnvironment = typename lhsResult::NextEnvironment;
			using TmpNest = typename lhsResult::NextNest;
			using rhsResult = StatementFold<parser::Statement_Node<parser::BraceStatement_Node<Ts...>>, TmpEnvironment, TmpNest>;
			using NextEnvironment = typename rhsResult::NextEnvironment;
			using NextNest = typename rhsResult::NextNest;
			using lhsAST = typename lhsResult::TypedAST;
			using rhsAST = typename rhsResult::TypedAST;
			using TypedAST = decltype( statement_merge( lhsAST{}, rhsAST{} ) );
		};

		// --- ExprStatement_Node ---
		template<class T, class NowEnvironment, class NowNest>
		struct StatementFold<parser::ExprStatement_Node<T>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = NowNest;
			using TypedAST = parser::ExprStatement_Node<decltype( typed_expr( T{}, NowEnvironment{} ) )>;
		};

		// --- IfStatement_Node ---
		template<class T, class U, class NowEnvironment, class NowNest>
		struct StatementFold<parser::IfStatement_Node<T, U>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = decltype( nextNest( NowNest{} ) );
			using TypedT = decltype( promote( typed_expr( T{}, NowEnvironment{} ) ) );
			using TypedU = typename StatementFold<U, NowEnvironment, decltype( intoThen( NowNest{} ) )>::TypedAST; // cannot declare in condition
			using TypedAST = Labeled<decltype( getEndLabel( NowNest{} ) ), parser::IfStatement_Node<TypedT, TypedU>>;

			NonVoidCondition<TypedT> check;
		};

		template<class T, class U, class V, class NowEnvironment, class NowNest>
		struct StatementFold<parser::IfStatement_Node<T, U, V>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = decltype( nextNest( NowNest{} ) );
			using TypedT = decltype( promote( typed_expr( T{}, NowEnvironment{} ) ) );
			using TypedU = typename StatementFold<U, NowEnvironment, decltype( intoThen( NowNest{} ) )>::TypedAST; // cannot declare in condition
			using TypedV = typename StatementFold<V, NowEnvironment, decltype( intoElse( NowNest{} ) )>::TypedAST;
			using TypedAST = Labeled<decltype( getEndLabel( NowNest{} ) ), parser::IfStatement_Node<TypedT, TypedU, TypedV>>;

			NonVoidCondition<TypedT> check;
		};

		// --- ForStatement_Node ---
		template<class T, class U, class V, class W, class NowEnvironment, class NowNest>
		struct StatementFold<parser::ForStatement_Node<T, U, V, W>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = decltype( nextNest( NowNest{} ) );
			using TypedT = decltype( promote( typed_expr( T{}, NowEnvironment{} ) ) );
			using TypedU = decltype( promote( typed_expr( U{}, NowEnvironment{} ) ) ); // cannot declare in initialize
			using TypedV = decltype( promote( typed_expr( V{}, NowEnvironment{} ) ) );
			using TypedW = typename StatementFold<W, NowEnvironment, decltype( addNest( NowNest{} ) )>::TypedAST;
			using TypedAST = Labeled<decltype( getEndLabel( NowNest{} ) ), parser::ForStatement_Node<TypedT, TypedU, TypedV, TypedW>>;

			NonVoidCondition<TypedU> check;
		};

		// --- WhileStatement_Node ---
		template<class T, class U, class NowEnvironment, class NowNest>
		struct StatementFold<parser::WhileStatement_Node<T, U>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = decltype( nextNest( NowNest{} ) );
			using TypedT = decltype( promote( typed_expr( T{}, NowEnvironment{} ) ) );
			using TypedU = typename StatementFold<U, NowEnvironment, decltype( addNest( NowNest{} ) )>::TypedAST;
			using TypedAST = Labeled<decltype( getEndLabel( NowNest{} ) ), parser::WhileStatement_Node<TypedT, TypedU>>;

			NonVoidCondition<TypedT> check;
		};

		// --- DoStatement_Node ---
		template<class T, class U, class NowEnvironment, class NowNest>
		struct StatementFold<parser::DoStatement_Node<T, U>, NowEnvironment, NowNest> {
			using NextEnvironment = NowEnvironment;
			using NextNest = decltype( nextNest( NowNest{} ) );
			using TypedT = typename StatementFold<T, NowEnvironment, decltype( addNest( NowNest{} ) )>::TypedAST;
			using TypedU = decltype( promote( typed_expr( U{}, NowEnvironment{} ) ) );
			using TypedAST = Labeled<decltype( getEndLabel( NowNest{} ) ), parser::DoStatement_Node<TypedT, TypedU>>;

			NonVoidCondition<TypedU> check;
		};

		// --- Fold ---
		using NullNestEnvironment = NestEnvironment<InGlobal>;

		template<class AST, class NowEnvironment>
		struct Fold;

		template<class AST, class NowEnvironment>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, AST>, NowEnvironment> : StatementFold<AST, NowEnvironment, NullNestEnvironment> {};

		template<lfl::index_t... Index, class... AST, class NowEnvironment>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<Index...>, AST...>, NowEnvironment> {
			using tuple = lfl::flat_tuple<lfl::index_tuple<Index...>, AST...>;
			using tailResult = Fold<decltype(tail(tuple{})), NowEnvironment>;
			using headResult = Fold<decltype(head(tuple{})), typename tailResult::NextEnvironment>;
			using TypedAST = decltype( merge( typename headResult::TypedAST{}, typename tailResult::TypedAST{} ) );
			using NextEnvironment = typename headResult::NextEnvironment;
		};


		using NullTypeEnvironment = TypeEnvironment<IdentifierList<>, TypeList<>>;

		template<class... AST>
		using Result = typename Fold<lfl::flat_tuple<lfl::make_index_tuple<sizeof...(AST)>, AST...>, NullTypeEnvironment>::TypedAST;

	} // namespace detail

	template<class... AST>
	using Result = typename type_analysis::detail::Result<AST...>;

	template<class... AST>
	using detail_NextEnvironment = typename type_analysis::detail::Fold<lfl::flat_tuple<lfl::make_index_tuple<sizeof...(AST)>, AST...>, detail::NullTypeEnvironment>::NextEnvironment;

} // namespace type_analysis

#endif // LTMPC_TYPEANALYSIS_HPP
