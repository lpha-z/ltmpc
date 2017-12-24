#ifndef LTMPC_CODEEMITTER_HPP
#define LTMPC_CODEEMITTER_HPP
#include "Location.hpp"

namespace code_emitter {

	template<char...>
	struct Code {};

	// ADL
	auto concat()
		-> Code<>;

	template<char... Chars>
	auto concat( Code<Chars...> )
		-> Code<Chars...>;

	template<char... C1, char... C2, class... Ts>
	auto concat( Code<C1...>, Code<C2...>, Ts... )
		-> decltype( concat( Code<C1..., C2...>{}, Ts{}... ) );

	namespace detail {

		struct Intro : Code<'\t','.','i','n','t','e','l','_','s','y','n','t','a','x',' ','n','o','p','r','e','f','i','x','\n','\n'> {};

		template<unsigned long long N, bool B = N < 10>
		struct ToString;

		template<unsigned long long N>
		struct ToString<N, true> : Code<'0'+N> {};

		template<unsigned long long N>
		struct ToString<N, false> : decltype( concat( ToString<N/10>{}, Code<'0'+N%10>{} ) ) {};

		template<std::ptrdiff_t N, bool B = N < 0>
		struct ToOffset;

		template<std::ptrdiff_t N>
		struct ToOffset<N, true> : decltype( concat( Code<'-'>{}, ToString<static_cast<unsigned long long>(-N)>{} ) ) {};

		template<std::ptrdiff_t N>
		struct ToOffset<N, false> : decltype( concat( Code<'+'>{}, ToString<static_cast<unsigned long long>(N)>{} ) ) {};

		template<std::size_t N>
		struct StringRef : decltype( concat( Code<'O','F','F','S','E','T',' ','F','L','A','T',':','.','L','C'>{}, ToString<static_cast<unsigned long long>(N)>{} ) ) {};

		template<char... Chars>
		struct FuncRef : Code<'O','F','F','S','E','T',' ','F','L','A','T',':', Chars...> {};

		struct EAX : Code<'e','a','x'> {};
		struct ECX : Code<'e','c','x'> {};
		struct EDX : Code<'e','d','x'> {};
		struct EBP : Code<'e','b','p'> {};
		struct ESP : Code<'e','s','p'> {};
		struct  AL : Code<' ','a','l'> {};

		template<class BaseReg, std::ptrdiff_t N>
		struct Address : decltype( concat( Code<'['>{}, BaseReg{}, ToOffset<N>{}, Code<']'>{} ) ) {};

		template<class Addr>
		struct DWord : decltype( concat( Code<'D','W','O','R','D',' ','P','T','R',' '>{}, Addr{} ) ) {};

		template<class Addr>
		struct Byte : decltype( concat( Code<'B','Y','T','E',' ','P','T','R',' '>{}, Addr{} ) ) {};

		template<unsigned long long N>
		struct Immediate : ToString<N> {};

		template<std::size_t N>
		auto toImm( location::detail::Size<N> )
			-> Immediate<static_cast<unsigned long long>(N)>;

		template<class>
		struct LabelString;
		template<char... FuncName>
		struct LabelString<type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>>>> : Code<'.','L','F','_', FuncName...> {};
		template<char... FuncName, std::size_t N, std::size_t... Ns>
		struct LabelString<type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>, N, Ns...>>>
			: decltype( concat( LabelString<type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>, Ns...>>>{}, Code<'_'>{}, ToString<static_cast<unsigned long long>(N)>{} ) ) {};
		template<char... FuncName, std::size_t... Ns, std::size_t M, std::size_t... Ms>
		struct LabelString<type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>, Ns...>, M, Ms...>>
			: decltype( concat( LabelString<type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>, Ns...>, Ms...>>{}, Code<'_',(M?'e':'t')>{} ) ) {};

		template<char... FuncName, std::size_t... Ns, std::size_t... Ms>
		auto makeElseLabel( type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>, Ns...>, Ms...> label )
			-> decltype( concat( LabelString<decltype(label)>{}, Code<'_','E'>{} ) );

		template<char... FuncName, std::size_t... Ns, std::size_t... Ms>
		auto makeContinueLabel( type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>, Ns...>, Ms...> label )
			-> decltype( concat( LabelString<decltype(label)>{}, Code<'_','C'>{} ) );

		template<char... FuncName, std::size_t... Ns, std::size_t... Ms>
		auto makeBreakLabel( type_analysis::IfNestId<type_analysis::LoopNestId<type_analysis::Identifier<FuncName...>, Ns...>, Ms...> label )
			-> decltype( concat( LabelString<decltype(label)>{}, Code<'_','B'>{} ) );

		template<class T>
		struct Prace : decltype( concat( T{}, Code<':','\n'>{} ) ) {};

		template<class T, char... Chars>
		struct Mnemonic1 : decltype( concat( Code<'\t',Chars...,'\t'>{}, T{}, Code<'\n'>{} ) ) {};

		template<class T, class U, char... Chars>
		struct Mnemonic2 : decltype( concat( Code<'\t',Chars...,'\t'>{}, T{}, Code<',',' '>{}, U{}, Code<'\n'>{} ) ) {};

		template<class T>
		struct Push : Mnemonic1<T, 'p','u','s','h'> {};

		template<class T>
		struct Pop : Mnemonic1<T, 'p','o','p'> {};

		template<class T, class U>
		struct Mov : Mnemonic2<T, U, 'm','o','v'> {};

		template<class T, class U>
		struct Lea : Mnemonic2<T, U, 'l','e','a'> {};

		struct Ret : Code<'\t','r','e','t','\n'> {};

		template<class T, class U>
		struct Test : Mnemonic2<T, U, 't','e','s','t'> {};

		template<class T, class U>
		struct Add : Mnemonic2<T, U, 'a','d','d'> {};

		template<class T, class U>
		struct Sub : Mnemonic2<T, U, 's','u','b'> {};

		template<class T, class U>
		struct Mul : Mnemonic2<T, U, 'i','m','u','l'> {};

		template<class T, class U>
		struct Div : Mnemonic2<T, U, 'i','d','i','v'> {};

		template<class T, class U>
		struct Movsx : Mnemonic2<T, U, 'm','o','v','s','x'> {};

		template<class T, class U>
		struct Cmp : Mnemonic2<T, U, 'c','m','p'> {};

		template<class T, T Op>
		struct SetCC;
		template<> struct SetCC<parser::CompareOpType, parser::CompareOpType::LessThan>          : Mnemonic1<AL, 's','e','t','l'>     {};
		template<> struct SetCC<parser::CompareOpType, parser::CompareOpType::EqualOrLessThan>   : Mnemonic1<AL, 's','e','t','l','e'> {};
		template<> struct SetCC<parser::CompareOpType, parser::CompareOpType::GraterThan>        : Mnemonic1<AL, 's','e','t','g'>     {};
		template<> struct SetCC<parser::CompareOpType, parser::CompareOpType::EqualOrGraterThan> : Mnemonic1<AL, 's','e','t','g','e'> {};
		template<> struct SetCC<parser::EqCompOpType , parser::EqCompOpType ::Equal>             : Mnemonic1<AL, 's','e','t','e'>     {};
		template<> struct SetCC<parser::EqCompOpType , parser::EqCompOpType ::NotEqual>          : Mnemonic1<AL, 's','e','t','n','e'> {};

		template<class Label>
		struct Jz : Mnemonic1<Label, 'j','z'> {};

		template<class Label>
		struct Jmp : Mnemonic1<Label, 'j','m','p'> {};

		template<class T>
		struct Call : Mnemonic1<T, 'c','a','l','l'> {};

		struct Prolog : decltype( concat( Push<EBP>{}, Mov<EBP, ESP>{} ) ) {};

		// ------ Compile Expr ------

		template<class>
		struct Compile
		{
			// dummy
			using EmitCode = Code<'n','o','t',' ','i','m','p','l','i','m','e','n','t','e','d','\n'>;
		};

		template<char... Chars>
		struct Compile<parser::IntLiteral<Chars...>> {
			using EmitCode = Push<Immediate<parser::IntLiteral<Chars...>::value>>;
		};

		template<char... Chars>
		struct Compile<parser::CharLiteral<Chars...>> {
			using EmitCode = Push<Immediate<parser::CharLiteral<Chars...>::value>>;
		};

		//  --- add ---
		template<class T, class U>
		struct Compile<parser::Plus_Node<parser::PlusOpType::Plus, type_analysis::Typed<type_analysis::type_int, T, false>, type_analysis::Typed<type_analysis::type_int, U, false>>> {
			using lhsProgram = typename Compile<T>::EmitCode;
			using rhsProgram = typename Compile<U>::EmitCode;
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EDX>{}, Pop<EAX>{}, Add<EAX, EDX>{}, Push<EAX>{} ) );
		};

		template<class Ty, class T, class U>
		struct Compile<parser::Plus_Node<parser::PlusOpType::Plus, type_analysis::Typed<type_analysis::PtrTo<Ty>, T, false>, type_analysis::Typed<type_analysis::type_int, U, false>>> {
			using lhsProgram = typename Compile<T>::EmitCode; // addr: eax
			using rhsProgram = typename Compile<U>::EmitCode; // index: edx
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) );
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EDX>{}, Pop<EAX>{}, Mul<EDX, PtrSize>{}, Add<EAX, EDX>{}, Push<EAX>{} ) );
		};

		template<class Ty, class T, class U>
		struct Compile<parser::Plus_Node<parser::PlusOpType::Plus, type_analysis::Typed<type_analysis::type_int, T, false>, type_analysis::Typed<type_analysis::PtrTo<Ty>, U, false>>> {
			using lhsProgram = typename Compile<T>::EmitCode; // index: edx
			using rhsProgram = typename Compile<U>::EmitCode; // addr: eax
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) );
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EAX>{}, Pop<EDX>{}, Mul<EDX, PtrSize>{}, Add<EAX, EDX>{}, Push<EAX>{} ) );
		};

		// --- sub ---
		template<class T, class U>
		struct Compile<parser::Plus_Node<parser::PlusOpType::Minus, type_analysis::Typed<type_analysis::type_int, T, false>, type_analysis::Typed<type_analysis::type_int, U, false>>> {
			using lhsProgram = typename Compile<T>::EmitCode;
			using rhsProgram = typename Compile<U>::EmitCode;
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EDX>{}, Pop<EAX>{}, Sub<EAX, EDX>{}, Push<EAX>{} ) );
		};

		template<class Ty, class T, class U>
		struct Compile<parser::Plus_Node<parser::PlusOpType::Minus, type_analysis::Typed<type_analysis::PtrTo<Ty>, T, false>, type_analysis::Typed<type_analysis::type_int, U, false>>> {
			using lhsProgram = typename Compile<T>::EmitCode; // addr: eax
			using rhsProgram = typename Compile<U>::EmitCode; // index: edx
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) );
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EDX>{}, Pop<EAX>{}, Mul<EDX, PtrSize>{}, Sub<EAX, EDX>{}, Push<EAX>{} ) );
		};

		template<class Ty, class T, class U>
		struct Compile<parser::Plus_Node<parser::PlusOpType::Minus, type_analysis::Typed<type_analysis::PtrTo<Ty>, T, false>, type_analysis::Typed<type_analysis::PtrTo<Ty>, U, false>>> {
			using lhsProgram = typename Compile<T>::EmitCode;
			using rhsProgram = typename Compile<U>::EmitCode;
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) );
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EDX>{}, Pop<EAX>{}, Sub<EAX, EDX>{}, Div<EAX, PtrSize>{}, Push<EAX>{} ) );
		};

		// --- compare ---
		template<parser::CompareOpType Op, class T, class U>
		struct Compile<parser::Compare_Node<Op, T, U>> {
			using lhsProgram = typename Compile<T>::EmitCode;
			using rhsProgram = typename Compile<U>::EmitCode;
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EDX>{}, Pop<EAX>{}, Cmp<EAX, EDX>{}, SetCC<parser::CompareOpType, Op>{}, Movsx<EAX, AL>{}, Push<EAX>{} ) );
		};

		// --- eq comp ---
		template<parser::EqCompOpType Op, class T, class U>
		struct Compile<parser::EqComp_Node<Op, T, U>> {
			using lhsProgram = typename Compile<T>::EmitCode;
			using rhsProgram = typename Compile<U>::EmitCode;
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EDX>{}, Pop<EAX>{}, Cmp<EAX, EDX>{}, SetCC<parser::EqCompOpType, Op>{}, Movsx<EAX, AL>{}, Push<EAX>{} ) );
		};

		// --- normal assign ---
		template<class Ty, class T, class U>
		struct Compile<parser::Assign_Node<parser::AssignOpType::NormalAssign, type_analysis::Typed<Ty, T, true>, U>> {
			using lhsProgram = typename Compile<T>::Addr;
			using rhsProgram = typename Compile<U>::EmitCode;
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EAX>{}, Pop<EDX>{}, Mov<DWord<Address<EDX, 0>>, EAX>{}, Push<EAX>{} ) );
		};

		template<class T, class U>
		struct Compile<parser::Assign_Node<parser::AssignOpType::NormalAssign, type_analysis::Typed<type_analysis::type_char, T, true>, U>> {
			using lhsProgram = typename Compile<T>::Addr;
			using rhsProgram = typename Compile<U>::EmitCode;
			using EmitCode = decltype( concat( lhsProgram{}, rhsProgram{}, Pop<EAX>{}, Pop<EDX>{}, Mov<Byte<Address<EDX, 0>>, AL>{}, Push<EAX>{} ) );
		};

		// --- post increment/decrement ---
		template<template<class, class>class Op, class T, class Reg, class Imm>
		struct CompilePostIncDec {
			using ProgramT = typename Compile<T>::Addr;
			using EmitCode = decltype( concat( ProgramT{}, Pop<EDX>{}, Mov<Reg, Address<EDX, 0>>{}, Push<EAX>{}, Op<Reg, Imm>{}, Mov<Address<EDX, 0>, Reg>{} ) );
		};

		template<class Ty, class T>
		struct Compile<parser::SuffixUnary_Node<parser::SuffixUnaryOpType::Increment, type_analysis::Typed<type_analysis::PtrTo<Ty>, T, true>>>
			: CompilePostIncDec<Add, type_analysis::Typed<type_analysis::PtrTo<Ty>, T, true>, EAX, decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) )> {};

		template<class T>
		struct Compile<parser::SuffixUnary_Node<parser::SuffixUnaryOpType::Increment, type_analysis::Typed<type_analysis::type_int, T, true>>>
			: CompilePostIncDec<Add, type_analysis::Typed<type_analysis::type_int, T, true>, EAX, Immediate<1>> {};

		template<class T>
		struct Compile<parser::SuffixUnary_Node<parser::SuffixUnaryOpType::Increment, type_analysis::Typed<type_analysis::type_char, T, true>>>
			: CompilePostIncDec<Add, type_analysis::Typed<type_analysis::type_char, T, true>, AL, Immediate<1>> {};

		template<class Ty, class T>
		struct Compile<parser::SuffixUnary_Node<parser::SuffixUnaryOpType::Decrement, type_analysis::Typed<type_analysis::PtrTo<Ty>, T, true>>>
			: CompilePostIncDec<Sub, type_analysis::Typed<type_analysis::PtrTo<Ty>, T, true>, EAX, decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) )> {};

		template<class T>
		struct Compile<parser::SuffixUnary_Node<parser::SuffixUnaryOpType::Decrement, type_analysis::Typed<type_analysis::type_int, T, true>>>
			: CompilePostIncDec<Sub, type_analysis::Typed<type_analysis::type_int, T, true>, EAX, Immediate<1>> {};

		template<class T>
		struct Compile<parser::SuffixUnary_Node<parser::SuffixUnaryOpType::Decrement, type_analysis::Typed<type_analysis::type_char, T, true>>>
			: CompilePostIncDec<Sub, type_analysis::Typed<type_analysis::type_char, T, true>, AL, Immediate<1>> {};

		// --- subscript ---
		template<class Ty, class T, class U>
		struct Compile<parser::Subscript_Node<type_analysis::Typed<type_analysis::PtrTo<Ty>, T, false>, type_analysis::Typed<type_analysis::type_int, U, false>>> {
			using ProgramT = typename Compile<T>::EmitCode;
			using ProgramU = typename Compile<U>::EmitCode;
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) );
			using Addr = decltype( concat( ProgramT{}, ProgramU{}, Pop<EDX>{}, Pop<EAX>{}, Mul<EDX, PtrSize>{}, Add<EAX, EDX>{}, Push<EAX>{} ) );
			// TODO: Ty = char
			using EmitCode = decltype( concat( Addr{}, Pop<EAX>{}, Mov<EAX, DWord<Address<EAX, 0>>>{}, Push<EAX>{} ) );
		};

		template<class Ty, class T, class U>
		struct Compile<parser::Subscript_Node<type_analysis::Typed<type_analysis::type_int, T, false>, type_analysis::Typed<type_analysis::PtrTo<Ty>, U, false>>> {
			using ProgramT = typename Compile<T>::EmitCode;
			using ProgramU = typename Compile<U>::EmitCode;
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<Ty>::size{} ) );
			using Addr = decltype( concat( ProgramT{}, ProgramU{}, Pop<EAX>{}, Pop<EDX>{}, Mul<EDX, PtrSize>{}, Add<EAX, EDX>{}, Push<EAX>{} ) );
			// TODO: Ty = char
			using EmitCode = decltype( concat( Addr{}, Pop<EAX>{}, Mov<EAX, DWord<Address<EAX, 0>>>{}, Push<EAX>{} ) );
		};

		template<class Ty, std::size_t Sz, class T, class U>
		struct Compile<parser::Subscript_Node<type_analysis::Typed<type_analysis::PtrTo<type_analysis::ArrayOf<Ty, Sz>>, T, false>, type_analysis::Typed<type_analysis::type_int, U, false>>> {
			using ProgramT = typename Compile<T>::EmitCode;
			using ProgramU = typename Compile<U>::EmitCode;
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<type_analysis::ArrayOf<Ty, Sz>>::size{} ) );
			using Addr = decltype( concat( ProgramT{}, ProgramU{}, Pop<EDX>{}, Pop<EAX>{}, Mul<EDX, PtrSize>{}, Add<EAX, EDX>{}, Push<EAX>{} ) );
			using EmitCode = Addr;
		};

		template<class Ty, std::size_t Sz, class T, class U>
		struct Compile<parser::Subscript_Node<type_analysis::Typed<type_analysis::type_int, T, false>, type_analysis::Typed<type_analysis::PtrTo<type_analysis::ArrayOf<Ty, Sz>>, U, false>>> {
			using ProgramT = typename Compile<T>::EmitCode;
			using ProgramU = typename Compile<U>::EmitCode;
			using PtrSize = decltype( toImm( typename location::detail::ParseSize<type_analysis::ArrayOf<Ty, Sz>>::size{} ) );
			using Addr = decltype( concat( ProgramT{}, ProgramU{}, Pop<EAX>{}, Pop<EDX>{}, Mul<EDX, PtrSize>{}, Add<EAX, EDX>{}, Push<EAX>{} ) );
			using EmitCode = Addr;
		};

		// --- function call ---

		template<class...>
		struct ArgPush;
		template<>
		struct ArgPush<> {
			using EmitCode = Code<>;
		};
		template<class T, class... Ts>
		struct ArgPush<T, Ts...> {
			using Program = typename Compile<T>::EmitCode;
			using EmitCode = decltype( concat( typename ArgPush<Ts...>::EmitCode{}, Program{} ) );
		};

		template<class T, class... Args>
		struct Compile<parser::Func_Node<T, Args...>> {
			using ArgPushProgram = typename ArgPush<Args...>::EmitCode;
			using FuncPushProgram = typename Compile<T>::EmitCode;
			using StackUnwind = Add<ESP, Immediate<static_cast<unsigned long long>(sizeof...(Args)*4)>>;
			using EmitCode = decltype( concat( ArgPushProgram{}, FuncPushProgram{}, Pop<EAX>{}, Call<EAX>{}, StackUnwind{}, Push<EAX>{} ) );
		};

		// --- other ---

		template<class T>
		struct Compile<parser::Term_Node<T>> {
			using EmitCode = typename Compile<T>::EmitCode;
		};

		template<std::ptrdiff_t N>
		struct Compile<location::FramePosition<N>> {
			using Addr = decltype( concat( Lea<EAX, Address<EBP, N>>{}, Push<EAX>{} ) );
			using EmitCode = Push<DWord<Address<EBP, N>>>;
		};

		template<std::ptrdiff_t N>
		struct Compile<type_analysis::Typed<type_analysis::type_char, location::FramePosition<N>, false>> {
			using EmitCode = decltype( concat( Mov<AL, Byte<Address<EBP, N>>>{}, Push<EAX>{} ) );
		};

		template<class T>
		struct Compile<type_analysis::IntegralPromotion<T>> {
			using Program = typename Compile<T>::EmitCode;
			using EmitCode = decltype( concat( Program{}, Pop<EAX>{}, Movsx<EAX, AL>{}, Push<EAX>{} ) );
		};

		template<class T>
		struct Compile<type_analysis::ArrayDecay<T>> {
			using EmitCode = typename Compile<T>::Addr;
		};

		template<std::size_t N, std::size_t M>
		struct Compile<type_analysis::ArrayDecay<type_analysis::Typed<type_analysis::ArrayOf<type_analysis::type_char, N>, location::StringRef<M>, false>>> {
			using EmitCode = Push<StringRef<M>>;
		};

		template<class Ty, std::ptrdiff_t N>
		struct Compile<type_analysis::Typed<Ty, location::FramePosition<N>, true>> {
			using Addr = decltype( concat( Lea<EAX, Address<EBP, N>>{}, Push<EAX>{} ) );
		};

		template<class Ty, class T>
		struct Compile<type_analysis::Typed<Ty, T, true>> {
			using Addr = typename Compile<T>::Addr;
			using EmitCode = typename Compile<T>::EmitCode;
		};

		template<class T, class... Args, char... Chars>
		struct Compile<type_analysis::Typed<type_analysis::PtrTo<type_analysis::Function<T, Args...>>, type_analysis::Identifier<Chars...>, true>> {
			using EmitCode = Push<FuncRef<Chars...>>;
		};

		template<class Ty, class T>
		struct Compile<type_analysis::Typed<Ty, T, false>> {
			using EmitCode = typename Compile<T>::EmitCode;
		};

		// ------ StatementFold ------

		template<class>
		struct StatementFold
		{
			// dummy
			using EmitCode = Code<'n','o','t',' ','i','m','p','l','i','m','e','n','t','e','d','\n'>;
		};

		template<>
		struct StatementFold<parser::NullStatement> {
			using EmitCode = Code<>;
		};

		// --- SetSP ---
		template<std::ptrdiff_t N>
		struct StatementFold<location::SetSP_Node<location::FramePosition<N>>> {
			using EmitCode = Lea<ESP, Address<EBP, N>>;
		};

		// --- Return ---
		template<>
		struct StatementFold<location::Return_Node> {
			using EmitCode = decltype( concat( Pop<EBP>{}, Ret{} ) );
		};

		template<class T>
		struct StatementFold<parser::ReturnStatement<T>> {
			using Program = typename Compile<T>::EmitCode;
			using EmitCode = decltype( concat( Program{}, Pop<EAX>{}, Mov<ESP, EBP>{}, Pop<EBP>{}, Ret{} ) );
		};

		// --- ExprStatement ---
		template<class T>
		struct StatementFold<parser::ExprStatement_Node<T>> {
			using Program = typename Compile<T>::EmitCode;
			using EmitCode = decltype( concat( Program{}, Pop<EAX>{} ) );
		};

		// --- BraceStatement ---
		template<class... Ts>
		struct StatementFold<parser::Statement_Node<parser::BraceStatement_Node<Ts...>>> {
			using EmitCode = decltype( concat( typename StatementFold<Ts>::EmitCode{}... ) );
		};

		// --- IfStatement ---
		template<class Label, class T, class U>
		struct StatementFold<type_analysis::Labeled<Label, parser::IfStatement_Node<T, U>>> {
			using ProgramT = typename Compile<T>::EmitCode;
			using ProgramU = typename StatementFold<U>::EmitCode;
			using EmitCode = decltype( concat( ProgramT{}, Pop<EAX>{}, Test<EAX, EAX>{}, Jz<LabelString<Label>>{}, ProgramU{}, Prace<LabelString<Label>>{} ) );
		};

		template<class Label, class T, class U, class V>
		struct StatementFold<type_analysis::Labeled<Label, parser::IfStatement_Node<T, U, V>>> {
			using ProgramT = typename Compile<T>::EmitCode;
			using ProgramU = typename StatementFold<U>::EmitCode;
			using ProgramV = typename StatementFold<V>::EmitCode;
			using ElseLabel = decltype( makeElseLabel( Label{} ) );
			using EmitCode = decltype( concat( ProgramT{}, Pop<EAX>{}, Test<EAX, EAX>{}, Jz<ElseLabel>{}, ProgramU{}, Jmp<LabelString<Label>>{}, Prace<ElseLabel>{}, ProgramV{}, Prace<LabelString<Label>>{} ) );
		};

		// --- ForStatement ---
		template<class Label, class T, class U, class V, class W>
		struct StatementFold<type_analysis::Labeled<Label, parser::ForStatement_Node<T, U, V, W>>> {
			using ProgramT = typename Compile<T>::EmitCode;
			using ProgramU = typename Compile<U>::EmitCode;
			using ProgramV = typename Compile<V>::EmitCode;
			using ProgramW = typename StatementFold<W>::EmitCode;

			using ContinueLabel = decltype( makeContinueLabel( Label{} ) );
			using BreakLabel = decltype( makeBreakLabel( Label{} ) );
			using Initialize = decltype( concat( ProgramT{}, Pop<EAX>{} ) );
			using Condition = decltype( concat( ProgramU{}, Pop<EAX>{}, Test<EAX, EAX>{}, Jz<BreakLabel>{} ) );
			using LoopInner = decltype( concat( ProgramW{}, ProgramV{}, Pop<EAX>{}, Jmp<ContinueLabel>{} ) );

			using EmitCode = decltype( concat( Initialize{}, Prace<ContinueLabel>{}, Condition{}, LoopInner{}, Prace<BreakLabel>{} ) );
		};

		// --- StringPoolFold ---

		template<class, std::size_t>
		struct StringPoolFold;

		template<char... Chars, std::size_t N>
		struct StringPoolFold<location::String<Chars...>, N> {
			using Prefix1 = Code<'.','L','C'>;
			using Prefix2 = Code<'.','s','t','r','i','n','g','\t'>;
			using EmitCode = decltype( concat( Prefix1{}, ToString<static_cast<unsigned long long>(N)>{}, Code<':','\t'>{}, Prefix2{}, Code<'\"', Chars..., '\"'>{}, Code<'\n'>{} ) );
		};

		// ------ GlobalStatementFold ------
		template<class>
		struct GlobalStatementFold;

		// --- StringPool ---

		template<class... Strings, lfl::index_t... Index>
		auto makeStringPoolFold( lfl::index_tuple<Index...>, Strings... )
			-> decltype( concat( typename StringPoolFold<Strings, Index>::EmitCode{}..., Code<'\n'>{} ) );

		template<class... Strings>
		struct GlobalStatementFold<location::StringPool<Strings...>> {
			using EmitCode = decltype( makeStringPoolFold( lfl::make_index_tuple<sizeof...(Strings)>{}, Strings{}... ) );
		};

		// --- GlobalVariable ---
		template<char... Chars, std::size_t Size, std::size_t Align>
		struct GlobalStatementFold<location::GlobalVariable<type_analysis::Identifier<Chars...>, Size, Align>> {
			using Prefix = Code<'\t','.','c','o','m','m','\t'>;
			using EmitCode = decltype( concat( Prefix{}, Code<Chars...>{}, Code<','>{}, ToString<static_cast<unsigned long long>(Size)>{}, Code<','>{}, ToString<static_cast<unsigned long long>(Align)>{}, Code<'\n'>{} ) );
		};

		// --- Function ---
		template<char... Chars, class Statement>
		struct GlobalStatementFold<location::Function<type_analysis::Identifier<Chars...>, Statement>> {
			using Prefix1 = Code<'\t','.','g','l','o','b','l','\t'>;
			using Prefix2 = Code<'\t','.','t','y','p','e','\t'>;
			using Type = Code<',',' ','@','f','u','n','c','t','i','o','n','\n'>;
			using Program = decltype( concat( Prolog{}, typename StatementFold<Statement>::EmitCode{} ) );
			using EmitCode = decltype( concat( Code<'\n'>{}, Prefix1{}, Code<Chars...>{}, Code<'\n'>{}, Prefix2{}, Code<Chars...>{}, Type{}, Code<Chars...>{}, Code<':','\n'>{}, Program{} ) );
		};

		// ------ Fold ------
		template<class>
		struct Fold;

		template<class AST>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<0>, AST>> : GlobalStatementFold<AST> {};

		template<lfl::index_t... Index, class... AST>
		struct Fold<lfl::flat_tuple<lfl::index_tuple<Index...>, AST...>> {
			using tuple = lfl::flat_tuple<lfl::index_tuple<Index...>, AST...>;
			using headCode = typename Fold<decltype(head(tuple{}))>::EmitCode;
			using tailCode = typename Fold<decltype(tail(tuple{}))>::EmitCode;
			using EmitCode = decltype( concat( headCode{}, tailCode{} ) );
		};

		template<class... AST>
		using Result = decltype( concat( Intro{}, typename Fold<lfl::flat_tuple<lfl::make_index_tuple<sizeof...(AST)>, AST...>>::EmitCode{} ) );

	} // namespace detail

	template<class... AST>
	using Result = typename code_emitter::detail::Result<AST...>;

} // namespace code_emitter

#endif // LTMPC_CODEEMITTER_HPP
