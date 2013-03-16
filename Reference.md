# Hash Ver 0.01 Reference

	      ###   ###              ___   
	     ###   ###              /  /   
	  #############            /  /___ 
	   ###   ###   ___________/  ___  \
	############# / __     _____/  /  /
	 ###   ###   / /__\ /____  /  /  / 
	###   ###    \_____/______/__/  /  

## Binaries in Directory "Code"

* main.exe : Just interpret into C++11 code. It uses standard input/output. You can use it like
<pre>main &lt;a.hash</pre>

* Compile.exe : Compile with g++. You can use it like
<pre>compile a.hash -oa.exe</pre>


## Point

* The grammar of Hash is quite similar to C++.
* Hash may generate a C++ code with errors. See the output of a C++ compiler.
* Hash automatically writes #line preprocessor directives to match the lines of original Hash source codes.


## Comment

	/*a*/ ===> //a
	/*a/*b*/c*/ ===>
		//a
		////b
		//c
	/*/ ===> (nothing)
	//a


## Name

	a
	n::a
	int
	true
	return ===> returnNotReservedWord
	@a

* Hash doesn't have any reserved words, but *contextual keywords*.
* You can use a C++ keyword to define something, and the name will be automatically changed to avoid crashing.
* Please note that C++ keywords that refer to type names (such as int) and data (such as true) won't be automatically changed.
  Hash sees these names as *automatically declared names*, not as contextual keywords.
* You can add "@" to avoid clashing with Hash's contextual keywords. Don't insert any whitespace between "@" and name.


## Literal

	123
	'x'
	"foo"


## Type

	A
	var ===> auto
	val ===> const auto
	ref ===> auto&
	rref ===> auto&&
	decltype
	const A
	constexpr A
	mutable A
	typename A
	A*
	A&
	A&&
	A@ ===> const A
	A->B ===> hash::function<A,B>
	(A,B) ===> hash::tuple<A,B>
	[A] ===> hash::forward_list<A>


## Expression

	x
	if(x) y else z ===> x?y:z
	\(A x,B y){} ===> [&](A x,B y){}
	\(A x,B y)->C{} ===> [&](A x,B y)->C{}
	{return x;} ===> [&](){return x;}
	{x;} ===> [&](){x;return hash::unit;}
	(x,y) ===> hash::make_tuple(x,y)
	() ===> hash::unit
	[x,y] ===> hash::make_forward_list(x,y)

* Blocks can be treated as expressions. If a block has no return statement, Hash automatically adds "return hash::unit", because C++ can't have a value with a type "void".
* Hash uses an if-else expression instead of C++'s trinary operator (a?b:c).


## Statement

	x;
	{x;}
	if(x){}
	if(x){}else{}
	for(x;y;z){}
	foreach(int x in xs){}
	while(x){}
	until(x){} ===> while(!x){}
	dowhile(x){} ===> do{}while(x)
	dountil(x){} ===> do{}while(!x)
	return x;
	return;
	continue;
	break;


## Variant

	extern A x,y;
	A x=0,y;
	static A x=0;
	constexpr A x=0;
	A* x,y; ===> A *x, *y;


## Function

	R f(A x,B y);
	R f(A x,B y){}
	R f<T,int x>(){} ===> template<typename T,int x> R f(){}
	R f(A x,B y)=x; ===> R f(A x,B y){return x;}
	R f()const{}
	static R f(){}
	virtual R f(){}
	constexpr R f(){}


## Pattern

	a ---- Name
	10 ---- Literal
	null ---- Null
	=(10+20) ---- Expression
	_ ---- Wildcard
	[1,2,3,_*] ---- Wildcard Others (0 or more elements)
	(a) ---- Parenthesized
	(1,2,3) ---- Tuple
	[1,2,3] ---- List
	[|1,2,3|] ---- Array
	Cons{1,[]} ---- Data Constructor
	{data=(a,b), height=10} ---- Record
	:? T ---- Type Test
	a@(1,2,3) ---- As
	(1,2,3) as a ---- As
	(a,b) & (_,"good") ---- And
	(a,b) | (_,"good") ---- Or
	!(_,_) ---- Not
	(a,b) when a+b=10 ---- When
	(a,b) unless a+b=10 ---- Unless


## Pattern Match

	match x with case (a,b) -> a+b case _ -> 0 ---- Match Expression
	match x with case (a,b) -> {return a+b;} case _ -> {return 0;} ---- Match Statement


## Enum

	enum A{x=0,y} ===> enum A{x=0,y};
	enum class A{} ===> enum class A{};
	bitenum A{x,y,z} ===> enum A{x=1,y=2,z=4};
	bitenum class A{}
	enum A;
	enum class A;
	bitenum A;
	bitenum class A;


## Class/Struct

	class A{} ===> class A{typedef A thistype;};
	class A{thistype(){}} ===> class A{...A(){}};
	class A{public int x;} ===> class A{...public: int x;};
	class A<T,int x>{} ===> template<typename T,int x> class A{...};
	class A where c {} ===> class A{...static_assert(...);};
	class A;


## Require

	require true


## Where

	where c1,c2 ===> static_assert(...);static_assert(...);
	where a ===> static_assert(a,"DOESN'T MATCH STATIC CONDITION");
	where A :=: B ===> static_assert(hash::is_same<A,B>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :>: B ===> static_assert(hash::is_base_of<A,B>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :<: B ===> static_assert(hash::is_base_of<B,A>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :is: abstract ===> static_assert(hash::is_abstract<A>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :has: nothrow_assign ===> static_assert(hash::has_nothrow_assign<A>::value,"DOESN'T MATCH TYPE CONDITION");

