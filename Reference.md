# Hash Ver 0.01 Reference

## Binaries in Directory "Code"

* main.exe : Just interpret into C++11 code. It uses standard input/output. You can use it like
<pre>main &lt;a.hash</pre>

* Compile.exe : Compile with g++. You can use it like
<pre>compile a.hash -oa.exe</pre>

## Point

* The grammar of Hash is quite similar to C++.
* Every definition will be automatically declared forward.
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

## Type

	A
	var ===> auto
	val ===> const auto
	ref ===> auto&
	rref ===> auto&&
	decltype
	const A
	mutable A
	A*
	A&
	A&&
	A! ===> const A
	A->B ===> hash::function<A,B>
	(A,B) ===> hash::tuple<A,B>
	[A] ===> hash::forward_list<A>


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
* You can add "@" to avoid clashing with Hash's contextual keywords.

## Literal

	123
	'x'
	"foo"


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
	A* x,y; ===> A *x, *y;


## Function

	R f(A x,B y);
	R f(A x,B y){}
	R f<T,int x>(){} ===> template<typename T,int x> R f(){}
	R f(A x,B y)=x; ===> R f(A x,B y){return x;}
	static R f(){}
	R f()const{}

## Pattern

	a ---- Name
	10 ---- Literal
	=10+20 ---- Expression
	_ ---- Wildcard
	(1,2,3) ---- Tuple
	[1,2,3] ---- List
	Cons{1,[]} ---- Data Constructor

## Enum

	enum A{x=0,y} ===> enum A{x=0,y};
	bitenum A{x,y,z} ===> enum A{x=1,y=2,z=4};
	enum A;


## Class/Struct

	class A{} ===> class A{typedef A thistype;};
	class A{thistype(){}} ===> class A{...A(){}};
	class A{public int x;} ===> class A{...public: int x;};
	class A<T,int x>{} ===> template<typename T,int x> class A{...};
	class A where c {} ===> class A{...static_assert(...);};
	class A;


## Where

	where c1,c2 ===> static_assert(...);static_assert(...);
	where a ===> static_assert(a,"DOESN'T MATCH STATIC CONDITION");
	where A :=: B ===> static_assert(hash::is_same<A,B>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :>: B ===> static_assert(hash::is_base_of<A,B>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :<: B ===> static_assert(hash::is_base_of<B,A>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :is: abstract ===> static_assert(hash::is_abstract<A>::value,"DOESN'T MATCH TYPE CONDITION");
	where A :has: nothrow_assign ===> static_assert(hash::has_nothrow_assign<A>::value,"DOESN'T MATCH TYPE CONDITION");

