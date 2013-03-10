# Hash Ver 0.01 Reference

## Binaries in Directory "Code"

* main.exe : Just interpret into C++11 code. It uses standard input/output. You can use it like

<pre><code> main <a.hash </code></pre>

* Compile.exe : Compile with g++. You can use it like

<pre><code> compile a.hash -oa.exe </code></pre>

## Point

* The grammar of Hash is quite similar to C++.
* Expressions and statements are very similarly treated.
* Every definition will be automatically pre-declared.

## Comment

<pre><code>
/*a*/ ===> //a
/*a/*b*/c*/ ===>
	//a
	////b
	//c
/*/ ===> (nothing)
//a
</code></pre>

## Type

<pre><code>
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
A->B ===> Hash::Function<A,B>
(A,B) ===> Hash::Tuple<A,B>
[A] ===> Hash::List<A>
</pre></code>

## Name (of Variants, Functions, Types, and so on)

<pre><code>
a
n::a
int
return ===> returnNotReservedWord
</pre></code>

## Literal
123
'x'
"foo"

## Expression

<pre><code>
x
if(x) y else z ===> x?y:z
\(A x,B y){} ===> [&](A x,B y){}
\(A x,B y)->C{} ===> [&](A x,B y)->C{}
{return x;} ===> [&](){return x;}
{x;} ===> [&](){x;return Hash::unit;}
(x,y) ===> Hash::makeTuple(x,y)
() ===> Hash::unit
[x,y] ===> Hash::makeList(x,y)
</pre></code>

## Statement

<pre><code>
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
</pre></code>

## Variant

<pre><code>
extern A x,y;
A x=0,y;
A* x,y; ===> A *x, *y;
</pre></code>

## Function

<pre><code>
R f(A x,B y);
R f(A x,B y){}
R f<T,int x>(){} ===> template<typename T,int x> R f(){}
R f(A x,B y)=x; ===> R f(A x,B y){return x;}
</pre></code>

## Enum

<pre><code>
enum A{x=0,y} ===> enum A{x=0,y};
bitenum A{x,y,z} ===> enum A{x=1,y=2,z=4};
</pre></code>

## Class/Struct

<pre><code>
class A{} ===> class A{};
class A{public int x;} ===> class A{public: int x;};
class A<T,int x>{} ===> template<typename T,int x> class A{};
class A where c {} ===> class A{static_assert(...);};
</pre></code>

## Where

<pre><code>
where c1,c2 ===> static_assert(...);static_assert(...);
where a ===> static_assert(a,"DOESN'T MATCH STATIC CONDITION");
where A :=: B ===> static_assert(Hash::is_same<A,B>::value,"DOESN'T MATCH TYPE CONDITION");
where A :>: B ===> static_assert(Hash::is_base_of<A,B>::value,"DOESN'T MATCH TYPE CONDITION");
where A :<: B ===> static_assert(Hash::is_base_of<B,A>::value,"DOESN'T MATCH TYPE CONDITION");
where A :is: abstract ===> static_assert(Hash::is_abstract<A>::value,"DOESN'T MATCH TYPE CONDITION");
where A :has: nothrow_assign ===> static_assert(Hash::has_nothrow_assign<A>::value,"DOESN'T MATCH TYPE CONDITION");
</pre></code>
