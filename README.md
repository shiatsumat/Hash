Hash
====

Hash is an object-oriented programming language with a Haskell-like syntax.
Hash compiler generates a C++11 source code.
Hash was named after "Has"kell and sugar syntax("sh").

Features of Hash
----------------
* Off-side rule
* Algebraic data type
* Pattern matching
* Lazy evaluation
* Active pattern
* Units of measure

Some features are inherited from Haskell, some from F#, and others from C++.

Syntax
------
Haskell has a very simple and beautiful syntax while C++ syntax is slightly complicated. Hash has a great syntax like Haskell.
Off-side rule will make source codes clear.
Hash allows you to make new symbolic operators freely and decide their precedence.

Constancy
---------
Hash encourages you to program statically as possible.
Haskell is perfectly referentially tranparent, and C++ encourages you to rewrite values. Hash is at the halfway point between the two.

Automatically, Hash compiler adds const keywords to the proper points of the output. In order to make something not constant, add a keyword which means "variable". Surely you will find it very reasonable.

Type
----
Hash has no system of type inference, so you need to declare a type for each functions.
However, it can use a template type of C++. Hash provides a simpler syntax.

Class
-----
Hash uses a class just like C++, which can contain member variants and functions including constructors and destructors.
Inheritance and encapsulation are also possible.

Algebraic Data Type
-------------------
You can use a Haskell-like algebraic data type (ADT). You can also use a recursive definition for ADTs.
ADTs can have member functions just like a class.

Pattern Matching
----------------
Haskell has a very strong pattern matching system, which makes programming highly intuititive.
The syntax of pattern matching is mainly based on Haskell, but you can also use an active pattern inherited from F#.

Pattern matching will be automatically prepared for an algebraic data type. You can also define active patterns for classes and ADTs like F#.

Lazy Evaluation
---------------
Lazy evaluation on expressions and types is implemented using a pointer.
Although the lazy evaluation of Hash is not very strong, the syntax is similar to that of Haskell.
You can also use an infinite list.

Function
--------
You can use a lambda expression with a powerful syntax.
Functions are constant as default, but you can also make variable functions.
You can make template functions as C++.

Hash provides a great environment for functional programming.

Implementation of Hash Compiler
-------------------------------
Hash compiler is implemented in Haskell.
The syntax is parsed with a monadic packrat parser of a library "Pappy" made by Bryan Ford.
For more information about Pappy, see http://pdos.csail.mit.edu/~baford/packrat/thesis/ .
