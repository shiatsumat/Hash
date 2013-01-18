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
The very distinctive point of Hash is that classes can contain an algebraic data type.

Pattern Matching
----------------
Haskell has a very strong pattern matching system, which makes programming highly intuititive.
The syntax of pattern matching is mainly based on Haskell, but you can also use an active pattern inherited from F#.
Pattern matching can be applied for algebraic data type in a class.

Lazy Evaluation
---------------
Lazy evaluation on expressions and types is implemented using a pointer.
Although the lazy evaluation of Hash is not very strong, the syntax is similar to that of Haskell.

Implementation of Hash Compiler
-------------------------------
Hash compiler is implemented in Haskell.
The grammar is written in a parsing expression grammar (PEG) and the parser is packrat parser.
I use Parser.hs and Pos.hs in a library "Pappy" made by Bryan Ford. I use a monadic parser.