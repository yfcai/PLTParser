Quick and Dirty Parser Implementation for Lambda Calculus
=========================================================
To use the REPL (Read Eval Print Loop) you first need to compile
the parsers and interpreters by using the following command:

    scalac -d bin modular2.scala

This will generate the necessary class files in the directory `bin`.
Afterwards the scala interpreter can be booted up using this classpath

    scala -cp ./bin

Inside the scala-shell all interpreter objects may be used. These are currently:

    object LambdaInterpreter extends EagerInterpreter with LambdaParser with Nodes
    object FAEInterpreter extends EagerInterpreter with FAEParser with Nodes
    object Lazy extends LazyInterpreter with LambdaParser with Nodes

    object D extends DInterpreter with BackslashIf0Parser with NodesIf0

So, if you want to use the DInterpreter for exercise sheet D. Then just type into
the scala shell:

    D.repl

This will bring up a cmdline inside of the scala shell, prompting you to insert 
lambda calculus expressions:

    > \x.\y.x+y
    Closure(Fun('x,Fun('y,Add(Id('x),Id('y)))),Map())

    > (\x.\y.x+y) 3 4
    NumV(7)


Syntax
------
Basically there are two different Parsers to choose from. Here only one (The lambda-
calculus Parser) is shown.

To create a lambda abstraction over the variable x type:

    \x.x+1