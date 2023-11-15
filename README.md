Hectoc solver
=============

I wrote this as an algorithmic exercise, then as a way to learn Dune, then as another algorithmic exercice (write arithmetic ASTs with minimum amount of parenthesis).

Solver itself is by no mean optimal (specially concerning memory usage).
optimal AST serializer is not finished (say it doesn't know + is both left and right associative, so sometimes outputs a+(b+c)...)

Example
=======

    ᐅ dune exec hectoc 253316                                                 
    135 solutions:                     
    (-2*5)^(3*((3+1)/6))
    (-2*5)^(3*(3+1)/6)
    (-2*5)^(3*3-(1+6))
    (-2*5)^(3*3-1-6)
    (-2*5)^(3/3+1^-6)
    (-2*5)^(3/3+1^6)
    (-2+(53/3+1))*6
    (-2+5)*33+1^-6
    ...
