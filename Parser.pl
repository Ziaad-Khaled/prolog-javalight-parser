% Start Rule
s(T) --> statements(T).


% statements
statements([T]) --> statement(T).
statements([T|Rest]) --> statement(T), statements(Rest).

statement(T) --> assignment_stmt(T).
statement(T) --> conditional_stmt(T).
statement(T) --> loop_stmt(T).


% Assignment statement
assignment_stmt(assign(ID, Expr)) --> identifier(ID), [=], arithmetic_expr(Expr), [;].





%Arithmetic expressions
arithmetic_expr(N) --> isInteger(N), arithmetic_expr_tail([]).
arithmetic_expr(ID) --> identifier(ID), arithmetic_expr_tail([]).
arithmetic_expr(binop(Op, Lhs, Rhs)) --> identifier(Lhs), arithmetic_operator(Op), arithmetic_expr(Rhs), arithmetic_expr_tail([]).
arithmetic_expr(binop(Op, Lhs, Rhs)) --> isInteger(Lhs), arithmetic_operator(Op), arithmetic_expr(Rhs), arithmetic_expr_tail([]).

arithmetic_expr_tail(_) --> [].
arithmetic_expr_tail(Tail) --> arithmetic_operator(Op), arithmetic_expr(Rhs), {Tail = [Op, Rhs | Tail1]}, arithmetic_expr_tail(Tail1).

arithmetic_operator(arithm_op("+")) --> [+].
arithmetic_operator(arithm_op("-")) --> [-].
arithmetic_operator(arithm_op("*")) --> [*].
arithmetic_operator(arithm_op("/")) --> [/].
arithmetic_operator(arithm_op("%")) --> ['%'].



% If_else
conditional_stmt(if(T, Statements)) -->
    [if], ['('], relational_expr(T), [')'], statements(Statements).
conditional_stmt(if_else(relational_expr(T), Stmt1, Stmt2)) -->
    [if], ['('], relational_expr(T), [')'], statements(Stmt1), [else], statements(Stmt2).

% Relational Expressions
relational_expr(T) --> ['('], relational_expr(T), [')'].
relational_expr(relational_exp(T1, Op, T2)) --> 
    arithmetic_expr(T1), relational_operator(Op), arithmetic_expr(T2).

relational_operator(relational_op("==")) --> [==].
relational_operator(relational_op("!=")) --> ['!='].
relational_operator(relational_op("<=")) --> [<=].
relational_operator(relational_op("<")) --> [<].
relational_operator(relational_op(">=")) --> [>=].
relational_operator(relational_op(">")) --> [>].


% Loop
loop_stmt(while_loop(T, Statements)) --> [while], ['('], relational_expr(T), [')'], statements(Statements).


% DCG rule for Java identifiers with parse tree
identifier(id(Name)) --> java_letter(H), java_identifier_tail(T), {dif(Name,else), dif(Name,if), dif(Name,while), atomic_list_concat([H|T], Name)}.

java_letter(Char) --> [Char], { \+member(Char, [if, else, while,==,'!=',<=,<,>=, >]), (char_type(Char, csymf) ;  Char = '$') }.

java_identifier_tail([]) --> [].
java_identifier_tail([H|T]) --> [H], java_identifier_tail(T), { \+member(H, [if, else, while, ==,'!=',<=,<,>=, >]),(char_type(H, csym); integer(H); H = '$') }.



isInteger([H|T]) --> [H|T], {isdigit(H), integer_helper(T)}.

isdigit(C) :-
    integer(C).

integer_helper([]).
integer_helper([H|T]) :-
    isdigit(H),
    integer_helper(T).



% Test case for assignment statement
test_assignment_stmt_1 :-
    phrase(statements(X), [c,=,10,;], []),
    writeln(X).

%"c=0;c=0;counter=2+3;"
test_assignment_stmt_2 :-
    %string_codes("c=0;x=222;counter=500*3+2;", Input),
    phrase(statements(X), [c,=,0,;,x,=,222,;,c,o,u,n,t,e,r,=,500,*,3,+,5,;], []),
    writeln(X).

% Test case 1: Conditional statement with equality
test_conditional_stmt_1 :-
    phrase(statements(X), "if(xyz==10)c=5;", []),
    writeln(X).

% Test case 2: Conditional statement with inequality
test_conditional_stmt_2 :-
    phrase(statements(X), "if(c!=x)c=y;", []),
    writeln(X).

% Test case 3: Conditional statement with less than or equal to
test_conditional_stmt_3 :-
    phrase(statements(X), "if(c<=x)c=y;", []),
    writeln(X).

% Test case 4: Conditional statement with less than
%"if(c<x)c=y;"
test_conditional_stmt_4 :-
    phrase(statements(X), [if,"(", c, ==, x,")", c,=,y,;], []),
    writeln(X).

% Test case 5: Conditional statement with greater than or equal to
test_conditional_stmt_5 :-
    phrase(statements(X), "if(c>=x)c=y;", []),
    writeln(X).

% Test case 6: Conditional statement with greater than
test_conditional_stmt_6 :-
    phrase(statements(X), "if(c>x)c=y;", []),
    writeln(X).


test_conditional_stmt_7 :-
    phrase(statements(X), "if(c>x)c=y;c=x;", []),
    writeln(X).

test_conditional_stmt_8 :-
    phrase(statements(X), "if(c>x)c=y;elsec=x;", []),
    writeln(X).


test_conditional_stmt_9 :-
    phrase(statements(X), "if(c>x)if(counter>x)c=y;", []),
    writeln(X).

test_conditional_stmt_10 :-
    phrase(statements(X), "if(c>x)if(counter>x)c=y;elseif(counter>y)counter=x;w=y/x;", []),
    writeln(X).

% Test case for loop statement
test_loop_stmt :-
    phrase(statements(X), "while(counter<=w-1)counter=counter+x+5;", []),
    writeln(X).

% Test case for parsing a complete Java-Light program
test_java_light_program :-
    phrase(statements(X), "counter=x+y;while(counter<=w-1)while(counter!=y)counter=counter+x+5;if(c>x)if(counter>x)c=y;elseif(counter>y)counter=x;w=y/x;", []),
    writeln(X).

%Public Testcases
%s(T,[r, u, n,=,0,;,while,'(',r, u, n, <=, w, -, 1, ')', r, u, n, =, 2, *, r, u, n, +,1, ;],[]).
%s(T, [if, '(' , 'C', o, m, p, i, l, e, r, '!=', o, t, h, e, r, s, ')', a, l, l, =, 1,;], []).
%s(T, [while,'(',r, u, n, <=, w, -, 1, ')', r, u, n, =, 2, *, r, u, n, +, 1, ;], []).
%s(T,[if, '(', 'T', o, t, a, l, <, 500, ')', 'T', o, t, a, l, =, 'T', o, t, a, l, +,150, ;, else, if, '(', x, '!=', 'T', o, t, a, l, ')', 'T', o, t, a, l, =, 0, ;, else,x, =, 55, /, y, -, 15, ;],[]).
%s(T,[if, '(', t, o, t, a, l, <, 500, ')', t, o, t, a, l, =, t, o, t, a, l, +,150, ;, else, if, '(', x, >, t, o, t, a, l, ')', t, o, t, a, l, =, 0, ;, else,x, =, 55, /, y, -, 15, ;],[]).
%s(T,[if, '(', t, >=, 500, ')', t, =, t, +,150, ;, else, if, '(', x, >, t, ')', t, =, 0, ;, else,x, =, 55, /, y, -, 15, ;],[]).

%Public Testcases (Soundness)
%s(T, [v, =, 23, *, s, a, l, +, 1, a, z, ;], []).
%s(T, [x, =, z, ;, if, '(', x, >, y, ')', x, '_', n, e, w, =, z, ;, else, x, '_', n,e, w, =, y, ;, else, y, =, z, ;], []).
%s(T, [if, '(', x, >, 0, ')', x, =, x, -, 1], []).