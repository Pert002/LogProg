:-['output.pl'].

sex(X, m) :- father(X, _).
sex(X, f) :- mother(X, _).

relative(Rel,  X,  Y) :- nonvar(Rel),  !,  dfs(Rel,  Y,  [X]).
relative(Rel,  X,  Y) :- bfs(Rel,  Y,  [[X]]).

rel(father, X, Y) :- father(X, Y).
rel(mother, X, Y) :- mother(X, Y).
rel(son, X, Y) :- father(Y, X),  sex(X, m).
rel(son, X, Y) :- mother(Y, X),  sex(X, m).
rel(dauther, X, Y) :- father(Y, X),  sex(X, f).
rel(dauther, X, Y) :- mother(Y, X),  sex(X, f).
rel(sister, X, Y) :- father(A, X),  father(A, Y),  X \= Y,  sex(X, f).
rel(sister, X, Y) :- mother(A, X),  mother(A, Y),  X \= Y,  sex(X, f).
rel(brother, X, Y) :- father(A, X),  father(A, Y),  X \= Y,  sex(X, m).
rel(brother, X, Y) :- mother(A, X),  mother(A, Y),  X \= Y,  sex(X, m).
rel(grandmother, X, Y) :- mother(X, Z),  mother(Z, Y).
rel(grandmother, X, Y) :- mother(X, Z),  father(Z, Y).
rel(grandfather, X, Y) :- father(X, Z),  father(Z, Y).
rel(grandfather, X, Y) :- father(X, Z),  mother(Z, Y).
rel(granddaughter, X, Y) :- rel(grandfather, Y, X),  sex(X, f).
rel(granddaughter, X, Y) :- rel(grandmother, Y, X),  sex(X, f).
rel(grandson, X, Y) :- rel(grandfather, Y, X),  sex(X, m).
rel(grandson, X, Y) :- rel(grandmother, Y, X),  sex(X, m).
rel(zolovka, X, Y):- wife(Y, Z), father(A, X),  father(A, Z),  X \= Z,  sex(X, f).
rel(zolovka, X, Y):- wife(Y, Z), mother(A, X),  mother(A, Z),  X \= Z,  sex(X, f).

prolong([X|T],  [Y,  X|T]) :- rel(_,  X,  Y),  not(member(Y,  [X|T])).
to_rel([],  [_]).
to_rel([R|T1],  [Y,  X|T2]) :- rel(R,  X,  Y),  to_rel(T1,  [X|T2]).

dfs([],  Y,  [Y|_]).
dfs([Rel1|Rel2],  Y,  [X|T]) :-
	rel(Rel1,  X,  Z),  not(member(Z,  [X|T])), 
	dfs(Rel2,  Y,  [Z, X|T]).

bfs(R,  Y,  [[Y|T]|_]) :- to_rel(R1,  [Y|T]),  reverse(R1,  R).
bfs(R,  Y,  [[X|P]|Q1]) :-
	setof(Z,  prolong([X|P],  Z),  T),  append(Q1,  T,  Q2),  !, 
	bfs(R,  Y,  Q2).
bfs(R,  Y,  [_|T]) :- bfs(R,  Y,  T).