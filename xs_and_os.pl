%will never change
hvalues([2,1,2,
        1,4,1,
        2,1,2]).

%true
board([_X0,_X1,_X2,_X3,_X4,_X5,_X6,_X7,_X8]).

full_board([X0,X1,X2,
            X3,X4,X5,
            X6,X7,X8]) :-
    atom(X0),
    atom(X1),
    atom(X2),
    atom(X3),
    atom(X4),
    atom(X5),
    atom(X6),
    atom(X7),
    atom(X8).

replace(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R).



place(I, L, R) :-
    replace(I, L, x, R).

replace_h_0([], [], []).

replace_h_0([H|T], [X|Y], [0|Y2]) :-
    atom(H),
    replace_h_0(T, Y, Y2).

replace_h_0([H|T], [X|Y], [X|Y2]) :-
	\+ atom(H),
    replace_h_0(T, Y, Y2).
    

   
emptySquare(Board, I) :-
    nth0(I, Board, E),
    \+atom(E).

highH(F, L, I) :-
    max_list(L, F),
    nth0(I, [2,1,2,
        1,4,1,
        2,1,2], F).

test(Board, Hvalues, R) :- full_board(Board).
test(Board, Hvalues, R) :- xWin(Board).

test(Board, Hvalues, R) :-
    replace_h_0(Board, Hvalues, Result),
    highH(F, Result, I),
    emptySquare(Board, I),
    place(I, Board, R),
    
    test(R, Result, A).


%---------------------
%wincons for xs
xWin(
      [X0,_X1,_X2,
            _X3,X4,_X5,
            _X6,_X7,X8])
:-
    X0 == x,
    X4 == x,
    X8 == x.

xWin(
      [X0,X1,X2,
            _X3,_X4,_X5,
            _X6,_X7,_X8])
:-
    X0 == x,
    X1 == x,
    X2 == x,
    write('win').

xWin(
     [_X0,_X1,_X2,
            X3,X4,X5,
            _X6,_X7,_X8])
:-
    X3 == x,
    X4 == x,
    X5 == x,
    write('win').

xWin(
      [_X0,_X1,_X2,
            _X3,_X4,_X5,
            X6,X7,X8])
:-
    X6 == x,
    X7 == x,
    X8 == x,
    write('win').

xWin(
     [X0,_X1,_X2,
            X3,_X4,_X5,
            X6,_X7,_X8])
:-
    X0 == x,
    X3 == x,
    X6 == x,
    write('win').

xWin(
     [X0,X1,X2,
            X3,X4,X5,
            X6,X7,X8])
:-
    X1 == x,
    X4 == x,
    X7 == x,
    write('win').

xWin(
     [X0,X1,X2,
            X3,X4,X5,
            X6,X7,X8])
:-
    X2 == x,
    X5 == x,
    X8 == x,
    write('win').