% tabuleiro
mat(
    [
    [point(0, 0), point(0, 1), point(4, 1), point(0, 1),  point(2, 5),  point(0, 4)],
    [point(0, 0), point(0, 2), point(3, 1), point(0, 5),  point(0, 5),  point(0, 5)],
    [point(1, 0), point(4, 0), point(0, 6), point(4, 5),  point(0, 8),  point(0, 8)],
    [point(0, 12), point(5, 11), point(0, 6), point(0, 7), point(0, 7), point(2,8)],
    [point(0, 12), point(0, 11), point(0, 11), point(0, 10),  point(3, 10),  point(0, 8)],
    [point(6, 11), point(2, 11), point(0, 11), point(2, 10),  point(0, 10),  point(5, 10)]
    ]
).

% constroi um ponto com valor V e grupo G
mpoint(V, G, point(V, G)) :- !.

% constroi uma coordenada com valores I e J
mcoord(I, J, coord(I, J)) :- !.

% imprime o valor V de um ponto
displaypoint(point(V, _)) :- write(V), tab(1).

% imprime uma linha
displayline([]) :- nl.
displayline([H|T]) :-
    displaypoint(H),
    displayline(T),
    !.

% imprime uma matriz
displaymatrix([]).
displaymatrix([H|T]) :- 
    displayline(H),
    displaymatrix(T),
    !.

% retorna o ponto P da matriz M na posição I, J
getp(coord(I, J), M, P) :-
    (
        length(M, Z1),
        I < Z1,
        I >= 0,
        nth0(0, M, L),
        length(L, Z2),
        J < Z2,
        J >= 0,
        nth0(I, M, Li),
        nth0(J, Li, P),
        !
    );
    mpoint(-1, -1, P),
    !.

% substitui o elemento de indice K por P em uma lista
replace([_|T], K, P, [P|T]) :-
    K == 0,
    !.
replace([H|T], K, P, [H|L]) :-
    K1 is K - 1,
    replace(T, K1, P, L),
    !.

% modifica um ponto da matriz M na posição I, J
setp(coord(I, J), P, M, NM) :-
    nth0(I, M, L),
    replace(L, J, P, NL),
    replace(M, I, NL, NM),
    !.

% retorna o valor V de um ponto
getv(point(V, _), V).
% retorna o grupo G de um ponto
getg(point(_, G), G).

% retorna uma lista de todas as coordenadas de uma matriz M que fazem
% parte do grupo do ponto na corrdenada C
getgp(C, M, R) :-
    getp(C, M, P),
    getg(P, G),
    findall(
        C1, 
        (
            nth0(0, M, L), 
            length(M, MI1), length(L, MJ1),
            MI is MI1 - 1, MJ is MJ1 - 1, 
            between(0, MI, I), between(0, MJ, J), 
            mcoord(I, J, C1), getp(C1, M, P1), getg(P1, G1), G1 == G
            ), 
        R
    ).

% retorna uma lista das coordenadas adjacentes a coord(i, J)
getneib(coord(I, J), [C1, C2, C3, C4]) :-
    mcoord(A1, J, C1), A1 is I + 1,
    mcoord(A2, J, C2), A2 is I - 1,
    mcoord(I, B1, C3), B1 is J + 1,
    mcoord(I, B2, C4), B2 is J - 1.

% verifica se o valor N ainda não existe no grupo da coordenada C
verifygroup(C, N, M) :-
    getgp(C, M, GP),
    verifylist(GP, N, M),
    !.

% verifica se o valor N ainda não existe nas adjacencias de C
verifyneib(C, N, M) :-
    getneib(C, NB),
    verifylist(NB, N, M),
    !.

% verifica se há um valor N em uma lista
verifylist([], _, _).
verifylist([H|T], N, M) :-
    getp(H, M, P),
    getv(P, V),
    V \== N,
    verifylist(T, N, M),
    !.

% verifica se um valor N pode ser inserido na coordenada C de uma matriz M
verify(C, N, M) :-
    verifygroup(C, N, M),
    verifyneib(C, N, M),
    !.

% retorna uma coordenada C ainda não preenchida da matriz M
emptyslot(M, C) :-
    (
        nth0(0, M, L),
        length(M, MI1),
        length(L, MJ1),
        MI is MI1 - 1,
        MJ is MJ1 - 1,
        between(0, MI, I),
        between(0, MJ, J),
        mcoord(I, J, C),
        getp(C, M, P),
        getv(P, V),
        V == 0
    );
    mcoord(-1, -1, C).

conc([], L, L).
conc([X | L1], L2, [X | L3]):-
conc(L1, L2, L3). 

inverter([], []). % A inversão de uma lista vazia é a própria lista vazia.
inverter([X|Y], Z):- % A inversão de uma lista não-vazia é a inversão de
inverter(Y, Y1), % seu corpo e a concatenação deste corpo invertido
conc(Y1, [X], Z). % com a cabeça da lista original.

% retorna uma lista S de todas as soluções possiveis da coordenada C
solat(C, M, S) :-
    getgp(C, M, GP),
    length(GP, Max),
    findall(
        N,
        (
            between(1, Max, N),
            verify(C, N, M)
        ),
        I
    ), 
    inverter(I, S).

% resolve o puzzle
solve(coord(-1, -1), M, _, M) :- displaymatrix(M).
solve(_, _, [], []).
solve(C, M, [H|T], R) :-
    getp(C, M, P1),
    getg(P1, G),
    mpoint(H, G, P),
    setp(C, P, M, MN),
    emptyslot(MN, C2),
    solat(C2, MN, S),
    solve(C2, MN, S, R2),
    (
        length(R2, 0),
        solve(C, M, T, _)
    );
    (
        length(R2, T),
        T \== 0,
        R is R2
    ).

% entrada para solve
solveit :-
    mat(M),
    emptyslot(M, C),
    solat(C, M, S),
    solve(C, M, S, _),
    !.