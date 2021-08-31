% Codigo desenvolvido por Leonardo e Andre.
% Matriculas: 19102922 e 19150871.
% Regras:
% 1) Insira um número em cada célula do diagrama de forma que cada região de tamanho N contenha cada número de 1 a N exatamente uma vez.
% 2) Os números nas células ortogonalmente adjacentes devem ser diferentes.
% 3) Se duas células estiverem verticalmente adjacentes na mesma região, o número da célula superior deve ser maior do que o número da célula inferior.

% Docs:https://docs.google.com/document/d/1ThmNWIclKiW8nQZP8xoloydWxWW5NYXhw18YLgRHL2A/edit?usp=sharing

% Sistemas de pontos, com uma tupla onde primeiro ponto é o valor e o segundo ponto é seu grupo. parametros: (valor numerico, grupo)
% Campos vazios seram representados com 0
mat(
    [
    [ponto(0, 0), ponto(0, 1), ponto(4, 1), ponto(0, 1),  ponto(2, 5),  ponto(0, 4)],
    [ponto(0, 0), ponto(0, 2), ponto(3, 1), ponto(0, 5),  ponto(0, 5),  ponto(0, 5)],
    [ponto(1, 0), ponto(4, 0), ponto(0, 6), ponto(4, 5),  ponto(0, 8),  ponto(0, 8)],
    [ponto(0, 12), ponto(5, 11), ponto(0, 6), ponto(0, 7), ponto(0, 7), ponto(2,8)],
    [ponto(0, 12), ponto(0, 11), ponto(0, 11), ponto(0, 10),  ponto(3, 10),  ponto(0, 8)],
    [ponto(6, 11), ponto(2, 11), ponto(0, 11), ponto(2, 10),  ponto(0, 10),  ponto(5, 10)]
    ]
).

% Cria um ponto
mponto(V, G, ponto(V, G)) :- !.

% Cria uma coordenada
mcoord(I, J, coord(I, J)) :- !.

% Mostrar o valor de um ponto
displayponto(ponto(V, _)) :- write(V), tab(1).

% Mostrar uma linha da matriz
mostrarLinha([]) :- nl.
mostrarLinha([H|T]) :-
    displayponto(H),
    mostrarLinha(T),
    !.

% Mostrar toda matriz
mostrarMatriz([]).
mostrarMatriz([H|T]) :- 
    mostrarLinha(H),
    mostrarMatriz(T),
    !.

% Dado uma cordenada na posicao I, J retorna o valor do ponto
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
    mponto(-1, -1, P),
    !.

% Metodo para troca de linha da matriz de elementos em uma dada posição
% Recebe o valor da nova linha da matriz -> K
% Recebe o valor da nova coluna da matriz -> P
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
getv(ponto(V, _), V).
% retorna o grupo G de um ponto
getg(ponto(_, G), G).

% retorna uma lista de todas as coordenadas de uma matriz M que fazem
% parte do grupo do ponto na cordenada C
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
pegaradj(coord(I, J), [C1, C2, C3, C4]) :-
    mcoord(A1, J, C1), A1 is I + 1,
    mcoord(A2, J, C2), A2 is I - 1,
    mcoord(I, B1, C3), B1 is J + 1,
    mcoord(I, B2, C4), B2 is J - 1.

% verifica se o valor N ainda não existe no grupo da coordenada C
verificagrupo(C, N, M) :-
    getgp(C, M, GP),
    verificarlista(GP, N, M),
    !.

% verifica se o valor N ainda não existe nas adjacencias de C
verificaradj(C, N, M) :-
    pegaradj(C, NB),
    verificarlista(NB, N, M),
    !.

% verifica se há um valor N em uma lista
verificarlista([], _, _).
verificarlista([H|T], N, M) :-
    getp(H, M, P),
    getv(P, V),
    V \== N,
    verificarlista(T, N, M),
    !.

% verifica se um valor N pode ser inserido na coordenada C de uma matriz M
verifica(C, N, M) :-
    verificagrupo(C, N, M),
    verificaradj(C, N, M),
    !.

% retorna uma coordenada C ainda não preenchida da matriz M
posicaovazia(M, C) :-
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

% Este metodo pegamos dessa apresentação
% https://hudsoncosta.files.wordpress.com/2012/11/listas_em_prolog.pdf
conc([], L, L).
conc([X | L1], L2, [X | L3]):-
conc(L1, L2, L3). 

% https://hudsoncosta.files.wordpress.com/2012/11/listas_em_prolog.pdf
% Este metodo pegamos dessa apresentação
inverter([], []). % A inversão de uma lista vazia é a própria lista vazia.
inverter([X|Y], Z):- % A inversão de uma lista não-vazia é a inversão de
inverter(Y, Y1), % seu corpo e a concatenação deste corpo invertido
conc(Y1, [X], Z). % com a cabeça da lista original.

% retorna uma lista S de todas as soluções possiveis da coordenada C
numValidos(C, M, S) :-
    getgp(C, M, GP),
    length(GP, Max),
    findall(
        N,
        (
            between(1, Max, N),
            verifica(C, N, M)
        ),
        I
    ), 
    inverter(I, S).

% Metodo 
solucionar(coord(-1, -1), M, _, M) :- mostrarMatriz(M).
solucionar(_, _, [], []).
solucionar(C, M, [H|T], R) :-
    getp(C, M, P1),
    getg(P1, G),
    mponto(H, G, P),
    setp(C, P, M, MN),
    posicaovazia(MN, C2),
    numValidos(C2, MN, S),
    solucionar(C2, MN, S, R2),
    (
        length(R2, 0),
        solucionar(C, M, T, _)
    );
    (
        length(R2, T),
        T \== 0,
        R is R2
    ).

% Metodo principal
soluciona :-
    mat(M),
    posicaovazia(M, C),
    numValidos(C, M, S),
    solucionar(C, M, S, _),
    !.