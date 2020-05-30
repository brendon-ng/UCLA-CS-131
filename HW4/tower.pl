% tower implementaion

tower(N, T, C) :-
    % Condition 1
    N >= 0,
    % Condition 2
    length(T, N),
    maplist(valid_row(N), T),
    transpose(T, X),
    maplist(fd_all_different, X),
    % Condition 3
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right,N),
    valid_counts(N, Top),
    valid_counts(N, Bottom),
    valid_counts(N, Left),
    valid_counts(N, Right),
    maplist(tower_counts(N), Left, T),
    maplist(tower_counts(N), Top, X),
    flip(T, R),
    maplist(tower_counts(N), Right, R),
    flip(X, B),
    maplist(tower_counts(N), Bottom, B),
    maplist(fd_labeling, T).

valid_counts(N, Counts) :-
    length(Counts, N),
    fd_domain(Counts, 1, N).

valid_row(N, Row) :-
    length(Row, N),
    fd_domain(Row, 1, N),
    fd_all_different(Row).

tower_counts(Max, N, [Hd|Tl]) :- tower_counts_help(Max, N, [Hd|Tl], 0). 

tower_counts_help(Max, One, [Hd|_], _) :-
    One #= 1,
    Max #= Hd.
tower_counts_help(Max, Count, [Hd|Tl], H) :-
    Hd #> H,
    tower_counts_help(Max, Count-1, Tl, Hd).
tower_counts_help(Max, Count, [Hd|Tl], H) :-
    Hd #< H,
    tower_counts_help(Max, Count, Tl, H).

    

% plain_tower implementation

plain_tower(N, T, C) :-
    % Condition 1
    N >= 0,
    % Condition 2
    length(T, N),
    % Condition 3
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right,N),
    % Check both Condition 2 and validity of counts
    length(P, N),
    perm_of(P, N),
    maplist(p_valid_counts(N, P), T, Left),
    flip(T, R),
    maplist(p_valid_counts(N, P), R, Right),
    transpose(T, X),
    maplist(p_valid_counts(N, P), X, Top),
    flip(X, B),
    maplist(p_valid_counts(N, P), B, Bottom).

p_valid_counts(N, P, Row, Count) :-
    permutation(P, Row),
    between(1, N, Count),
    p_tower_counts(N, 0, Count, Row).

p_tower_counts(_, _, 0, []). 
p_tower_counts(Max, H, Count, [Hd|Tl]) :-
    Hd > H,
    Count_minus is Count - 1,
    p_tower_counts(Max, Hd, Count_minus, Tl).
p_tower_counts(Max, H, Count, [Hd|Tl]) :-
    Hd < H,
    p_tower_counts(Max, H, Count, Tl).


perm_of([],0).
perm_of([H|T],N) :-
    H = N,
    N_minus is N - 1,
    perm_of(T,N_minus).


% Speed Test
run_tower(Tower):-
    statistics(cpu_time, [Start|_]),
    tower(5,_T, counts([1,2,3,3,2],[3,2,3,2,1],[1,2,3,4,3],[2,3,3,2,1])),
    statistics(cpu_time, [End|_]),
    Tower is End - Start.
run_plain(Plain):-
    statistics(cpu_time, [Start|_]),
    plain_tower(5, _T, counts([1,2,3,3,2],[3,2,3,2,1],[1,2,3,4,3],[2,3,3,2,1])),
    statistics(cpu_time, [End|_]),
    Plain is End - Start.
speedup(R) :-
    run_tower(Tower),
    run_plain(Plain),
    R is Plain/Tower.


% Ambiguous Implementation
ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.
    

% Helper Flip Function to horizontally flip array
flip(X, F) :- maplist(reverse, X, F).

% Transpose implementation from TA Help
transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

  
