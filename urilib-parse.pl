% -*- Mode: Prolog -*-

% Studenti:
% Riccardo Betelli 914471
% Isabella Faifer // 

% urilib_parse/2

urilib_parse(URIString, URI) :-

    % converto la stringa URI in lista di codici di carattere
    string_to_list(URIString, URIcodes),

    % converto in una lista di atomi
    codes_to_atom(URIcodes, URIlist),

    get_scheme(URIlist, SchemeAt, Rest1),

    atomics_to_string(SchemeAt, SchemeTemp),

    string_lower(SchemeTemp, Scheme),

    check_scheme(Scheme, Sch),

    check_authority(Rest1, Sch, Aut),

    remove_slash(Rest1, Rest2, Aut),

    userinfo_presence(Rest2, Sch, Aut, UserI),
    
    get_userinfo(Rest2, UserInfoAt, Rest3, Sch, UserI),

    get_host(Rest3, HostAt, Rest4, Sch, Aut),

    get_port(Rest4, PortAt, Rest5, Sch, Aut),

    get_path(Rest5, PathAt, Rest6, Sch, Aut),

    get_query(Rest6, QueryAt, Rest7, Sch),

    get_fragment(Rest7, FragAt, Rest8, Sch),

    to_string_if_not_empty(UserInfoAt, UserInfo),

    to_string_if_not_empty(HostAt, Host),

    to_string_if_not_empty(PortAt, Port),

    to_string_if_not_empty(PathAt, Path),

    to_string_if_not_empty(QueryAt, Query),

    to_string_if_not_empty(FragAt, Fragment),
    
    URI = uri(Scheme, UserInfo, Host, Port, Path, Query, Fragment).


% SISTEMA CASO IDENTIFICATORE VUOTO (FAIL)



uri(_, _, _, _, _, _, _).



% get_scheme/3
% divide la lista in due liste, una contenente lo Scheme
% l'altra contenente il resto della lista
% (senza il carattere divisore)

get_scheme([: | Xs], [], Xs).

get_scheme([X | Xs], [X | Ys], Cut) :-
    check_identificatore(X),
    X \== :,
    get_scheme(Xs, Ys, Cut).



% get_userinfo/5
% divide la lista in due liste, una contenente (se c'è) lo userinfo
% l'altra contenente il resto della lista

get_userinfo(L, [], L, Sch, User) :-
    User == 0.

get_userinfo(L, Y, Rest, Sch, User) :-
    Sch == 1,
    User == 1,
    get_userinfo(L, Y, Rest).

get_userinfo(L, Y, Rest, Sch, User) :-
    Sch == 2,
    User == 1,
    get_userinfo(L, Y, Rest).

get_userinfo([@ | Ls], [], Ls).

get_userinfo([L | Ls], [L | Ys], Rest) :-
    L \== @,
    check_identificatore(L),
    get_userinfo(Ls, Ys, Rest).



% get_host/4
% divide la lista in due liste, la prima contente l'host la
% seconda il resto della lista iniziale

get_host(L, [], L, Sch, 0).

get_host(L, H, Rest, 1, 1) :-
    get_host(L, H, Rest).

get_host(L, H, Rest, 2, 1) :-
    get_host(L, H, Rest).

get_host([L | Ls], [L | Ys], Rest) :-
    char_type(L, alpha),
    get_host_name(Ls, Ys, Rest).

get_host(L, Y, Rest) :-
    get_host_ip(L, Y, Rest).



% get_host_name/3

get_host_name([L | Ls], [L | Ys], Rest) :-
    check_iden_host(L),
    get_host_name(Ls, Ys, Rest).

get_host_name([], [], []).

get_host_name([L | Ls], [], [L | Ls]).



% get_host_ip/3

get_host_ip(L, Y, R) :-
    get_host_ip_parts(L, ['.' | R1], Z1),
    get_host_ip_parts(R1, ['.' | R2], Z2),
    get_host_ip_parts(R2, ['.' | R3], Z3),
    get_host_ip_parts(R3, R, Z4),

    append(Z1, ['.'], Y1),
    append(Y1, Z2, Y2),
    append(Y2, ['.'], Y3),
    append(Y3, Z3, Y4),
    append(Y4, ['.'], Y5),
    append(Y5, Z4, Y).
    
    

% get_host_parts/3

get_host_ip_parts([A, B, C | Ls], Ls, Z) :-
    char_type(A, digit),
    char_type(B, digit),
    char_type(C, digit),

    append([A], [B], Conc),
    append(Conc, [C], Z),

    check_ip_part(Z).

get_host_ip_parts([A, B | Ls], Ls, Z) :-
    char_type(A, digit),
    char_type(B, digit),

    append([A], [B], Z),

    check_ip_part(Z).

get_host_ip_parts([A | Ls], Ls, [A]) :-
    char_type(A, digit),

    check_ip_part([A]).



% get_port/4
% divide la lista in due liste, la prima contente la porta la
% seconda il resto della lista iniziale, se la porta è assente
% restituisce 80

get_port(L, [], L, Sch, 0).

get_port(L, P, Rest, 1, 1) :-
    get_port(L, P, Rest).

get_port(L, P, Rest, 2, 1) :-
    get_port(L, P, Rest).

get_port([':' | Ls], Y, R) :-
    get_port_num(Ls, Y, R),
    length(Y, Len),
    Len > 0,
    list_To_Num(Y, N),
    N >= 0,
    N =< 65535.

get_port([L | Ls], ['8', '0'], [L | Ls]) :-
    L \== ':'.

get_port([], ['8', '0'], []).

get_port_num([L | Ls], [L | Ys], R) :-
    char_type(L, digit),
    get_port_num(Ls, Ys, R).

get_port_num([L | Ls], [], [L | Ls]) :-
    not(char_type(L, digit)).

get_port_num([], [], []).



% get_path/5
% divide la lista in due liste, la prima contente il path la
% seconda il resto della lista iniziale

get_path(L, [], L, Sch, Aut) :-
    Sch > 2.

get_path([L | Ls], P, Rest, Sch, Aut) :-
    Sch =< 2,
    Aut == 1,
    L == '/',
    get_path([L | Ls], P, Rest).

get_path([L | Ls], P, Rest, Sch, Aut) :-
    Sch =< 2,
    Aut == 0,
    L == '/',
    get_path([L | Ls], P, Rest).

get_path([L | Ls], P, Rest, Sch, Aut) :-
    Sch =< 2,
    Aut == 0,
    L \== '/',
    get_path([L | Ls], P, Rest).

get_path([], [], [], Sch, Aut).

get_path([L | Ls], [L | Ys], Rest) :-
    check_iden_path(L),
    get_path(Ls, Ys, Rest).

get_path([], [], []).

get_path([L | Ls], [], [L | Ls]) :-
    not(check_iden_path(L)).



% get_query/4
% divide la lista in due liste, la prima contente la query la
% seconda il resto della lista iniziale

get_query(L, [], L, Sch) :-
    Sch > 2.

get_query(L, Y, R, Sch) :-
    Sch =< 2,
    get_query(L, Y, R).

get_query(['?' | Ls], Ys, R) :-
    get_query(Ls, Ys, R).

get_query([L | Ls], [L | Ys], R) :-
    check_identificatore(L),
    get_query(Ls, Ys, R).

get_query([], [], []).

get_query(L, [], L).


% get_fragment/4
% divide la lista in due liste, la prima contente il fragment la
% seconda il resto della lista iniziale

get_fragment(L, [], L, Sch) :-
    Sch > 2.

get_fragment(L, Y, R, Sch) :-
    Sch =< 2,
    get_fragment(L, Y, R).

get_fragment(['#' | Ls], Ys, R) :-
    get_fragment(Ls, Ys, R).

get_fragment([L | Ls], [L | Ys], R) :-
    check_identificatore(L),
    get_fragment(Ls, Ys, R).

get_fragment([], [], []).

get_fragment(L, [], L).




% remove_slash/2

remove_slash(['/', '/' | Ls], Ls, 1).

remove_slash([L1, L2 | Ls], [L1, L2 | Ls], 0).


% codes_to_atom/2
% converte una lista di codici di caratteri in una lista di atomi

codes_to_atom([], []).

codes_to_atom([X | Xs], [Y | Ys]) :-
    char_code(Y, X),
    codes_to_atom(Xs, Ys).



% check_authority/2

check_authority([C1, C2 | Cs], 1, 1) :-
    C1 == '/',
    C2 == '/'.

check_authority([C1, C2 | Cs], 2, 1) :-
    C1 == '/',
    C2 == '/'.

check_authority(C, Sch, 0).

check_authority(C, Sch, 0).



% check_iden_path/1

check_iden_path(C) :-
    check_identificatore(C).

check_iden_path(C) :-
    C == '/'.



% check_scheme/2
% ritorna una valore a seconda del tipo di uri (determinato dallo scheme)
% 1 = standard
% 2 = zos
% 3 = news
% 4 = tel o fax

check_scheme(L, 1) :-
    L \== "zos",
    L \== "mailto",
    L \== "news",
    L \== "tel",
    L \== "fax".

check_scheme(L, 2) :-
    L == "zos".

check_scheme(L, 3) :-
    L == "news".

check_scheme(L, 4) :-
    L == "tel".

check_scheme(L, 4) :-
    L == "fax".



% check_identificatore/1

check_identificatore(C) :-
    char_type(C, alnum).

check_identificatore(C) :-
    C == '_'.

check_identificatore(C) :-
    C == '='.

check_identificatore(C) :-
    C == '+'.

check_identificatore(C) :-
    C == '-'.



% check_iden_host/1

check_iden_host(C) :-
    char_type(C, alnum).

check_iden_host(C) :-
    C == '.'.



% to_string_if_not_empty/2

to_string_if_not_empty(L1, L2) :-
    length(L1, Len),
    Len > 0,
    atomics_to_string(L1, L2).

to_string_if_not_empty(L1, L1) :-
    length(L1, Len),
    Len == 0.



% not_snail/1

not_snail([L | Ls]) :-
    L \== @,
    not_snail(Ls).

not_snail([]).



% userinfo_presence/3

userinfo_presence(L, Sch, Aut, 1) :-
    Sch =< 2,
    Aut == 1,
    not(not_snail(L)).

userinfo_presence(L, Sch, Aut, 0).


% list_To_Num/2

list_To_Num(L, N) :-
    atomic_list_concat(L, A),
    atom_number(A, N).



% check_ip_part/1

check_ip_part(Z) :-
    length(Z, Len),
    Len =< 3,
    list_To_Num(Z, N),
    N >= 0,
    N =< 255.



% file ends here
