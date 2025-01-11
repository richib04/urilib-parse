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

    atomics_to_string(SchemeAt, Scheme),

    is_standard(Rest1, IsStandard),

    remove_slash(Rest1, Rest2),

    userinfo_presence(Rest2, IsStandard, Bool1),
    
    get_userinfo(Rest2, UserInfoAt, Rest3, IsStandard, Bool1),

%    to_string_if_not_empty(UserInfoAt, UserInfo),

    get_host(Rest3, Host, Rest4, IsStandard),
    
    URI = uri(Scheme, UserInfoAt, Host, _, _, _, Rest4).



uri(_, _, _, _, _, _, _).



% get_scheme/3
% divide la lista in due liste, una contenente lo Scheme
% l'altra contenente il resto della lista
% (senza il carattere divisore)

get_scheme([: | Xs], [], Xs).

get_scheme([X | Xs], [X | Ys], Cut) :-
    chek_identificatore(X),
    X \== :,
    get_scheme(Xs, Ys, Cut).



% get_userinfo/5
% divide la lista in due liste, una contenente (se c'è) lo userinfo
% l'altra contenente il resto della lista

get_userinfo(L, [], L, Std, Check) :-
    Check == 0.

get_userinfo(L, Y, Rest, Std, Check) :-
    Std == 1,
    Check == 1,
    get_userinfo(L, Y, Rest).

get_userinfo([@ | Ls], [], Ls).

get_userinfo([L | Ls], [L | Ys], Rest) :-
    L \== @,
    chek_identificatore(L),
    get_userinfo(Ls, Ys, Rest).



% get_host/4
% divide la lista in due liste, la prima contente l'host la
% seconda il resto della lista iniziale

% DOMANI LO FAI FUNZIONARE CON LO SCREEN

get_host(L, [], L, 0).

get_host(L, H, Rest, 1) :-
    get_host_p(L, H, Rest).

get_host_p([L | Ls], [L | Ys], Rest) :-
    char_type(L, alpha),
    get_host_name(Ls, Ys, Rest).

get_host_p([L | Ls], [L | Ys], Rest) :-
    char_type(L, digit),
    get_host_ip([L | LS], [L | Ys], Rest).




% get_host_name/3

get_host_name([L | Ls], [L | Ys], Rest) :-
    check_iden_host(L),
    get_host_name(Ls, Ys, Rest).

get_host_name([], [], []).

get_host_name([L | Ls], [], [L | Ls]).



% get_host_ip/3

get_host_ip(L, Y, R4) :-
    get_host_ip_parts(L, A, R1, Z1),
    check_ip_part(Z1),

    get_host_ip_parts(R1, B, R2, Z2),
    check_ip_part(Z2),

    get_host_ip_parts(R2, C, R3, Z3),
    check_ip_part(Z3),

    get_host_ip_parts(R3, D, R4, Z4),
    check_ip_part(Z4),
    
    append(A, B, Y1),
    append(Y1, C, Y2),
    append(Y2, D, Y).

get_host_ip_parts([L | Ls], [L | Ys], Rest, [L | Zs]) :-
    L \== '.',
    char_type(L, digit),
    get_host_ip_parts(Ls, Ys, Rest, Zs).

get_host_ip_parts([Ch | Ls], Ch, Ls, []) :-
    Ch = '.'.

%get_host_ip_parts([], [], [], []).

%get_host_ip_parts([L | Ls], [], [L | Ls], []).


get_host_ip_parts([L | Ls], L, Ls, L) :-
    char_type(L, digit),
    length(Ls, Len),
    Len == 0.

get_host_ip_parts([L | Ls], L, Ls, [L | _]) :-
    char_type(L, digit),
    check_rest(Ls).
    



% remove_slash/2

remove_slash([/, / | Ls], Ls).

remove_slash([L1, L2 | Ls], [L1, L2 | Ls]) :-
    L1 \== '/',
    L2 \== '/'.



% code_to_atom/2
% converte una lista di codici di caratteri in una lista di atomi

codes_to_atom([], []).

codes_to_atom([X | Xs], [Y | Ys]) :-
    char_code(Y, X),
    codes_to_atom(Xs, Ys).



% is_standard/2

is_standard([C1, C2 | Cs], Boolean) :-
    C1 == /,
    C2 == /,
    Boolean is 1.

is_standard(C, Boolean) :-
    Boolean is 0.



% check_identificatore/1

chek_identificatore(C) :-
    char_type(C, alnum).

chek_identificatore(C) :-
    C == '_'.

chek_identificatore(C) :-
    C == '='.

chek_identificatore(C) :-
    C == '+'.

chek_identificatore(C) :-
    C == '-'.



% check_iden_host/1

check_iden_host(C) :-
    char_type(C, alnum).

check_iden_host(C) :-
    C == '.'.



% to_string_if_not_empty/2

to_string_if_not_empty(L1, L2) :-
    L1 == [_|_],
    atomics_to_string(L1, L2).

to_string_if_not_empty(L1, []) :-
    L1 == [].



% not_snail/1

not_snail([L | Ls]) :-
    L \== @,
    not_snail(Ls).



% userinfo_presence/3

userinfo_presence(L, Check, Bool) :-
    not(not_snail(L)),
    Check == 1,
    Bool is 1.

userinfo_presence(L, Check, Bool) :-
    Bool is 0.



% list_To_Num/2

list_To_Num(L, N) :-
    atomic_list_concat(L, A),
    atom_number(A, N).



% check_ip_part/1

check_ip_part(Z) :-
    atomics_to_string(Z, Zs),
    string_length(Zs, Len),
    Len =< 3,
    list_To_Num(Z, N),
    N >= 0,
    N =< 255.



%check_rest/1

check_rest([L | Ls]) :-
    L \== '.',
    not(char_type(L, digit)).




% CANCELLA
% uguali([X | Xs], B) :-
%     X == B.

% file ends here
