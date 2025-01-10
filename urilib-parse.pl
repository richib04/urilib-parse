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
    
    get_userinfo(Rest2, UserInfoAt, Rest3, IsStandard),

%    to_string_if_not_empty(UserInfoAt, UserInfo),

    get_host(Rest3, Host, Rest4, IsStandard),
    
    URI = uri(Scheme, UserInfoAt, Host, Rest4, IsStandard, _, _).



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

get_userinfo(L, [], L, Check) :-
    Check == 0.

get_userinfo(Ls, [], Ls, 1) :-
    not(find_snail(Ls)).

get_userinfo([@ | Ls], [], Ls, 1).

get_userinfo([L | Ls], [L | Ys], Rest, 1) :-
    L \== @,
    chek_identificatore(L),
    get_userinfo(Ls, Ys, Rest, 1).



% get_host/4

get_host(L, [], L, 0).

get_host(L, H, Rest, 1) :-
    get_host(L, H, Rest).

get_host([L | Ls], [L | Ys], Rest) :-
    char_type(L, alpha),
    get_host_name(Ls, Ys, Rest).

get_host([L | Ls], [L | Ys], Rest) :-
    char_type(L, digit),
    het_host_ip().



% get_host_name/3

get_host_name([L | Ls], [L | Ys], Rest) :-
    check_iden_host(L),
    get_host_name(Ls, Ys, Rest).

get_host_name([], [], []).

get_host_name([L | Ls], [], [L | Ls]) :-
    not(char_type(L, digit)).



% get_host_ip/3

get_host_ip().



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



find_snail([L | Ls]) :-
    L \== @,
    find_snail(Ls).






% CANCELLA
% uguali([X | Xs], B) :-
%     X == B.

% file ends here
