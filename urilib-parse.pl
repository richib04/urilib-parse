% -*- Mode: Prolog -*-

% Studenti
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

    % PROBLEMI QUI
    get_userinfo(Rest1, UserInfo, Rest2, IsStandard, Bool2),

    URI = uri(Scheme, UserInfo, Rest2, _, _, _, _).



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



% get_userinfo/3
% divide la lista in due liste, una contenente (se c'è) lo userinfo
% l'altra contenente il resto della lista

% DA PROBLEMI, DA RIFARE

get_userinfo(L, Y, Rest, IsStandard, Bool) :-
    IsStandard == 0,
    Y is [].

get_userinfo(L, Y, Rest, IsStandard, Bool) :-
    IsStandard == 1,
    get_userinfo(L, Y, Rest, Bool).

get_userinfo([/ | Ls], Y, Rest, Bool) :-
    get_userinfo(Ls, Y, Rest, Bool).

get_userinfo([L | Ls], [L | Ys], Rest, Bool) :-
    chek_identificatore(L),
    L \== @,
    get_userinfo(Ls, Ys, Rest, 1, Bool).

get_userinfo([@ | Ls], [], Ls, Bool) :-
    Bool is 1.

%get_userinfo([], [], Rest, Bool) :-
%    Bool is 0.



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





% CANCELLA
% uguali([X | Xs], B) :-
%     X == B.

% file ends here
