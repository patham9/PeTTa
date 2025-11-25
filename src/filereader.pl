:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)). % re_replace/4
:- current_prolog_flag(argv, Args), ( (memberchk(silent, Args) ; memberchk('--silent', Args) ; memberchk('-s', Args))
                                      -> assertz(silent(true)) ; assertz(silent(false)) ).

%Read Filename into string S and process it (S holds MeTTa code):
load_metta_file(Filename, Results) :- load_metta_file(Filename, Results, '&self').
load_metta_file(Filename, Results, Space) :- read_file_to_string(Filename, S, []),
                                             process_metta_string(S, Results, Space).

%Extract function definitions, call invocations, and S-expressions part of &self space:
process_metta_string(S, Results) :- process_metta_string(S, Results, '&self').
process_metta_string(S, Results, Space) :- string_codes(S, SCodes),
                                           strip_comments_string_aware(SCodes, CleanCodes),
                                           phrase(top_forms(Forms, 1), CleanCodes),
                                           maplist(parse_form, Forms, ParsedForms),
                                           maplist(process_form(Space), ParsedForms, ResultsList), !,
                                           append(ResultsList, Results).

%First pass to convert MeTTa to Prolog Terms and register functions:
parse_form(form(S), parsed(T, S, Term)) :- sread(S, Term),
                                           ( Term = [=, [F|W], _], atom(F) -> register_fun(F), length(W, N), Arity is N + 1, assertz(arity(F,Arity)), T=function
                                                                            ; T=expression ).
parse_form(bang(S), parsed(bang, S, Term)) :- sread(S, Term).

%Second pass to compile / run / add the Terms:
process_form(Space, parsed(expression, _, Term), []) :- 'add-atom'(Space, Term, true).
process_form(_, parsed(bang, FormStr, Term), Result) :- translate_expr([collapse, Term], Goals, Result),
                                                        ( silent(true) -> true ; format("\e[33m-->   metta bang  -->~n\e[36m!~w~n\e[33m-->  prolog goal  -->\e[35m ~n", [FormStr]),
                                                                                 forall(member(G, Goals), portray_clause((:- G))),
                                                                                 format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ),
                                                        call_goals(Goals).
process_form(Space, parsed(function, FormStr, Term), []) :- add_sexp(Space, Term),
                                                            translate_clause(Term, Clause),
                                                            assertz(Clause, Ref),
                                                            ( silent(true) -> true ; format("\e[33m-->  metta func   -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [FormStr]),
                                                                                     clause(Head, Body, Ref),
                                                                                     ( Body == true -> Show = Head; Show = (Head :- Body) ),
                                                                                     portray_clause(current_output, Show),
                                                                                     format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ).
process_form(_, In, _) :- format('Failed to process form: ~w~n', [In]), halt(1).

%Like blanks but counts newlines:
newlines(C0, C2) --> blanks_to_nl, !, {C1 is C0+1}, newlines(C1,C2).
newlines(C, C) --> blanks.

%String-aware comment stripping (removes ; comments only when not in strings)
%
% State machine with two flags:
%   InString: true when between unescaped double quotes, false otherwise
%   Escaped:  true when previous char was backslash (only inside strings)
%
% State transitions:
%   Outside string + `;`  -> strip to end of line (comment)
%   Outside string + `"`  -> toggle InString to true
%   Inside string  + `\`  -> set Escaped flag
%   Inside string (escaped) + any char -> keep char, clear Escaped
%   Inside string  + `"`  -> toggle InString to false
%   Any other char        -> keep it
%
strip_comments_string_aware(Codes, Clean) :-
    strip_comments_state(Codes, false, false, Clean).

%Base case: end of input
strip_comments_state([], _, _, []).

%Inside string, escaped character - keep it
strip_comments_state([C|Rest], true, true, [C|Clean]) :-
    !,  % Deterministic: don't try catch-all clause
    strip_comments_state(Rest, true, false, Clean).

%Escape character while in string
strip_comments_state([0'\\|Rest], true, false, [0'\\|Clean]) :-
    !,  % Deterministic: don't try catch-all clause
    strip_comments_state(Rest, true, true, Clean).

%Quote - toggle string state
strip_comments_state([0'"|Rest], InString, false, [0'"|Clean]) :-
    !,  % Deterministic: don't try catch-all clause
    (InString == true -> InString1 = false ; InString1 = true),
    strip_comments_state(Rest, InString1, false, Clean).

%Semicolon while NOT in string - skip to end of line
strip_comments_state([0';|Rest], false, false, Clean) :-
    !,  % Deterministic: don't try catch-all clause
    skip_to_newline(Rest, AfterComment),
    strip_comments_state(AfterComment, false, false, Clean).

%Any other character - keep it (catch-all)
strip_comments_state([C|Rest], InString, _Escaped, [C|Clean]) :-
    strip_comments_state(Rest, InString, false, Clean).

%Skip to newline helper:
skip_to_newline([], []).
skip_to_newline([10|Rest], [10|Rest]) :- !.  %Keep the newline
skip_to_newline([_|Rest], After) :- skip_to_newline(Rest, After).

% =====================================================================
% FIXED VERSION: String-aware balanced parenthesis collector
% Fixes bug where parentheses inside strings were counted for balance
% =====================================================================

% Main entry point - starts with InString=false, Escaped=false
grab_until_balanced(D, Acc, Cs, LC0, LC2) -->
    grab_until_balanced_state(D, Acc, false, false, Cs, LC0, LC2).

% State-tracking version:
%   D = depth counter for parentheses
%   Acc = accumulator for characters
%   InString = true if currently inside a string literal
%   Escaped = true if previous char was backslash (inside string)
grab_until_balanced_state(D, Acc, InString, Escaped, Cs, LC0, LC2) -->
    [C],
    {
        % Update line counter if newline
        ( C=10 -> LC1 is LC0+1 ; LC1 = LC0 ),

        % Handle string state transitions and paren counting
        (
            % Case 1: We're escaped - consume char and clear escape flag
            Escaped = true
        ->  InString1 = InString,
            Escaped1 = false,
            D1 = D

        % Case 2: Escape sequence starting (only matters in strings)
        ;   C = 0'\\, InString = true
        ->  InString1 = InString,
            Escaped1 = true,
            D1 = D

        % Case 3: Quote character - toggle string state (if not escaped)
        ;   C = 0'"
        ->  ( InString = true -> InString1 = false ; InString1 = true ),
            Escaped1 = false,
            D1 = D

        % Case 4: Opening paren (only count if not in string)
        ;   C = 0'(, InString = false
        ->  D1 is D + 1,
            InString1 = InString,
            Escaped1 = false

        % Case 5: Closing paren (only count if not in string)
        ;   C = 0'), InString = false
        ->  D1 is D - 1,
            InString1 = InString,
            Escaped1 = false

        % Case 6: Any other character
        ;   D1 = D,
            InString1 = InString,
            Escaped1 = false
        ),

        % Add character to accumulator
        Acc1 = [C|Acc]
    },

    % Check if we're done (depth = 0 and not in string)
    (   { D1 =:= 0, InString1 = false }
    ->  { reverse(Acc1, Cs), LC2 = LC1 }
    ;   grab_until_balanced_state(D1, Acc1, InString1, Escaped1, Cs, LC1, LC2)
    ).

%Read a balanced (...) block if available, turn into string, then continue with rest, ignoring comments:
top_forms([],_) --> blanks, eos.
top_forms([Term|Fs], LC0) --> newlines(LC0, LC1),
                              ( "!" -> {Tag = bang} ; {Tag = form} ),
                              ( "(" -> [] ; string_without("\n", Rest), { format("Parse error: expected '(' or '!(', line ~w:~n~s~n", [LC1, Rest]), halt(1) } ),
                              ( grab_until_balanced(1, [0'(], Cs, LC1, LC2)
                                -> { true } ; string_without("\n", Rest), { format("Parse error: missing ')', starting at line ~w:~n~s~n", [LC1, Rest]), halt(1) } ),
                              { string_codes(FormStr, Cs), Term =.. [Tag, FormStr] },
                              top_forms(Fs, LC2).
