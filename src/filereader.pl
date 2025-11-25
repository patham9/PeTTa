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
process_metta_string(S, Results, Space) :- re_replace("(;[^\n]*)"/g, "", S, Clean),
                                           string_codes(Clean, Codes),
                                           phrase(top_forms(Forms, 1), Codes),
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

%Collect characters until all parentheses are balanced (depth 0), accumulating codes also count newlines:
grab_until_balanced(D,Acc,Cs,LC0,LC2) --> [C], { ( C=0'( -> D1 is D+1 ; C=0') -> D1 is D-1 ; D1=D ), Acc1=[C|Acc],
                                                 ( C=10 -> LC1 is LC0+1 ; LC1 = LC0) },
                                          ( { D1=:=0 } -> { reverse(Acc1,Cs) , LC2 = LC1 } ; grab_until_balanced(D1,Acc1,Cs,LC1,LC2) ).

%Read a balanced (...) block if available, turn into string, then continue with rest, ignoring comments:
top_forms([],_) --> blanks, eos.
top_forms([Term|Fs], LC0) --> newlines(LC0, LC1),
                              ( "!" -> {Tag = bang} ; {Tag = form} ),
                              ( "(" -> [] ; string_without("\n", Rest), { format("Parse error: expected '(' or '!(', line ~w:~n~s~n", [LC1, Rest]), halt(1) } ),
                              ( grab_until_balanced(1, [0'(], Cs, LC1, LC2)
                                -> { true } ; string_without("\n", Rest), { format("Parse error: missing ')', starting at line ~w:~n~s~n", [LC1, Rest]), halt(1) } ),
                              { string_codes(FormStr, Cs), Term =.. [Tag, FormStr] },
                              top_forms(Fs, LC2).
