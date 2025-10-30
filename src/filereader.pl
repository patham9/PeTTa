:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)).

%Read Filename into string S and process it (S holds MeTTa code):
load_metta_file(Filename, Results) :- read_file_to_string(Filename, S, []),
                                      process_metta_string(S, Results).

process_metta_string(S, Results) :- re_replace("(;[^\n]*)"/g, "", S, Clean),
                                    string_codes(Clean, Codes),
                                    phrase(top_entities(Entities,1), Codes),
                                    maplist(parse_entity, Entities, ParsedEntities),
                                    maplist(process_entity_with_error, ParsedEntities, ResultsList), !,
                                    append(ResultsList, Results).

%First pass to convert MeTTa to Prolog Terms and register functions
parse_entity(form(FormStr), parsed(Type, FormStr, Term)) :- sread_with_error(FormStr, Term), register_if_fun(Term, Type).
parse_entity(bang(FormStr), parsed(bang, FormStr, Term)) :- sread_with_error(FormStr, Term).

sread_with_error(FormStr, Term) :- ( sread(FormStr, Term) -> true ; format('Parse error in form: ~w~n', [FormStr]), fail ).

register_if_fun([=, [FAtom|_], _],function) :- atom(FAtom), register_fun(FAtom).
register_if_fun(_,expression).

%Second pass to compile / run / add the Terms
process_entity_with_error(In, Out) :- ( process_entity(In, Out) -> true ; format('Failed to process entity: ~w~n', [In]), halt(1) ).

process_entity(parsed(function, FormStr, Term), []) :- assert_function_form(Term, Ref), 
                                                       show_compiled_function(FormStr,Ref).
process_entity(parsed(expression, _, Term), []) :- 'add-atom'('&self', Term, true).
process_entity(parsed(bang, _, Term), [Result]) :- eval([collapse, Term], Result).

%Translate the clause and assert it
assert_function_form(Term, Ref) :- add_sexp('&self', Term),
                                   translate_clause(Term, Clause),
                                   assertz(Clause, Ref).

show_compiled_function(FormStr,Ref) :- current_prolog_flag(argv, Args),
                                       ( ( memberchk(silent, Args) ; memberchk('--silent', Args) ; memberchk('-s', Args) )
                                       -> true
                                       ; format("\e[33m-->  metta S-exp  -->~n\e[36m~w~n\e[33m--> prolog clause -->~n\e[32m", [FormStr]),
                                         clause(Head, Body, Ref),
                                         ( Body == true -> Show = Head; Show = (Head :- Body) ),
                                         portray_clause(current_output, Show),
                                         format("\e[33m^^^^^^^^^^^^^^^^^^^^^~n\e[0m") ).

%Read balanced entities, distinguishing between regular forms and !-forms:
top_entities([],_) --> blanks, eos.
top_entities([Term|Fs],LC) --> newlines(LC,LC1), 
                               ( ( "!" -> { Tag = bang , E = "!" } ; { Tag = form , E = "" } ), "("
                                  -> {true} ; here_line(Rest) , {format("Parse error: expected start of statment either '!(' or '(' found line ~w:~n~w~n", [LC1,Rest]), halt(1)} ),
                               (grab_until_balanced(1, [0'(], Cs, LC1, LC2)
                                  -> {true} ; here_line(Rest) , {format("Parse error: missing ) starting at line ~w:~n~w(~w~n", [LC1,E,Rest]), halt(1)} ),
                               { string_codes(FormStr, Cs), Term =.. [Tag, FormStr] },
                               top_entities(Fs,LC2).
 
%Collect characters until all parentheses are balanced (depth 0), accumulating codes also count newlines
grab_until_balanced(D,Acc,Cs,LC0,LC2) --> [C], { ( C=0'( -> D1 is D+1 ; C=0') -> D1 is D-1 ; D1=D ), Acc1=[C|Acc] ,
                                                 ( C=10 -> LC1 is LC0+1 ; LC1 = LC0) },
                                          ( { D1=:=0 } -> { reverse(Acc1,Cs) , LC2 = LC1 } ; grab_until_balanced(D1,Acc1,Cs,LC1,LC2) ).

%  Capture the rest of the current line
here_line(Str, S0, S0) :- ( append(LineCodes, [10|_], S0) ->  true ; LineCodes = S0 ), string_codes(Str, LineCodes).

% like blanks but counts newlines
newlines(C0, C2) --> blanks_to_nl, !, {C1 is C0+1}, newlines(C1,C2).
newlines(C,C) --> blanks.

