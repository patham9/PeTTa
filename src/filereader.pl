:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)).     % re_replace/4

%Read Filename into string S and process it (S holds MeTTa code):
load_metta_file(Filename) :- read_file_to_string(Filename, S, []),
                             process_metta_string(S).

%Replace !(EXP) with (= (run) (EXP)):
re_replace_all(PFrom, PTo, S, Out) :- re_replace(PFrom, PTo, S, S1),
                                      ( S1 \== S -> re_replace_all(PFrom, PTo, S1, Out)
                                                  ; Out = S ).

%Extract function definitions and process each, whereby !(Z) is transformed to (= (run) (Z)):
process_metta_string(S) :- split_string(S, "\n", "", L0),
                           findall(C, (member(L,L0), split_string(L,";","",[C|_])), L1),
                           atomic_list_concat(L1, '\n', CodeWithoutComment),
                           re_replace_all("(?m)^\\s*!\\s*\\(((?:[^()]|\\((?-1)\\))*)\\)",
                                          "(= (run) (\\1))", CodeWithoutComment, FunctionizedCode),
                           string_codes(FunctionizedCode, Codes),
                           phrase(top_forms(Forms), Codes),
                           maplist(assert_function, Forms).

%Functions stay functions and runaway S-expressions become add-atom calls with result omitted:
to_function_form(T, T) :- T = [=, [_|_], _], !.
to_function_form(T, [=, [run], [['add-atom','&self', T], [empty]]]).

%From a function string: parse, extract first atom as name, register, transform to relation, add to buffer:
:- dynamic fun_buffer_file/1.
assert_function(FormStr) :- sread(FormStr, Orig),
                            to_function_form(Orig, Term),
                            Term = [=, [FAtom|_], _BodyExpr],
                            atom(FAtom),
                            register_fun(FAtom),
                            assertz(compiled(FAtom)),
                            translate_clause(Term, Clause),
                            ( fun_buffer_file(_) -> true
                                                  ; tmp_file(pl, File),
                                                    open(File, write, Stream),
                                                    asserta(fun_buffer_file(File)),
                                                    nb_setval(fun_buffer_stream, Stream) ),
                            nb_getval(fun_buffer_stream, Stream),
                            portray_clause(Stream, Clause),
                            flush_output(Stream),
                            ( current_prolog_flag(argv, Args) -> true ; Args = [] ),
                            ( \+ ( member(Flag, Args),
                                   (Flag == silent ; Flag == '--silent' ; Flag == '-s') )
                              -> format("~w~n----> staged for compilation~n", [FormStr])
                               ; true ).

%"Assert" functions from buffer but via static code loading for higher performance:
compile_metta_code :- fun_buffer_file(File),
                      nb_getval(fun_buffer_stream, Stream),
                      close(Stream),
                      retractall(fun_buffer_file(_)),
                      nb_delete(fun_buffer_stream),
                      load_files(File, [silent(true), if(not_loaded)]),
                      delete_file(File),
                      ( current_prolog_flag(argv, Args) -> true ; Args = [] ),
                      ( \+ ( member(Flag, Args),
                             (Flag == silent ; Flag == '--silent' ; Flag == '-s') )
                             -> forall(compiled(Name), ( format('~n--- ~w ---~n', [Name]), listing(Name)))
                              ; true ).


%Collect characters until all parentheses are balanced (depth 0), accumulating codes:
grab_until_balanced(D,Acc,Cs) --> [C], { ( C=0'( -> D1 is D+1 ; C=0') -> D1 is D-1 ; D1=D ), Acc1=[C|Acc] },
                                  ( { D1=:=0 } -> { reverse(Acc1,Cs) } ; grab_until_balanced(D1,Acc1,Cs) ).

%Read a balanced (...) block if available, turn into string, then continue with rest, ignoring comment lines:
top_forms([])     --> blanks, eos.
top_forms([F|Fs]) --> blanks, "(", grab_until_balanced(1, [0'(], Cs), { string_codes(F, Cs) }, top_forms(Fs).
