%Translate a MeTTa S-expression file (no code, no bangs) to prolog predicates to load:
metta_file_to_prolog(Input, Space, Output) :- setup_call_cleanup( open(Input, read, In),
                                                                  setup_call_cleanup( open(Output, write, Out),
                                                                                      convert_stream(In, Out, Space),
                                                                                      close(Out) ),
                                                                  close(In) ).

%Process stream line by line
convert_stream(In, Out, Space) :- read_line_to_string(In, Line),
                                  ( Line == end_of_file
                                    -> true
                                     ; convert_line(Line, Space, Out),
                                       convert_stream(In, Out, Space) ).

%Perform simple transformation from S-Expression to space-rel predicate:
convert_line(Line0, Space, Out) :- sub_string(Line0, 1, _, 1, Inner0),
                                   replace_all("(", "[", Inner0, Inner1),
                                   replace_all(")", "]", Inner1, Inner2),
                                   replace_all(" ", ",", Inner2, Inner3),
                                   format(Out, "'~w'(~w).~n", [Space, Inner3]).

%Helper predicate for string repkacement:
replace_all(P, R, S, O) :- split_string(S, P, "", Parts),
                           atomic_list_concat(Parts, R, O).

%The static import function that allows loading static data files fast:
'static-import!'(Space, File, true) :- style_check(-discontiguous),
                                       atom_string(File, SFile),
                                       working_dir(Base),
                                       atomic_list_concat([Base, '/', SFile, '.qlf'], QlfFile),
                                       atomic_list_concat([Base, '/', SFile, '.pl'], PlFile),
                                       atomic_list_concat([Base, '/', SFile, '.metta'], MettaFile),
                                       ( exists_file(QlfFile)
                                         -> % Case 1: .qlf exists → load fastest
                                            consult(QlfFile)
                                          ; exists_file(PlFile)
                                         -> % Case 2: .pl exists → compile to qlf and load
                                            qcompile(PlFile),
                                            consult(QlfFile)
                                          ; % Case 3: .pl does not exist → generate from .metta then compile
                                            metta_file_to_prolog(MettaFile, Space, PlFile),
                                            qcompile(PlFile),
                                            consult(QlfFile) ).
