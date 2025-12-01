!(import_prolog_function consult)

(= (import_prolog_functions_from_file $prologfile $importfunctions) (let* (($prologfile (consult))
                                                                           ($function (superpose $importfunctions)))
                                                                          (import_prolog_function $function)))

!(import_prolog_function use_module)

(= (import_prolog_functions_from_module $prologfile $importfunctions) (let* (($prologfile (use_module))
                                                                             ($function (superpose $importfunctions)))
                                                                            (import_prolog_function $function)))

!(import_prolog_functions_from_file ./lib/lib_prolog (static-import!))
