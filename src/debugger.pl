:- dynamic debug_category/1.
:- dynamic debug_goal_target/1.
:- dynamic silent/1.

init_runtime_flags(Args) :-
    retractall(silent(_)),
    retractall(debug_category(_)),
    retractall(debug_goal_target(_)),
    ( silent_requested(Args) -> assertz(silent(true)) ; assertz(silent(false)) ),
    debug_categories(Args, Categories),
    forall(member(Category, Categories), assertz(debug_category(Category))),
    debug_goal_targets(Args, Targets),
    forall(member(Target, Targets), assertz(debug_goal_target(Target))).

:- initialization((current_prolog_flag(argv, Args), init_runtime_flags(Args))).

silent_requested(Args) :-
    member(Arg, Args),
    memberchk(Arg, [silent, '--silent', '-s']).

debug_categories(Args, Categories) :-
    findall(Category, debug_category_arg(Args, Category), RawCategories),
    sort(RawCategories, Categories).

debug_category_arg(Args, all) :-
    member('--debug', Args).
debug_category_arg(Args, all) :-
    member('--debug-all', Args).
debug_category_arg(Args, Category) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug=', Spec, Arg),
    debug_category_spec(Spec, Category).

debug_category_spec(Spec, Category) :-
    atomic_list_concat(Parts, ',', Spec),
    member(Part, Parts),
    normalize_debug_category(Part, Category).

debug_goal_targets(Args, Targets) :-
    findall(Target, debug_goal_target_arg(Args, Target), RawTargets),
    sort(RawTargets, Targets).

debug_goal_target_arg(Args, Target) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-goal=', Spec, Arg),
    atomic_list_concat(Parts, ',', Spec),
    member(Part, Parts),
    Target = Part.

normalize_debug_category('source', source).
normalize_debug_category('parse', parse).
normalize_debug_category('compile', compile).
normalize_debug_category('translate', translate).
normalize_debug_category('runtime', runtime).
normalize_debug_category('runtime-leaf', runtime_leaf).
normalize_debug_category('runtime-fail', runtime_fail).
normalize_debug_category('space', space).
normalize_debug_category('space-mutation', space_mutation).
normalize_debug_category('space-match-fail', space_match_fail).
normalize_debug_category('space-get-atoms', space_get_atoms).
normalize_debug_category('result', result).
normalize_debug_category('all', all).

debug_enabled(Category) :-
    debug_category(all), !;
    runtime_category_alias(Category), !;
    space_category_alias(Category), !;
    debug_category(Category).

runtime_category_alias(runtime) :-
    debug_category(runtime_leaf),
    !.
runtime_category_alias(runtime) :-
    debug_category(runtime_fail).

space_category_alias(space) :-
    debug_category(space_mutation),
    !.
space_category_alias(space) :-
    debug_category(space_match_fail),
    !.
space_category_alias(space) :-
    debug_category(space_get_atoms).

legacy_verbose_enabled(Category) :-
    silent(false),
    \+ debug_enabled(Category).

runtime_event_enabled(_Stage, _Goal) :-
    ( debug_category(all)
    ; debug_category(runtime)
    ),
    !.
runtime_event_enabled(_Stage, Goal) :-
    debug_enabled(runtime_leaf),
    leaf_goal(Goal),
    !.
runtime_event_enabled(fail, _Goal) :-
    debug_enabled(runtime_fail),
    !.
runtime_event_enabled(_, _) :-
    fail.

runtime_goal_enabled(_) :-
    \+ debug_goal_target(_),
    !.
runtime_goal_enabled(Goal) :-
    goal_head_name(Goal, Head),
    debug_goal_target(Head).

space_event_enabled(add, _) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_mutation)
    ),
    !.
space_event_enabled(remove, _) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_mutation)
    ),
    !.
space_event_enabled(match, fail) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_match_fail)
    ),
    !.
space_event_enabled(match, success) :-
    ( debug_category(all)
    ; debug_category(space)
    ),
    !.
space_event_enabled(get_atoms, _) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_get_atoms)
    ),
    !.
space_event_enabled(_, _) :-
    fail.

debug_event(Category, Meta, Payload) :-
    debug_enabled(Category),
    !,
    render_debug_event(Category, Meta, Payload).
debug_event(_, _, _).

render_debug_event(source, meta(Index, Line, Kind), FormStr) :-
    debug_header(source, Index, Line, Kind),
    source_text(Kind, FormStr, Display),
    format(user_error, "~w~n~n", [Display]).
render_debug_event(parse, meta(Index, Line, Kind), parsed(ParsedType, Term)) :-
    debug_header(parse, Index, Line, Kind),
    format(user_error, "kind: ~w~n", [ParsedType]),
    swrite(Term, PrettyTerm),
    format(user_error, "~w~n~n", [PrettyTerm]).
render_debug_event(compile, meta(Index, Line, Kind), clause(Clause)) :-
    debug_header(compile, Index, Line, Kind),
    clause_text(Clause, Text),
    format(user_error, "~s~n", [Text]),
    nl(user_error).
render_debug_event(translate, meta(Index, Line, Kind), goals(Goals)) :-
    debug_header(translate, Index, Line, Kind),
    print_goals(Goals),
    nl(user_error).
render_debug_event(runtime, meta(Index, Line, Kind), goal(Stage, GoalIndex, Goal)) :-
    debug_header(runtime, Index, Line, Kind),
    goal_path_text(GoalIndex, GoalPathText),
    goal_indent(GoalIndex, Indent),
    format(user_error, "stage: ~w goal: ~w~n", [Stage, GoalPathText]),
    goal_text(Goal, Text),
    print_indented_block(Indent, Text),
    nl(user_error).
render_debug_event(space, meta(Index, Line, Kind), space(add, SpaceName, Term)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Term, PrettyTerm),
    format(user_error, "op: add space: ~w~n", [SpaceName]),
    format(user_error, "~w~n~n", [PrettyTerm]).
render_debug_event(space, meta(Index, Line, Kind), space(remove, SpaceName, Term, Removed)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Term, PrettyTerm),
    format(user_error, "op: remove space: ~w removed: ~w~n", [SpaceName, Removed]),
    format(user_error, "~w~n~n", [PrettyTerm]).
render_debug_event(space, meta(Index, Line, Kind), space(match, Stage, SpaceName, Pattern, Result)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Pattern, PrettyPattern),
    format(user_error, "op: match stage: ~w space: ~w~n", [Stage, SpaceName]),
    format(user_error, "pattern: ~w~n", [PrettyPattern]),
    ( nonvar(Result)
      -> swrite(Result, PrettyResult),
         format(user_error, "result: ~w~n~n", [PrettyResult])
      ; nl(user_error)
    ).
render_debug_event(space, meta(Index, Line, Kind), space(get_atoms, SpaceName, Pattern)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Pattern, PrettyPattern),
    format(user_error, "op: get-atoms space: ~w~n", [SpaceName]),
    format(user_error, "pattern: ~w~n~n", [PrettyPattern]).
render_debug_event(result, meta(Index, Line, Kind), Results) :-
    debug_header(result, Index, Line, Kind),
    ( Results == []
      -> format(user_error, "<no results>~n~n", [])
      ; forall(member(Result, Results), print_result_line(Result)),
        nl(user_error)
    ).

debug_header(Category, Index, Line, Kind) :-
    debug_header_label(Index, Line, Kind, Label),
    format(user_error, "[DEBUG ~w ~w]~n", [Category, Label]).

debug_header_label(space, 0, space, "space") :-
    !.
debug_header_label(Index, Line, Kind, Label) :-
    format(atom(Label), "#~w line ~w ~w", [Index, Line, Kind]).

source_text(runnable, FormStr, Display) :-
    !,
    format(atom(Display), "!~w", [FormStr]).
source_text(_, FormStr, FormStr).

print_result_line(Result) :-
    swrite(Result, PrettyResult),
    format(user_error, "~w~n", [PrettyResult]).

print_indented_block(Indent, Text) :-
    split_string(Text, "\n", "", Lines),
    forall(member(Line, Lines), print_indented_line(Indent, Line)).

print_indented_line(_, "") :-
    nl(user_error).
print_indented_line(Indent, Line) :-
    format(user_error, "~w~s~n", [Indent, Line]).

clause_text(Clause, Text) :-
    with_output_to(string(Text), portray_clause(current_output, Clause)).

goal_text(Goal, Text) :-
    strip_trace_wrappers(Goal, CleanGoal),
    clause_text((:- CleanGoal), Text).

goal_head_name(Goal, Head) :-
    strip_trace_wrappers(Goal, CleanGoal),
    nonvar(CleanGoal),
    ( atomic(CleanGoal)
      -> Head = CleanGoal
      ; compound(CleanGoal)
        -> functor(CleanGoal, Head, _)
    ).

goal_path_text(Path, Text) :-
    ( integer(Path)
      -> format(atom(Text), "#~w", [Path])
      ; is_list(Path)
        -> atomic_list_concat(Path, '.', Segments),
           format(atom(Text), "#~w", [Segments])
      ; format(atom(Text), "#~w", [Path])
    ).

goal_indent(Path, Indent) :-
    goal_depth(Path, Depth),
    Spaces is max(0, (Depth - 1) * 2),
    format(atom(Indent), "~*c", [Spaces, 0' ]).

goal_depth(Path, Depth) :-
    integer(Path),
    !,
    Depth = 1.
goal_depth(Path, Depth) :-
    is_list(Path),
    !,
    length(Path, Depth).
goal_depth(_, 1).

strip_trace_wrappers(Goal, CleanGoal) :-
    var(Goal),
    !,
    CleanGoal = Goal.
strip_trace_wrappers(trace_goal_execution(_, _, InnerGoal), CleanGoal) :-
    !,
    strip_trace_wrappers(InnerGoal, CleanGoal).
strip_trace_wrappers((A,B), (CleanA,CleanB)) :-
    !,
    strip_trace_wrappers(A, CleanA),
    strip_trace_wrappers(B, CleanB).
strip_trace_wrappers((A;B), (CleanA;CleanB)) :-
    !,
    strip_trace_wrappers(A, CleanA),
    strip_trace_wrappers(B, CleanB).
strip_trace_wrappers((A->B), (CleanA->CleanB)) :-
    !,
    strip_trace_wrappers(A, CleanA),
    strip_trace_wrappers(B, CleanB).
strip_trace_wrappers(findall(Template, Goal, Bag), findall(Template, CleanGoal, Bag)) :-
    !,
    strip_trace_wrappers(Goal, CleanGoal).
strip_trace_wrappers(once(Goal), once(CleanGoal)) :-
    !,
    strip_trace_wrappers(Goal, CleanGoal).
strip_trace_wrappers(Goal, Goal).

leaf_goal(Goal) :-
    strip_trace_wrappers(Goal, CleanGoal),
    \+ composite_goal(CleanGoal).

composite_goal((_,_)).
composite_goal((_;_)).
composite_goal((_->_)).
composite_goal(findall(_, _, _)).
composite_goal(once(_)).

print_goals([]) :-
    format(user_error, "<no goals>~n", []).
print_goals([Goal]) :-
    !,
    clause_text((:- Goal), Text),
    format(user_error, "~s", [Text]).
print_goals([Goal|Goals]) :-
    clause_text((:- Goal), Text),
    format(user_error, "~s", [Text]),
    print_goals(Goals).

runtime_option(Arg) :-
    memberchk(Arg, [silent, '--silent', '-s', mork, '--debug', '--debug-all']),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-goal=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug=', _, Arg).

file_argument(Args, File) :-
    member(File, Args),
    \+ runtime_option(File),
    !.

mork_enabled(Args) :-
    memberchk(mork, Args).
