%%%%%%%%%% Compile-time typechecking support (see AGENTS.md) %%%%%%%%%%
%
% The checker is one body of code in twelve files, spliced together with
% include/1 - textual inclusion, so loading this file is exactly equivalent
% to loading the concatenation, directive order and clause contiguity
% included. The order below is load-bearing; add new code to the file whose
% subject owns it.

:- include('typecheck/flags_arrows.pl').   %mode flags, oracle switches, the arrow atoms
:- include('typecheck/decl_store.pl').     %declaration store, caching, LATE declarations
:- include('typecheck/type_lang.pl').      %type_unify/2, brands, unions, knowledge attrs, markers
:- include('typecheck/value_checks.pl').   %value typing, check_value/3, tuples, call-site args
:- include('typecheck/clause_checks.pl').  %clause output certification, strict gates, narrowing
:- include('typecheck/ctor_snapshots.pl'). %constructor-set snapshots and revalidation
:- include('typecheck/oracles.pl').        %--oracle-det cardinality oracle
:- include('typecheck/inference.pl').      %param promises, local inference for undeclared fns
:- include('typecheck/det_validate.pl').   %committed-arrow validation, boundness enforcement
:- include('typecheck/det_builtins.pl').   %the builtin determinism table, read off the sources
:- include('typecheck/det_args.pl').       %argument-aware verdicts, manifest shapes, HO closures
:- include('typecheck/det_analysis.pl').   %body analysis: lattice, case coverage, exhaustiveness
