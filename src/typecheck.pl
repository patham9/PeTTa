%%%%%%%%%% Compile-time typechecking support (see AGENTS.md) %%%%%%%%%%
%
% The checker is organized as separately loaded ownership units. Predicate
% definitions are never interleaved across files; each persistent store has an
% explicit dynamic/thread_local declaration in its owning file. Consequently
% the order below is organizational, not semantic. The boundary matrix loads
% the non-module units in a different order and rejects predicates defined in
% more than one unit.
%
% Real module:
%   builtin_registry.pl  declarative builtin metadata and consistency checks
%
% Documented non-module boundaries (kept in `user` while translator/metta
% still consume their interfaces directly):
%   flags_arrows.pl      modes and canonical arrow syntax
%   decl_store.pl        canonical declaration store and lifecycle
%   type_lang.pl         type language, normalization, compatibility, attrs
%   value_checks.pl      static/deferred value checks and call-site guards
%   clause_checks.pl     clause patterns, contextual results, output checks
%   inference.pl         undeclared inference and parametric promises
%   oracles.pl           runtime soundness/cardinality oracles
%   analysis_proofs.pl   functional proof records and memo boundary
%   det_builtins.pl      effective builtin/call effect lookup
%   det_proofs.pl        determinism walker, certificates, procedural rules
%   det_analysis.pl      effect flow, coverage, whole-set validation
%   det_validate.pl      committed-arrow validation and bound provisos
%   dependency_graph.pl  compiled dependencies and mutation invalidation

:- use_module('typecheck/builtin_registry.pl').

:- ensure_loaded([
       'typecheck/analysis_proofs.pl',
       'typecheck/clause_checks.pl',
       'typecheck/decl_store.pl',
       'typecheck/dependency_graph.pl',
       'typecheck/det_analysis.pl',
       'typecheck/det_builtins.pl',
       'typecheck/det_proofs.pl',
       'typecheck/det_validate.pl',
       'typecheck/flags_arrows.pl',
       'typecheck/inference.pl',
       'typecheck/oracles.pl',
       'typecheck/type_lang.pl',
       'typecheck/value_checks.pl'
   ]).
