#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

swipl -q -g "consult('$ROOT/src/main.pl'),message_queue_create(_,[alias(loader_context_probe)]),assertz(translator_rule(loader_pause_probe)),assertz((loader_pause_probe(Gs):-thread_send_message(loader_context_probe,entered),thread_get_message(loader_context_probe,continue),Gs=done)),thread_create(translate_runnable_expr([loader_pause_probe],_,_),Id,[]),thread_get_message(loader_context_probe,entered),retractall(symbol_head(loader_clause_symbol,_)),translate_clause([=,[loader_clause_function],[loader_clause_symbol,1]],_),findall(Context,symbol_head(loader_clause_symbol,Context),Contexts),thread_send_message(loader_context_probe,continue),thread_join(Id,_),(Contexts==[clause]->true;throw(error(shared_runnable_translation_context,Contexts))),halt"

# A nested runnable translation must remove only its own context marker.
swipl -q -g "consult('$ROOT/src/main.pl'),assertz(translator_rule(nested_context_probe)),assertz((nested_context_probe(Gs):-translate_runnable_expr([nested_inner_symbol],_,_),translate_expr([nested_outer_symbol],Gs,_))),retractall(symbol_head(nested_outer_symbol,_)),translate_runnable_expr([nested_context_probe],_,_),findall(Context,symbol_head(nested_outer_symbol,Context),Contexts),(Contexts==[runnable]->true;throw(error(lost_outer_runnable_context,Contexts))),halt"

printf 'loader concurrency checks passed\n'
