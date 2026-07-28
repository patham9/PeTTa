:- use_module(library(heaps)).

%PeTTa functions use the final Prolog argument as their result. Hide the heap
%key output so the native minimum lookup has one input and one result.
heap_peek_native(Heap, Value) :- min_of_heap(Heap, _, Value).
