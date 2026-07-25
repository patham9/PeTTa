## PeTTa

Efficient MeTTa language implementation in Prolog.

Please check out the [Wiki](https://github.com/patham9/PeTTa/wiki) for more information.

### Dependencies

- SWI-Prolog >= 9.3.x
- Python 3.x (for janus Python interop)

### Usage

Example run:

`time sh run.sh ./examples/nars_tuffy.metta`

### MORK and FAISS spaces

If MORK and FAISS is installed, execute `sh build.sh` to support MORK-based atom spaces and FAISS-based atom-vector spaces.

The following projects are cloned and built by build.sh:

**Repository:** [mork_ffi](https://github.com/patham9/mork_ffi) dependent on [trueagi-io/mork](https://github.com/trueagi-io/mork)

**Repository:** [faiss_ffi](https://github.com/patham9/faiss_ffi) dependent on [facebookresearch/faiss](https://github.com/facebookresearch/faiss)

### Extension libraries

Please check out [Extension libraries](https://github.com/trueagi-io/PeTTa/wiki/Extension-libraries) for a set of extension libraries that can be invoked from MeTTa files directly from the git repository.

### Compile-time typechecking

PeTTa checks `(: f (-> A B))` declarations during translation: provably
ill-typed programs are rejected at compile time with the file, line, and
offending form, and fully resolved calls compile to plain Prolog with **no
runtime type checks**. Runtime guards remain only where types are genuinely
unknown at the call site; they check bound values through the user-extensible
`get-type` reflection, so runtime refinement types keep working.

```metta
(: inc (-> Number Number))
(= (inc $x) (+ $x 1))

!(inc "oops")   ; rejected at compile time: Type mismatch: got "oops" but expected 'Number'
```

Types make code *faster*, not slower: resolved arithmetic fuses to native
`is/2` and comparisons fuse into if-conditions, so an annotated (or inferred,
see below) `fib` compiles to the same clause a Prolog programmer would write
by hand — about twice as fast as the untyped translation was before.

**Inference.** Undeclared functions get local type inference (parameters are
inferred from how the body uses them, including through self-recursion).
Inferred types only ever *add* knowledge — they eliminate guards, type call
outputs, and enable the fused code generation — but never reject a program:
a call that statically mismatches an inferred type fails at run time exactly
as it would have without inference.

**Match schemas.** Space reads are typed through declared relation schemas:
with `(: age (-> String Number Atom))` declared, the pattern variables of
`(match &self (age $who $n) ...)` acquire `String` and `Number`, so match
results are fully typed and guard-free code is generated for the body. Type
queries like `(match &self (: $x Fruit) $x)` bind `$x : Fruit` directly.

**Strict mode.** `sh run.sh file.metta --strict` additionally requires every
compiled function to have a declared or inferable type and rejects compilation
if any residual runtime type guard would be emitted — a machine-checked
guarantee that the compiled program contains no *implicit* runtime type checks.

**Type ascription.** Genuinely dynamic values (reads from schema-less
relations, `catch` results, `eval`) can be given an author-stated type with
`(the Type Expr)`: the checker treats the type as knowledge for everything
downstream and emits one explicit, visible runtime check at the boundary —
permitted even under `--strict`. An ascription that contradicts what the
checker already knows is a compile-time error.

```metta
(: greet (-> String))
(= (greet) (the String (match &self (name $n) $n)))
```

**Auditing runtime checks.** `--warn-runtime-checks` (independent of the other
flags) prints a warning for every runtime type check the compiler emits —
implicit residual guards and explicit `(the ...)` ascriptions alike, each with
the containing function and the checked type. Combined with `--strict` (which
already forbids the implicit ones) a warning-free compile certifies that the
program contains no runtime type checks at all.

**Structural tuple types.** A parenthesized type describes an expression, in
one of two readings — and which one you get is decided by the *declaration* of
its head atom, not by how the atom is spelled:

- **Tagged** — the value must carry the head as a literal atom. Chosen when the
  head is a declared constructor of matching arity, or when it has no type
  declaration at all. `(STV Number Number)` with `(: STV (-> Number Number TV))`
  declared, and `(Stats Number Number Number)` with `Stats` undeclared, both
  describe the three-element expressions `(STV 0.5 0.8)` and `(Stats 1.0 2.0 3)`.
- **Positional** — an n-element record whose i-th element has the i-th listed
  type, head included. Chosen when the head is not an atom (`($value Number)`),
  is a primitive or wildcard type (`(Number Number)`), or is an atom declared as
  something *other* than a constructor of that arity — typically a type name:

```metta
(: Statement Type)   (: KBContext Type)   (: TV Type)

(: fact-conf (-> (Statement KBContext TV) Number))   ; three fields
(= (fact-conf ($_statement $_context $tv)) (tv-conf $tv))
```

So: declare the field types of a positional tuple, and leave a tag undeclared
(or declare it as a constructor). See `examples/strict_tuple_types.metta` and
`examples/strict_positional_tuple_types.metta`.

**Union types.** A heterogeneous position can declare its alternatives with
`(| T1 T2 ...)` — for example `(List (| (CPU %Undefined% %Undefined%
%Undefined%) (: %Undefined% %Undefined% %Undefined% TV)))` for a mixed
execution list. A value fits a union if it fits some member, and `case` or
clause-head patterns narrow to the member their shape selects, typing the
pattern's variables. Unions are declared, never inferred.

A *tagged* pattern narrows by its tag. An *untagged* one — `($type $ctx $prf
$tv)` — only counts elements, which by itself proves nothing: another member's
constructor may build a value of exactly that width. Such a pattern narrows
only when every other member is ruled out, either because no constructor of it
has that width, or because an earlier branch of the same `case` already
consumed all of them (`case` is first-match: the branches compile to a nested
if-then-else, so a value matched earlier never reaches a later branch). Move
the untagged branch first and the narrowing is refused. Both directions are
pinned by `examples/strict_union_prior_branch_narrowing.metta` and
`examples/fail_strict_union_var_branch_first.metta`.

*Constructor* here means what it means everywhere else in PeTTa: a declared
symbol with **no** equations, which therefore stays literal data. A declared
symbol that has equations is a function — `(make-goal $f $a $r)` is rewritten
at the call site and no such expression ever survives as a value — so a helper
that merely *returns* the type does not count as one of its constructors and
does not block an exclusion. Give that same helper a declaration but no
equation and it becomes a genuine constructor that does block. See
`examples/strict_union_reducible_helper_not_ctor.metta` and
`examples/fail_strict_union_undefined_second_ctor.metta`. Definition order
inside one file is irrelevant (definedness is recorded in the parse prepass),
but the exclusion otherwise reads the constructor set as it stands at that
point in the compilation, so declare a type's constructors before the code
that matches on it.

**Erased nominal newtypes.** `(: KB (Newtype Expression))` declares a
distinct compile-time role over an existing representation: nothing is
wrapped at runtime and no guards are emitted. A branded value fits its
representation, but different brands never unify merely because their
representations do — swapping a `Proof` into a `KB` position is a compile
error. Raw literals and constructed values acquire a brand contextually from
the expected position; an unknown variable does not (under `--strict`) —
brand it explicitly with `(brand KB $x)`, an erased trust operation that
rejects conflicting brands but generates no check (a role has no runtime
witness). Declared relation schemas restore brands on `match`. Use
`(the KB ...)` instead when you want the representation checked at runtime.

**Determinism arrows.** `(: f (-[det]-> A B C))` — prefix, like `->` and
every other MeTTa form — declares a deterministic function: the compiler validates
that its clauses cannot overlap and its body is deterministic, then commits to
the first matching clause — guaranteeing choicepoint-free execution and
constant-memory deep recursion via last-call optimization
(see `examples/determinism_lco.metta`). `-[nondet]->` documents intentional
nondeterminism. Result cardinality is a total order: `-[det]->` (exactly one)
< `-[semidet]->` (zero or one) < `-[nondet]->` (any), and a closure fits an
arrow that allows at least as many results as it can produce.

**Inferred closure determinism (in every mode).** A closure over an *undeclared*
function — an inline `|->` lambda, or a bare/underapplied reference to a function
with no arrow declaration — has no written arrow, so the compiler derives one
from the same clause-set analysis: a body that provably yields exactly one result
gets `-[det]->`, one that may fail but never branch gets `-[semidet]->`, and
anything unproven claims nothing. That proof is worth the same in every mode:
`--strict-det` exists to force a determinism claim out of code that made none,
not to be a precondition for checking one you already wrote. So passing an inline
det lambda to a `-[det]->` parameter resolves at compile time under plain
`--strict` — no residual runtime check for `--strict` to reject. It is a proof,
not a rubber stamp: a `superpose` body still does not fit. See
`examples/strict_det_closure_inferred.metta`,
`examples/strict_semidet_closure_inferred.metta` and
`examples/fail_strict_nondet_closure_inferred.metta`.

**`-[semidet]->`: partial, but still committed.** `-[semidet]->` is the answer
to "I want a function that may legitimately have no answer" — a lookup with no
entry, a division by zero, a parse that does not apply. Its body is checked
exactly like a `-[det]->` body except that it is allowed to FAIL: `(empty)` and
calls to other `-[semidet]->` functions are fine, while `superpose`, `match`
and overlapping clause heads stay rejected, because failing is not the same as
answering twice. **It costs nothing.** semidet commits to its first matching
clause exactly like det — the right to fail leaves no choicepoint — so it keeps
choicepoint-free execution and last-call optimization
(`examples/strictdet_semidet_lco.metta` is `determinism_lco.metta` with the
arrow changed, one million levels deep). Reaching for `-[nondet]->` instead is
what actually costs: it throws away the commit for a nondeterminism that is not
there. A `-[det]->` function may not call a `-[semidet]->` one — that is
precisely the promise `-[det]->` makes — so partiality is visible in the types
all the way up. See `examples/strictdet_semidet_arrow.metta`.

**Exhaustiveness of `-[det]->` (under `--strict-det`).** Under `--strict-det`
only, a `-[det]->` function whose clauses PROVABLY cannot match some input of
its declared argument types is a compile error naming the unmatched case and
suggesting `-[semidet]->`. Two things are provable: an argument position every
clause pins to a literal of an uncoverable domain (`(= (lookup 1) 10)`,
`(= (lookup 2) 20)` over `Number`), and a position discriminating on the
constructors of a nominal type whose constructor set is known in full, with one
constructor unmatched. Everything else is accepted in silence — variables and
wildcards, guards, arithmetic conditions, `Atom`/`Expression`/`%Undefined%`
arguments, and any type whose constructors cannot be enumerated.

The check is deliberately one-sided: **provably incomplete is an error, cannot
tell is accepted**. It is not GHC's "warn unless proven exhaustive" — PeTTa's
nominal types are open, a constructor may be declared in a later file, so
"cannot tell" is the normal case and erroring on it would reject working code
with no way out. What the check reports is therefore a lower bound on
incompleteness, never a totality guarantee: it can only see the constructors
declared before it, and it judges a function on the clauses visible in the file
being loaded. `examples/strictdet_det_exhaustive_limits.metta` pins the
conservative side; `examples/fail_strictdet_nonexhaustive_det.metta` and
`fail_strictdet_nonexhaustive_ctor.metta` pin the two provable ones. Only an
explicit `-[det]->` is checked — under `--strict-det` a plain `->` reads as
deterministic too, but that is a mode-wide default rather than a per-function
promise of totality.

**Strict determinism mode.** `--strict-det` (implies `--strict`) makes a
plain `->` itself a determinism commitment: every declared function is
validated as deterministic unless its arrow says `-[nondet]->`. Overlapping
clause heads, `superpose`/`match` bodies and dynamic `eval` become compile
errors on `->` functions — in MeTTa every matching clause fires, so each such
error is either an accidental source of multiple results or a missing
`-[nondet]->`. Closure parameters carry the same commitment: a
`(-> $a $b)`-typed parameter may be applied inside a deterministic body,
a `-[nondet]->` one may not. A clause that commits with `(cut)` may overlap
with later clauses. See `examples/strictdet_basics.metta`. This flag also
turns on the `-[det]->` exhaustiveness check described above.

Notes and caveats:

- Function type declarations are pre-cached per file, so helpers may be
  declared and defined after their callers within the same file. Across files,
  imports must still precede use. Value declarations like `(: a A)` stay
  order-sensitive (they are knowledge atoms), and a function declaration
  arriving after its function was already compiled in an earlier file warns
  and has no retroactive effect.
- `Expression`-typed arguments are passed unevaluated (as data), except
  underapplied closures like `(+ 1)`.
- Interpreters that call `eval` per iteration pay for a (typed) translation
  each time — that pattern was always slow and types do not change it.
- The executable specification lives in `examples/type_*.metta`,
  `examples/fail_*.metta` (must fail compilation), `examples/strict_*.metta`,
  and `examples/type_dispatch_matrix.sh`, which asserts properties of the
  generated code itself.

## Notebooks, Servers, Browser

### Jupyter Notebook Support

A Jupyter kernel for PeTTa is available in a separate repository for interactive MeTTa development in notebooks.

**Repository:** [trueagi-io/jupyter-petta-kernel](https://github.com/trueagi-io/jupyter-petta-kernel)

Quick install:

```bash
# Set PETTA_PATH to this PeTTa installation
export PETTA_PATH=/path/to/PeTTa

# Clone and install the kernel
git clone https://github.com/trueagi-io/jupyter-petta-kernel.git
cd jupyter-petta-kernel
./install.sh
```

Please see the [jupyter-petta-kernel README](https://github.com/trueagi-io/jupyter-petta-kernel/blob/main/README.md) for detailed installation instructions and usage.

### MeTTa server

A HTTP server running MeTTa code is also available:

**Repository:** [MettaWamJam](https://github.com/trueagi-io/MettaWamJam)

Please see the [MettaWamJam README](https://github.com/trueagi-io/MettaWamJam/blob/main/README.md) for detailed installation instructions and usage.

### MeTTa in WASM

Since Swi-Prolog can be compiled to Web Assembly, one can embed PeTTa into websites.

Please see [Execution-in-browser](https://github.com/patham9/PeTTa/wiki/Execution-in-browser) for more information.
