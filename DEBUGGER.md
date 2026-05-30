# PeTTa Debugger Guide

This guide focuses on practical debugging workflows in PeTTa.

## Entry points

Show debugger help:

```bash
sh debug.sh
```

Run a file with the debugger:

```bash
sh debug.sh your_file.metta --debug=runtime
```

Run the debugger smoke test:

```bash
sh debug_test.sh
```

## Main debug modes

- `--debug=runtime`: MeTTa-first runtime trace.
- `--debug=runtime-prolog`: low-level runtime trace with compiled Prolog goals.
- `--debug=source,parse,translate,result`: source-to-result pipeline trace.
- `--debug=space`: atom-space mutations and matches.
- `--debug-output=<file>`: write a plain-text copy of debugger output to a file while still showing terminal output.
- `--silent`: suppress the legacy compile/run pretty-print and keep the debugger output.

## Common workflows

### Wrong result

Start with the function you suspect:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-goal=yourFunc --silent
```

This shows calls and returned values such as:

```text
ENTER  (yourFunc 7 3)
OK     (yourFunc 7 3) => 4
```

### Wrong operator

If a function may be calling the wrong built-in, trace the operator directly.

Example from the intentionally broken [examples/add_buggy.metta](./examples/add_buggy.metta):

```bash
sh debug.sh examples/add_buggy.metta --debug=runtime --debug-goal=- --silent
```

This makes the suspicious call explicit:

```text
ENTER  (- 7 3)
OK     (- 7 3) => 4
```

### Wrong recursion

Use a conditional breakpoint to stop on impossible states.

Example from [examples/fib_buggy.metta](./examples/fib_buggy.metta):

```bash
sh debug.sh examples/fib_buggy.metta --debug-break-if='fib:arg1<0' --debug-break-once --debug-goal=fib --silent
```

This stops the first time recursion reaches a negative input.

### Wrong return value

Break on a suspicious return value:

```bash
sh debug.sh examples/fib_buggy.metta --debug-break-if='fib:result=0' --debug-break-once --debug-goal=fib --silent
```

Result conditions only trigger on successful returns.

### Space and match problems

Trace atom-space operations:

```bash
sh debug.sh examples/spaces.metta --debug=space --silent
```

This shows:

- atoms added to spaces
- successful matches
- failed matches
- `get-atoms` activity

## Breakpoints

Break on function head:

```bash
sh debug.sh your_file.metta --debug-break=yourFunc --debug-goal=yourFunc --silent
```

Break on a condition:

```bash
sh debug.sh your_file.metta --debug-break-if='yourFunc:arg1<0' --debug-goal=yourFunc --silent
```

Supported fields:

- `argN`
- `result`

Supported operators:

- `=`
- `!=`
- `>`
- `<`
- `>=`
- `<=`

Supported condition combinators:

- `&` for `and`
- `|` for `or`

Examples:

```bash
sh debug.sh examples/fib_buggy.metta --debug-break-if='fib:arg1=2&result=0' --debug-break-once --debug-goal=fib --silent
sh debug.sh examples/fib_buggy.metta --debug-break-if='fib:arg1<0|result=0' --debug-break-skip=1 --debug-break-once --debug-goal=fib --silent
```

## Noise control

Limit trace depth:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-depth=3 --silent
```

Limit event count:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-max-events=40 --silent
```

Skip early breakpoint hits:

```bash
sh debug.sh your_file.metta --debug-break-if='yourFunc:arg1<0|result=0' --debug-break-skip=2 --silent
```

Save a trace to a file:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-output=trace.log --silent
```

## Reading breakpoint output

A breakpoint hit now shows:

- the goal that triggered
- the hit number
- the reason it matched
- the matched values
- the current MeTTa stack
- source form index and line number through `f` or `p`
- goal head and current stack depth through `f` or `p`
- current arguments, and result on success events, through `f` or `p`
- the original MeTTa source form through `l`

Example:

```text
[BREAKPOINT ...] (fib 2) => 0 @ #compiled
hit: 3
reason: matched condition fib:arg1=2&result=0
match: arg1 = 2
match: result = 0
stack: (fib 10) → (fib 9) → (fib 8) → (fib 7) → (fib 6) → (fib 5) → (fib 4) → (fib 3) → (fib 2)
```

## Interactive breakpoint prompt

When running in a TTY, breakpoint hits accept:

- `Enter`: continue
- `l`: print the current MeTTa source form
- `s`: print stack again
- `f`: print the current frame, source location, depth, and arguments
- `p`: print the current goal, source location, depth, arguments, and result if available
- `i`: step into the next visible runtime event
- `n`: step over until execution returns to the same frame depth or shallower
- `o`: step out of the current frame
- `c`: continue without any more breakpoint pauses
- `q`: abort

## Suggested first commands

Wrong result in one function:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-goal=yourFunc --silent
```

Suspicious recursion:

```bash
sh debug.sh your_file.metta --debug-break-if='yourFunc:arg1<0' --debug-break-once --debug-goal=yourFunc --silent
```

Suspicious operator:

```bash
sh debug.sh your_file.metta --debug=runtime --debug-goal=- --silent
```

Space behavior:

```bash
sh debug.sh your_file.metta --debug=space --silent
```
