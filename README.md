## PeTTa

Efficient MeTTa language implementation in Prolog.

Please check out the [Wiki](https://github.com/patham9/PeTTa/wiki) for more information.

### Dependencies

- SWI-Prolog >= 9.3.x
- Python 3.x (for janus Python interop)

### Usage

Example run:

`time sh run.sh ./examples/nars_tuffy.metta`

### Debugging

PeTTa includes a built-in debugger for source loading, translation, runtime goal tracing, and atom-space operations.

See [DEBUGGER.md](./DEBUGGER.md) for a short practical guide.

Quick start:

```bash
sh debug.sh examples/fib.metta --debug=runtime
```

Show debugger help:

```bash
sh debug.sh
# or
sh run.sh --debug-help
```

Common examples:

```bash
sh debug.sh examples/fib.metta --debug=runtime
sh debug.sh examples/fib.metta --debug=runtime-leaf --debug-goal=fib
sh debug.sh examples/fib.metta --debug=runtime-prolog --debug-goal=fib --silent
sh debug.sh examples/fib.metta --debug=runtime --debug-depth=3 --debug-max-events=20 --silent
sh debug.sh examples/fib.metta --debug-break=fib --debug-goal=fib --silent
sh debug.sh examples/fib_buggy.metta --debug-break-if=fib:arg1<0 --debug-goal=fib --silent
sh debug.sh examples/fib_buggy.metta --debug-break-if=fib:result=0 --debug-break-once --debug-goal=fib --silent
sh debug.sh examples/fib_buggy.metta --debug-break-if='fib:arg1=2&result=0' --debug-break-once --debug-goal=fib --silent
sh debug.sh examples/fib_buggy.metta --debug-break-if='fib:arg1<0|result=0' --debug-break-skip=1 --debug-break-once --debug-goal=fib --silent
sh debug.sh examples/add_buggy.metta --debug=runtime --debug-goal=- --silent
sh debug.sh examples/fib.metta --debug=runtime --debug-output=trace.log --silent
sh debug.sh examples/fib.metta --debug=source,parse,translate,result --silent
sh debug.sh examples/spaces.metta --debug=space-match-fail
```

Available debug categories:

- `source`
- `parse`
- `compile`
- `translate`
- `runtime`
- `runtime-leaf`
- `runtime-fail`
- `runtime-prolog`
- `space`
- `space-mutation`
- `space-match-fail`
- `space-get-atoms`
- `result`
- `all`

Notes:

- `--debug` and `--debug-all` enable all categories.
- `--debug-goal=<name>` filters runtime events by goal head, for example `fib` or `+`.
- `--debug-break=<name>` emits breakpoint hits when entering matching goals.
- `--debug-break-if=<spec>` emits breakpoint hits when a condition matches, for example `fib:arg1<0` or `fib:arg1=2&result=0`.
- `--debug-break-once` stops future breakpoint hits after the first match.
- `--debug-break-skip=<n>` skips the first `n` matching breakpoint hits.
- `--debug-output=<file>` writes a plain-text copy of debugger output to a file.
- `--debug-depth=<n>` limits runtime trace output to call-stack depth `n`.
- `--debug-max-events=<n>` truncates debug output after `n` emitted events.
- Conditional breakpoint fields currently support `argN` and `result`, with `&` for `and` and `|` for `or`.
- `result` conditions only trigger on successful returns, so they behave like return-value breakpoints instead of entry checks.
- `runtime` now shows a MeTTa-first trace by default.
- `runtime-prolog` adds the underlying Prolog goal text for low-level debugging.
- Runtime traces include a MeTTa-style call stack for user-visible goals.
- `--silent` keeps the debugger output while suppressing the older colorful compile/run dump.

### MORK and FAISS spaces

If MORK and FAISS is installed, execute `sh build.sh` to support MORK-based atom spaces and FAISS-based atom-vector spaces.

The following projects are cloned and built by build.sh:

**Repository:** [mork_ffi](https://github.com/patham9/mork_ffi) dependent on [trueagi-io/mork](https://github.com/trueagi-io/mork)

**Repository:** [faiss_ffi](https://github.com/patham9/faiss_ffi) dependent on [facebookresearch/faiss](https://github.com/facebookresearch/faiss)

### Extension libraries

Please check out [Extension libraries](https://github.com/trueagi-io/PeTTa/wiki/Extension-libraries) for a set of extension libraries that can be invoked from MeTTa files directly from the git repository.

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

**Repository:** [MettaWamJam](https://github.com/jazzbox35/MettaWamJam)

Please see the [MettaWamJam README](https://github.com/jazzbox35/MettaWamJam/blob/main/README.md) for detailed installation instructions and usage.

### MeTTa in WASM

Since Swi-Prolog can be compiled to Web Assembly, one can embed PeTTa into websites.

Please see [Execution-in-browser](https://github.com/patham9/PeTTa/wiki/Execution-in-browser) for more information.
