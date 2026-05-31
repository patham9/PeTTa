# Editor integration (Debug Adapter Protocol)

PeTTa ships a Debug Adapter Protocol (DAP) server so editors can drive the
debugger: set breakpoints, step, inspect the call stack and MeTTa variables, and
evaluate expressions while paused.

## The adapter

```bash
sh dap.sh
```

`dap.sh` launches `src/dap_server.pl`, which speaks DAP JSON over **stdin/stdout**
(`Content-Length`-framed messages). Program and debugger output go to **stderr**,
so the protocol channel stays clean.

The program to debug is taken from the `launch` request's `program` argument.

### Supported requests

| DAP request               | Mapped to                                             |
|---------------------------|-------------------------------------------------------|
| `initialize`              | capabilities + `initialized` event                    |
| `setBreakpoints`          | line breakpoints (`debug_break_line`)                 |
| `setFunctionBreakpoints`  | goal-head breakpoints (`debug_break_target`)          |
| `setExceptionBreakpoints` | break on `Error` results (`debug_break_error`)        |
| `launch` + `configurationDone` | load and run the `.metta` file with tracing      |
| `stackTrace`              | the MeTTa call stack                                  |
| `scopes` / `variables`    | the paused frame's MeTTa `$variable` bindings         |
| `evaluate`                | evaluate a MeTTa expression in the paused frame       |
| `continue` / `next` / `stepIn` / `stepOut` | resume / step                        |
| `disconnect` / `terminate`| stop the session                                      |

### Events

`initialized`, `stopped` (at a breakpoint), `terminated` and `exited` (when the
program finishes).

## VS Code

`vscode/launch.json` is a minimal launch configuration. VS Code needs a small
debug-adapter extension that declares a debug `type` of `petta` and starts
`dap.sh` as the adapter (an stdio adapter via `DebugAdapterExecutable`). Any
generic "DAP over stdio" bridge extension works too — point it at `dap.sh`.

## Trying it without an editor

The adapter is just stdio JSON, so you can script a session. See
`tests/debugger/dap.sh` for a complete worked example that initializes, sets a
function breakpoint, launches a file, hits a breakpoint, fetches the stack and
variables, evaluates an expression in the paused frame, and disconnects.
