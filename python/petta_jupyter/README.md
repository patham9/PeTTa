# PeTTa Jupyter Kernel

A Jupyter kernel for executing MeTTa code using PeTTa.

## Features

- Execute MeTTa code in Jupyter notebooks
- Clean output formatting
- Graceful error handling with readable error messages
- State persistence across notebook cells
- Support for syntax errors, type errors, and runtime errors

## Requirements

- Python 3.8+
- SWI-Prolog
- janus-swi
- ipykernel
- jupyterlab or jupyter notebook

## Installation

### Quick Install

From the PeTTa repository:

```bash
cd /path/to/PeTTa/python/petta_jupyter
pip install -e .
```

This will install the package and register the kernel with Jupyter.

### Manual Installation

If the automatic kernel registration fails, you can manually install it:

```bash
# Install dependencies
pip install ipykernel janus-swi

# Register the kernel
jupyter kernelspec install --user /path/to/PeTTa/python/petta_jupyter/resources
```

### Verify Installation

Check that the kernel is installed:

```bash
jupyter kernelspec list
```

You should see `petta` in the list.

## Usage

1. Launch JupyterLab or Jupyter Notebook:
   ```bash
   jupyter lab
   # or
   jupyter notebook
   ```

2. Create a new notebook and select "PeTTa (MeTTa)" as the kernel

3. Write MeTTa code in cells and execute them:
   ```metta
   !(+ 1 2)
   ```

   Output: `3`

4. Define functions:
   ```metta
   (= (factorial 0) 1)
   (= (factorial $n) (* $n (factorial (- $n 1))))
   ```

5. Use defined functions:
   ```metta
   !(factorial 5)
   ```

   Output: `120`

## Examples

### Basic arithmetic
```metta
!(+ 1 2)         # Returns: 3
!(* 3 4)         # Returns: 12
!(- 10 5)        # Returns: 5
```

### Function definition
```metta
(= (double $x) (* $x 2))
!(double 5)      # Returns: 10
```

### Error handling
```metta
!(+ 1           # Error: Parse error: missing ')'
!(+ abc def)    # Error: Type error: Expected evaluable, got def/0
```

## Implementation Details

### Architecture

- **kernel.py**: Main kernel implementation, handles code execution
- **output_formatter.py**: Formats results and error messages
- **resources/kernel.json**: Kernel specification for Jupyter

### Technical Decisions

- Uses temporary files to pass code to PeTTa (avoids Prolog string escaping issues)
- Integrates with PeTTa's Python API via janus_swi
- Extracts clean error messages from Prolog error terms
- Displays errors in red (stderr) and normal output in black (stdout)

## Troubleshooting

### Kernel crashes on startup

Make sure janus-swi is installed:
```bash
pip install janus-swi
```

### Parse errors crash the kernel

This should be fixed in the current version. Make sure you're using the latest PeTTa code with error handling improvements.

### Kernel not found in Jupyter

Re-register the kernel:
```bash
jupyter kernelspec install --user /path/to/PeTTa/python/petta_jupyter/resources --replace
```

## Development

To modify the kernel:

1. Edit the source files in `python/petta_jupyter/`
2. Reinstall: `pip install -e .`
3. Restart the kernel in your notebook

## License

[Add appropriate license information]
