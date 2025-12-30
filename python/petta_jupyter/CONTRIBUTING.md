# Contributing to PeTTa Jupyter Kernel

Thank you for your interest in contributing to the PeTTa Jupyter kernel!

## Development Setup

1. Clone the PeTTa repository
2. Navigate to the kernel directory:
   ```bash
   cd /path/to/PeTTa/python/petta_jupyter
   ```

3. Install in development mode:
   ```bash
   pip install -e .
   python3 -m jupyter kernelspec install --user resources --name petta --replace
   ```

4. Make your changes

5. Test your changes:
   - Restart the kernel in your test notebook
   - Test with various MeTTa code examples
   - Verify error handling works correctly

## Code Structure

```
python/petta_jupyter/
├── __init__.py          # Package initialization
├── __main__.py          # Entry point for kernel launch
├── kernel.py            # Main kernel implementation
├── output_formatter.py  # Output and error formatting
├── resources/
│   └── kernel.json      # Jupyter kernel specification
├── examples/
│   └── basic_usage.ipynb # Example notebook
├── setup.py             # Package setup
├── install.sh           # Installation script
├── README.md            # User documentation
├── CHANGELOG.md         # Version history
└── CONTRIBUTING.md      # This file
```

## Key Components

### kernel.py
- `PeTTaKernel` class inherits from `ipykernel.kernelbase.Kernel`
- `do_execute()` method handles code execution
- Uses temporary files to pass code to PeTTa (avoids escaping issues)
- Handles exceptions and formats error messages

### output_formatter.py
- `format_results()` function processes PeTTa output
- Extracts clean error messages from Prolog error terms
- Returns tuple: (formatted_output, is_error)

## Making Changes

### Adding Error Type Support

To add support for a new error type:

1. Edit `output_formatter.py`
2. Add a new elif clause in the error detection logic
3. Extract the relevant error information
4. Return a formatted error message

Example:
```python
elif 'your_error_type(' in error_text:
    # Extract and format the error
    return formatted_message, True
```

### Improving Output Formatting

To change how results are displayed:

1. Edit `output_formatter.py`
2. Modify the `format_results()` function
3. Test with various types of MeTTa expressions

### Modifying Kernel Behavior

To change how the kernel executes code:

1. Edit `kernel.py`
2. Modify the `do_execute()` method
3. Test thoroughly with notebooks

## Testing

### Manual Testing Checklist

Test the kernel with:
- [ ] Basic arithmetic: `!(+ 1 2)`
- [ ] Function definitions: `(= (double $x) (* $x 2))`
- [ ] Function usage: `!(double 5)`
- [ ] Syntax errors: `!(+ 1`
- [ ] Type errors: `!(+ abc def)`
- [ ] State persistence across cells
- [ ] Empty cells
- [ ] Multiple statements in one cell

### Testing New Features

1. Create a test notebook
2. Test your changes with various inputs
3. Verify error handling works correctly
4. Check that existing functionality still works

## Pull Request Process

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/your-feature`
3. Make your changes
4. Test thoroughly
5. Commit with clear messages (no Claude references in commits)
6. Push to your fork
7. Create a pull request with:
   - Clear description of changes
   - Explanation of why the changes are needed
   - Test results

## Code Style

- Follow PEP 8 for Python code
- Use descriptive variable names
- Add docstrings to functions
- Comment complex logic
- Keep functions focused and small

## Debugging Tips

### Kernel Not Starting

Check:
- Is janus-swi installed? `pip list | grep janus`
- Is SWI-Prolog installed? `which swipl`
- Check kernel logs: Look in Jupyter's terminal output

### Kernel Crashes

- Add print statements to `kernel.py`
- Check if PeTTa is throwing uncaught exceptions
- Verify temporary files are being cleaned up

### Output Not Displaying

- Check the `format_results()` function
- Verify `send_response()` is being called
- Check if results are None or empty

## Questions?

If you have questions:
1. Check the README.md
2. Review existing code and comments
3. Open an issue on GitHub

Thank you for contributing!
