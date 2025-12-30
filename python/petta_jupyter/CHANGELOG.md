# Changelog

All notable changes to the PeTTa Jupyter kernel will be documented in this file.

## [0.1.0] - 2024-12-30

### Added
- Initial release of PeTTa Jupyter kernel
- Execute MeTTa code in Jupyter notebooks
- Clean output formatting for single and multiple results
- Graceful error handling with readable error messages
  - Syntax error formatting
  - Type error formatting
  - Runtime error handling
- State persistence across notebook cells
- Automated installation script (install.sh)
- Python package setup (setup.py)
- Comprehensive documentation (README.md)
- Example notebook demonstrating basic usage

### Changed
- Modified PeTTa core error handling:
  - src/filereader.pl: Throw exceptions instead of halt(1) on parse errors
  - src/parser.pl: Throw exceptions on malformed S-expressions
  - python/helper.pl: Catch and return error messages

### Technical Details
- Uses temporary file approach to avoid Prolog string escaping issues
- Integrates with PeTTa's Python API via janus_swi
- Extracts clean error messages from Prolog error terms
- Displays errors to stderr (red) and results to stdout (black)

## Future Enhancements

Potential features for future versions:
- Code completion support
- Syntax highlighting improvements
- Introspection capabilities (? operator support)
- Magic commands for debugging
- Performance optimizations
- Additional error types handling
