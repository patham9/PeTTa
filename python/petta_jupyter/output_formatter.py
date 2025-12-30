"""Output formatting for PeTTa Jupyter kernel"""

def format_results(results):
    """
    Format PeTTa execution results for display in Jupyter.

    Args:
        results: List of result strings from PeTTa execution

    Returns:
        Tuple of (output_string, is_error) where:
        - output_string: Formatted string for display, or None if no output needed
        - is_error: True if this is an error result, False otherwise
    """
    if not results:
        # Empty results (e.g., from function definitions)
        return None, False

    # Check if result is an error
    if len(results) == 1 and results[0].startswith('ERROR:'):
        # Extract the actual error message from the Prolog error term
        error_text = results[0]

        # Try to extract clean error message from various error types
        # Format: ERROR: error(error_type(message), context)
        if 'syntax_error(' in error_text:
            # Syntax errors: extract message
            start = error_text.find('syntax_error(') + len('syntax_error(')
            end = error_text.rfind('),')
            if end == -1:
                end = error_text.rfind(')')
            if start > 0 and end > start:
                error_msg = error_text[start:end]
                return error_msg, True
        elif 'type_error(' in error_text:
            # Type errors: format nicely
            # Example: error(type_error(evaluable,def/0),context(system:(is)/2,_18480))
            start = error_text.find('type_error(') + len('type_error(')
            end = error_text.find('),context(')
            if end == -1:
                end = error_text.find('))', start)
            if start > 0 and end > start:
                error_content = error_text[start:end]
                # Try to parse "evaluable,def/0" into "Expected evaluable, got def/0"
                parts = error_content.split(',', 1)
                if len(parts) == 2:
                    expected = parts[0].strip()
                    got = parts[1].strip()
                    return f"Type error: Expected {expected}, got {got}", True
                else:
                    return f"Type error: {error_content}", True

        # Fallback: return the whole error
        return error_text, True

    if len(results) == 1:
        # Single result - display it directly
        return results[0], False

    # Multiple results - display as a list
    return '\n'.join(results), False
