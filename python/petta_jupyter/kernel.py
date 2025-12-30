"""PeTTa Jupyter Kernel - Main kernel implementation"""

import tempfile
import os
from ipykernel.kernelbase import Kernel
from petta import PeTTa
from .output_formatter import format_results


class PeTTaKernel(Kernel):
    """Jupyter kernel for executing MeTTa code via PeTTa"""

    implementation = 'PeTTa'
    implementation_version = '0.1.0'
    language = 'MeTTa'
    language_version = '0.1.0'

    language_info = {
        'name': 'MeTTa',
        'mimetype': 'text/x-metta',
        'file_extension': '.metta',
        'codemirror_mode': 'scheme',
        'pygments_lexer': 'scheme',
    }

    banner = "PeTTa Jupyter Kernel - MeTTa Language"

    def __init__(self, **kwargs):
        """Initialize the kernel and PeTTa instance"""
        super().__init__(**kwargs)
        # Initialize PeTTa with error handling
        try:
            self.petta = PeTTa(verbose=False)
            self.initialized = True
            self.init_error = None
        except Exception as e:
            self.initialized = False
            self.init_error = str(e)
            # Log error but don't crash - let first execution show the error
            import sys
            print(f"ERROR initializing PeTTa: {e}", file=sys.stderr)

    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False):
        """Execute MeTTa code and return results"""
        # Check if initialization succeeded
        if not self.initialized:
            if not silent:
                error_msg = f"PeTTa failed to initialize:\n{self.init_error}\n\nPlease check that:\n1. janus-swi is installed: pip install janus-swi\n2. SWI-Prolog is installed and working"
                stream_content = {'name': 'stderr', 'text': error_msg}
                self.send_response(self.iopub_socket, 'stream', stream_content)
            return {
                'status': 'error',
                'execution_count': self.execution_count,
                'ename': 'InitializationError',
                'evalue': self.init_error,
                'traceback': [self.init_error]
            }

        # Handle empty code
        if not code.strip():
            return {
                'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {}
            }

        # Execute MeTTa code via PeTTa
        # Use temp file approach to avoid string escaping issues
        with tempfile.NamedTemporaryFile(mode='w', suffix='.metta', delete=False) as f:
            f.write(code)
            temp_file = f.name

        try:
            results = self.petta.load_metta_file(temp_file)
        except Exception as e:
            # Clean up temp file on error
            os.unlink(temp_file)

            # Display error to user
            if not silent:
                error_msg = f"Error executing MeTTa code:\n{str(e)}"
                stream_content = {'name': 'stderr', 'text': error_msg}
                self.send_response(self.iopub_socket, 'stream', stream_content)

            # Return error status
            return {
                'status': 'error',
                'execution_count': self.execution_count,
                'ename': type(e).__name__,
                'evalue': str(e),
                'traceback': [str(e)]
            }
        finally:
            # Always clean up temp file if it still exists
            if os.path.exists(temp_file):
                os.unlink(temp_file)

        # Format and display results
        if not silent:
            output, is_error = format_results(results)
            if output is not None:
                stream_name = 'stderr' if is_error else 'stdout'
                stream_content = {'name': stream_name, 'text': output}
                self.send_response(self.iopub_socket, 'stream', stream_content)

        # Return success
        return {
            'status': 'ok',
            'execution_count': self.execution_count,
            'payload': [],
            'user_expressions': {}
        }
