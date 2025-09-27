import os
import janus_swi as janus

class PeTTa:
    def __init__(self, metta_src_path=None):
        if metta_src_path is None:
            # Assume the src directory is in the same directory as this file
            metta_src_path = os.path.join(os.path.dirname(__file__), '..', 'src' , 'main.pl')
        janus.consult(metta_src_path)

    def load_metta_file(self, file_path):
        """Compile a MeTTa file to Prolog."""
        return list(janus.query_once(f"load_metta_file('{file_path}')"))

    def process_metta_string(self, metta_code):
        """Compile a string of MeTTa code to Prolog."""
        # Note: This assumes process_metta_string is defined in the Prolog code
        # If not, we might need to add it or use load_metta_file with a temp file
        return list(janus.query_once(f"process_metta_string('{metta_code}')"))

    def run(self):
        """Execute the run(R) predicates and return results."""
        results = list(janus.query("run(R)"))
        return [result['R'] for result in results]
