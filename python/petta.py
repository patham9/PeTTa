import os
import threading
import janus_swi as janus

CONSULTED = False
CONSULT_LOCK = threading.Lock()

class PeTTa:
    def __init__(self, verbose=False, metta_src_path=None):
        global CONSULTED
        self.cmd = "process_metta_string" if verbose else "process_metta_string_silent"

        if not CONSULTED:
            with CONSULT_LOCK:
                if not CONSULTED:
                    if metta_src_path is None:
                        # Assume the src directory is in the same directory as this file
                        metta_src_path = os.path.join(os.path.dirname(__file__))
                    main_path = os.path.join(metta_src_path, '..', 'src' , 'main.pl')
                    helper_path = os.path.join(metta_src_path, 'helper.pl')
                    janus.consult(main_path)
                    janus.consult(helper_path)
                    CONSULTED = True

    def load_metta_file(self, file_path):
        """Compile a MeTTa file to Prolog and return the results of the run."""
        with open(file_path, 'r') as f:
            metta_code = f.read()
        return self.process_metta_string(metta_code)

    def process_metta_string(self, metta_code):
        """Compile a string of MeTTa code to Prolog and return the results of the run."""
        query = f"{self.cmd}('{metta_code}', Results)"
        result = janus.query_once(query)
        if result is None:
            return []
        return result.get('Results', [])
