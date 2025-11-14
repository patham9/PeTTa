from hyperon.ext import register_atoms
from hyperon import *
import sys, os
orig_cwd = os.getcwd()
sys.path.append(os.path.abspath(os.path.join(orig_cwd, '..')))
os.chdir(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from petta import PeTTa
peTTa = PeTTa()
os.chdir(orig_cwd)


class PatternOperation(OperationObject):
    def __init__(self, name, op, unwrap=False, rec=False):
        super().__init__(name, op, unwrap)
        self.rec = rec
    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        return super().execute(*args, res_typ=res_typ)

def wrapnpop(func):
    def wrapper(*args):
        a = [str("'"+arg) if arg is SymbolAtom else str(arg) for arg in args]
        res = func(*a)
        return [res]
    return wrapper

def call_petta(*a):
    tokenizer = globalmetta.tokenizer()
    EXPRESSION = str(*a)
    if EXPRESSION.startswith("\""): #unstring
        EXPRESSION = EXPRESSION[1:-1]
    if EXPRESSION.endswith(".metta"):
        peTTa.load_metta_file(file_path=orig_cwd + "/" + EXPRESSION)
        parser = SExprParser("True")
        return parser.parse(tokenizer)
    else:
        if not EXPRESSION.startswith("(="):
            EXPRESSION = "!" + EXPRESSION
        resultslist = peTTa.process_metta_string(EXPRESSION)
        if EXPRESSION.startswith("(="):
            parser = SExprParser("True")
            return parser.parse(tokenizer)
        allsolutions = "(superpose (" + (" ".join([str(x) for x in resultslist])) + "))"
        parser = SExprParser(allsolutions)
        return parser.parse(tokenizer)

globalmetta = None
@register_atoms(pass_metta=True)
def petta_atoms(metta):
    global globalmetta
    globalmetta = metta
    call_petta_atom = G(PatternOperation('petta', wrapnpop(call_petta), unwrap=False))
    return { r"petta": call_petta_atom }
