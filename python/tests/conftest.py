import importlib
from pathlib import Path
import pytest

@pytest.fixture(scope="session")
def repo_root():
    return Path(__file__).resolve().parents[2]

@pytest.fixture(scope="session")
def petta_module():
    return importlib.import_module("python.petta")

@pytest.fixture(scope="session")
def petta_path(repo_root):
    return str(repo_root)

@pytest.fixture(scope="session")
def petta_instance(petta_module, petta_path):
    return petta_module.PeTTa(verbose=False, petta_path=petta_path)

@pytest.fixture(scope="session")
def petta_verbose(petta_module, petta_path):
    return petta_module.PeTTa(verbose=True, petta_path=petta_path)

@pytest.fixture(scope="session")
def dummy_metta_path(repo_root):
    return repo_root / "python" / "tests" / "data" / "dummy.metta"
