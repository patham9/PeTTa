import builtins
import sys
import types
import uuid

import pytest


def write_increment_dependency(tmp_path):
    """Write dependency.metta defining a fresh increment function; return its name and the root path."""
    fn = f"late_inc_{uuid.uuid4().hex}"
    dependency_file = tmp_path / "dependency.metta"
    dependency_file.write_text(f"(= ({fn} $x) (+ $x 1))\n")
    return fn, tmp_path / "root.metta"


def test_failed_import_can_be_retried(petta_instance, tmp_path):
    module_name = f"petta_retry_{uuid.uuid4().hex}"
    root_file = tmp_path / "root.metta"
    dependency_file = tmp_path / "dependency.metta"
    python_file = tmp_path / f"{module_name}.py"

    root_file.write_text("!(import! &self dependency)\n!(retry-result)\n")
    dependency_file.write_text(
        f'!(import! &self "{module_name}.py")\n(= (retry-result) retry-ok)\n'
    )
    python_file.write_text('raise RuntimeError("first import fails")\n')
    previous_path = list(sys.path)

    with pytest.raises(Exception, match="first import fails"):
        petta_instance.load_metta_file(str(root_file))

    assert sys.path == previous_path
    assert module_name not in sys.modules

    python_file.write_text("RETRY_SUCCEEDED = True\n")
    results = petta_instance.load_metta_file(str(root_file))

    assert "retry-ok" in results


def test_entry_file_breaks_direct_import_cycle(
    petta_instance, petta_module, tmp_path
):
    function_name = f"entry_cycle_{uuid.uuid4().hex}"
    entry_file = tmp_path / "a.metta"
    sibling_file = tmp_path / "b.metta"
    entry_file.write_text(
        "!(import! &self b)\n"
        f"(= ({function_name}) a)\n"
    )
    sibling_file.write_text("!(import! &self a)\n")

    petta_instance.load_metta_file(str(entry_file))
    result = petta_module.janus.query_once(
        f"aggregate_all(count, clause('{function_name}'(_), _), Count)"
    )

    assert result["Count"] == 1


def test_definition_before_import_resolves(petta_instance, tmp_path, capfd):
    fn, root_file = write_increment_dependency(tmp_path)
    root_file.write_text(
        f"(= (uses-{fn} $x) ({fn} $x))\n"
        "!(import! &self dependency)\n"
        f"!(uses-{fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))
    stderr = capfd.readouterr().err

    # The definition compiled before the import is recompiled when the import
    # registers the function, so the cross-file call reduces regardless of order.
    assert "42" in results
    assert "Move the import or definition above the first use" not in stderr


def test_definition_before_dynamic_import_resolves(petta_instance, tmp_path):
    # The import target is computed at runtime, so no scan could know it upfront;
    # the definition still heals when the loaded file registers the function.
    fn, root_file = write_increment_dependency(tmp_path)
    root_file.write_text(
        f"(= (uses-{fn} $x) ({fn} $x))\n"
        "(= (dynamic-import-path) dependency)\n"
        "!(import! &self (dynamic-import-path))\n"
        f"!(uses-{fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))

    assert "42" in results


def test_execution_before_import_warns(petta_instance, tmp_path, capfd):
    fn, root_file = write_increment_dependency(tmp_path)
    root_file.write_text(
        f"!({fn} 41)\n"
        "!(import! &self dependency)\n"
        f"!({fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))
    stderr = capfd.readouterr().err

    # The call that ran before the import treated the name as a plain symbol...
    assert f"({fn} 41)" in results
    # ...the same call after the import reduces...
    assert "42" in results
    # ...and the unrepairable early execution is called out.
    assert fn in stderr
    assert "Move the import or definition above the first use" in stderr


def test_python_import_uses_canonical_path(petta_instance, tmp_path):
    module_name = f"same_name_{uuid.uuid4().hex}"
    event_name = f"PETTA_IMPORT_EVENTS_{uuid.uuid4().hex}"
    left = tmp_path / "left"
    right = tmp_path / "right"
    left.mkdir()
    right.mkdir()
    setattr(builtins, event_name, [])
    previous_module = types.ModuleType(module_name)
    sys.modules[module_name] = previous_module

    try:
        for directory, value in ((left, "left"), (right, "right")):
            (directory / f"{module_name}.py").write_text(
                "import builtins\n"
                f"builtins.{event_name}.append({value!r})\n"
                f"def origin(): return {value!r}\n"
            )
            (directory / "root.metta").write_text(
                f'!(import! &self "{module_name}.py")\n'
                f"!(py-call ({module_name}.origin))\n"
            )

        left_results = petta_instance.load_metta_file(str(left / "root.metta"))
        assert "left" in left_results
        assert sys.modules[module_name] is previous_module

        right_results = petta_instance.load_metta_file(str(right / "root.metta"))
        assert "right" in right_results

        assert getattr(builtins, event_name) == ["left", "right"]
        assert sys.modules[module_name] is previous_module
    finally:
        sys.modules.pop(module_name, None)
        delattr(builtins, event_name)


def test_python_import_can_load_sibling_module(petta_instance, tmp_path):
    module_name = f"python_sibling_{uuid.uuid4().hex}"
    helper_name = f"python_helper_{uuid.uuid4().hex}"
    module_file = tmp_path / f"{module_name}.py"
    helper_file = tmp_path / f"{helper_name}.py"
    root_file = tmp_path / "root.metta"
    helper_file.write_text('VALUE = "sibling-import-ok"\n')
    module_file.write_text(
        f"import {helper_name}\n"
        f"def sibling_value(): return {helper_name}.VALUE\n"
    )
    root_file.write_text(
        f'!(import! &self "{module_name}.py")\n'
        f"!(py-call ({module_name}.sibling_value))\n"
    )
    previous_path = list(sys.path)

    try:
        results = petta_instance.load_metta_file(str(root_file))

        assert "sibling-import-ok" in results
        assert sys.path == previous_path
        assert module_name not in sys.modules
    finally:
        sys.modules.pop(module_name, None)
        sys.modules.pop(helper_name, None)


def test_all_overloads_are_registered_before_repair(petta_instance, tmp_path):
    function_name = f"overloaded_{uuid.uuid4().hex}"
    caller_name = f"caller_{uuid.uuid4().hex}"
    (tmp_path / "dependency.metta").write_text(
        f"(= ({function_name} $x) one)\n"
        f"(= ({function_name} $x $y $z) three)\n"
    )
    root = tmp_path / "root.metta"
    root.write_text(
        f"(= ({caller_name}) ({function_name} 1 2))\n"
        "!(import! &self dependency)\n"
        f"!({caller_name} 3)\n"
    )

    results = petta_instance.load_metta_file(str(root))

    assert "three" in results


def test_missing_relative_import_does_not_fall_back_to_cwd(
    petta_instance, tmp_path, monkeypatch
):
    fallback_name = f"cwd_fallback_{uuid.uuid4().hex}"
    (tmp_path / "dependency.metta").write_text(
        f"(= ({fallback_name}) wrong-cwd)\n"
    )
    child = tmp_path / "sub"
    child.mkdir()
    root = child / "root.metta"
    root.write_text("!(import! &self dependency)\n")
    monkeypatch.chdir(tmp_path)

    with pytest.raises(Exception, match="source_sink"):
        petta_instance.load_metta_file(str(root))
