import uuid

import pytest


def test_direct_entry_file_cycle_loads_once(petta_instance, tmp_path):
    suffix = uuid.uuid4().hex
    cycle_a = tmp_path / "cycle_a.metta"
    cycle_b = tmp_path / "cycle_b.metta"

    cycle_a.write_text(
        f"!(import! &self cycle_b)\n(= (cycle-a-{suffix}) cycle-a-result)\n"
        f"!(cycle-a-{suffix})\n"
    )
    cycle_b.write_text(
        f"!(import! &self cycle_a)\n(= (cycle-b-{suffix}) cycle-b-result)\n"
    )

    results = petta_instance.load_metta_file(str(cycle_a))

    assert results.count("cycle-a-result") == 1


def test_source_functions_compile_once_across_spaces(petta_instance, tmp_path):
    suffix = uuid.uuid4().hex
    dependency_file = tmp_path / "dependency.metta"
    root_file = tmp_path / "root.metta"

    dependency_file.write_text(f"(= (shared-{suffix}) shared-result)\n")
    root_file.write_text(
        "!(bind! &space-a (new-space))\n"
        "!(bind! &space-b (new-space))\n"
        "!(import! &space-a dependency)\n"
        "!(import! &space-b dependency)\n"
        f"!(shared-{suffix})\n"
    )

    results = petta_instance.load_metta_file(str(root_file))

    assert results.count("shared-result") == 1


def test_nested_import_does_not_fall_back_to_cwd(
    petta_instance, tmp_path, monkeypatch
):
    importer_dir = tmp_path / "importer"
    cwd_dir = tmp_path / "cwd"
    importer_dir.mkdir()
    cwd_dir.mkdir()
    root_file = importer_dir / "root.metta"
    root_file.write_text("!(import! &self dependency)\n")
    (cwd_dir / "dependency.metta").write_text("(cwd-only-dependency)\n")
    monkeypatch.chdir(cwd_dir)

    with pytest.raises(Exception, match="source_sink"):
        petta_instance.load_metta_file(str(root_file))


def test_import_after_use_warns(petta_instance, tmp_path, capfd):
    fn = f"late_inc_{uuid.uuid4().hex}"
    root_file = tmp_path / "root.metta"
    dependency_file = tmp_path / "dependency.metta"

    dependency_file.write_text(f"(= ({fn} $x) (+ $x 1))\n")
    root_file.write_text(
        f"(= (uses-{fn} $x) ({fn} $x))\n"
        "!(import! &self dependency)\n"
        f"!(uses-{fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))
    stderr = capfd.readouterr().err

    # The definition compiled before the import treats the name as a plain symbol...
    assert f"({fn} 41)" in results
    # ...and the late registration is called out.
    assert fn in stderr
    assert "Move the import or definition above the first use" in stderr
