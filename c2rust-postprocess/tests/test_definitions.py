import json
from os.path import commonpath
from pathlib import Path

from conftest import EXAMPLES_ROOT

from postprocess import main
from postprocess.definitions import (
    get_c_sourcefile,
    get_function_span_pairs,
    get_rust_function_spans,
)
from postprocess.utils import read_chunk


def test_get_c_sourcefile_qsort(generate_compile_commands_for_qsort, transpile_qsort):
    print(generate_compile_commands_for_qsort)
    commands = generate_compile_commands_for_qsort
    qsort_rs = transpile_qsort

    qsort_c = get_c_sourcefile(commands, qsort_rs)
    assert qsort_c is not None and qsort_c.exists() and qsort_c.is_file()
    assert qsort_c == qsort_rs.with_suffix(".c")

    prefix = commonpath([qsort_c, qsort_rs])
    assert prefix == str(EXAMPLES_ROOT / "qsort")


def test_get_rust_function_spans(transpile_qsort, pytestconfig):
    qsort_rs = transpile_qsort

    fn_spans = get_rust_function_spans(qsort_rs)
    print(json.dumps(fn_spans, indent=4))

    expected_fn_spans = [
        {
            "name": "swap",
            "start_line": 10,
            "end_line": 14,
            "start_byte": 154,
            "end_byte": 316,
        },
        {
            "name": "partition",
            "start_line": 16,
            "end_line": 39,
            "start_byte": 330,
            "end_byte": 1193,
        },
        {
            "name": "quickSort",
            "start_line": 41,
            "end_line": 51,
            "start_byte": 1207,
            "end_byte": 1566,
        },
    ]

    for actual_fn_span, expected_fn_span in zip(
        fn_spans, expected_fn_spans, strict=True
    ):
        assert actual_fn_span == expected_fn_span


# TODO: this test needs a clearer scope
def test_c_function_splitting(generate_compile_commands_for_qsort, transpile_qsort):
    commands = generate_compile_commands_for_qsort
    qsort_rs = transpile_qsort
    qsort_c = get_c_sourcefile(commands, qsort_rs)

    assert qsort_c is not None

    rust_fn_spans = get_function_span_pairs(commands, qsort_rs)

    for rust_fn, c_fn in rust_fn_spans:
        assert rust_fn["name"] == c_fn["name"]

        c_def = read_chunk(qsort_c, c_fn["start_byte"], c_fn["end_byte"])
        print(f"C function {c_fn['name']} definition:\n{c_def}\n")

        rust_def = read_chunk(qsort_rs, rust_fn["start_byte"], rust_fn["end_byte"])
        print(f"Rust function {rust_fn['name']} definition:\n{rust_def}\n")

    # assert False


def test_comment_insertion_qsort():
    qsort_rs = Path(__file__).parent / "examples/qsort.rs"
    qsort_rs = qsort_rs.relative_to(Path.cwd())
    main([str(qsort_rs), "--no-update-rust"])
