import json
from os.path import commonpath

from conftest import EXAMPLES_ROOT

from postprocess.definitions import (
    get_c_sourcefile,
    get_function_span_pairs,
    get_rust_function_spans,
)


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

    assert len(fn_spans) == 3

    assert fn_spans[0]["name"] == "swap"
    assert fn_spans[0]["start_line"] == 10
    assert fn_spans[0]["end_line"] == 17

    assert fn_spans[1]["name"] == "partition"
    assert fn_spans[1]["start_line"] == 19
    assert fn_spans[1]["end_line"] == 42

    assert fn_spans[2]["name"] == "quickSort"
    assert fn_spans[2]["start_line"] == 44
    assert fn_spans[2]["end_line"] == 54


# TODO: this test needs a clearer scope
def test_c_function_splitting(generate_compile_commands_for_qsort, transpile_qsort):
    from postprocess.utils import read_chunk

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
