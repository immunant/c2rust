from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import pytest

import postprocess
from postprocess.transforms.base import TransformError, TransformResult
from postprocess.validate import BaselineError


@pytest.mark.parametrize("value", ["0", "-1", "many"])
def test_jobs_rejects_invalid_values(tmp_path: Path, value: str) -> None:
    source = tmp_path / "lib.rs"
    source.touch()
    with pytest.raises(SystemExit) as error:
        postprocess.build_arg_parser().parse_args([str(source), "-j", value])
    assert error.value.code == 2


@pytest.mark.parametrize("option", ["-j", "--jobs"])
def test_jobs_option_and_default(tmp_path: Path, option: str) -> None:
    source = tmp_path / "lib.rs"
    source.touch()
    parser = postprocess.build_arg_parser()
    assert parser.parse_args([str(source)]).jobs == 4
    assert parser.parse_args([str(source), option, "8"]).jobs == 8


@pytest.mark.parametrize("failure", [None, "baseline", "transform"])
def test_main_forwards_jobs_and_closes_model(monkeypatch, tmp_path, failure) -> None:
    source = tmp_path / "lib.rs"
    source.touch()
    close = AsyncMock()
    model = SimpleNamespace(aclose=close)
    monkeypatch.setattr(postprocess, "get_model", lambda _: model)

    apply = AsyncMock(return_value=TransformResult())
    if failure == "transform":
        apply.side_effect = TransformError("rejected")
    transform = SimpleNamespace(apply_dir=apply)
    monkeypatch.setattr(postprocess, "get_transform_by_id", lambda *a, **kw: transform)

    validator = Mock(return_value=None)
    if failure == "baseline":
        validator.side_effect = BaselineError("broken baseline")
    monkeypatch.setattr(postprocess, "make_validator", validator)

    result = postprocess.main(
        [str(source), "--cache-dir", str(tmp_path / "cache"), "-j", "8"]
    )

    assert result == (1 if failure else 0)
    if failure == "baseline":
        apply.assert_not_awaited()
    else:
        apply.assert_awaited_once()
        assert apply.await_args is not None
        assert apply.await_args.kwargs["jobs"] == 8
    close.assert_awaited_once_with()
