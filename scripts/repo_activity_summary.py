#!/usr/bin/env python3

from argparse import ArgumentParser
from dataclasses import dataclass, field
import dataclasses
from datetime import datetime
import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Type, TypeVar, Union
from urllib.parse import urlparse
import dateutil.parser
import plumbum as pb


@dataclass
class Args:
    repo: Optional[str] = None
    after: Optional[datetime] = None
    before: Optional[datetime] = None
    list: bool = False


def detect_repo() -> str:
    git = pb.local["git"]
    remote_url = urlparse(git["config", "--get", "remote.origin.url"]())
    assert remote_url.netloc == "github.com"
    remote_path = Path(remote_url.path)
    return str(remote_path.with_suffix("")).lstrip("/")


Json = Dict[str, Any]


@dataclass
class User:
    login: str
    name: Optional[str]
    is_bot: bool

    def __init__(
        self,
        login: str,
        name: Optional[str] = None,
        is_bot: bool = False,
        **_kwargs: Json,
    ) -> None:
        self.login = login
        self.name = name
        self.is_bot = is_bot

@dataclass
class Issue:
    number: int
    author: User
    title: str
    url: str
    createdAt: datetime
    updatedAt: datetime
    closedAt: Optional[datetime]

    def __init__(
            self,
            number: int,
            author: Union[User, Json],
            title: str,
            url: str,
            createdAt: Union[datetime, str],
            updatedAt: Union[datetime, str],
            closedAt: Optional[Union[datetime, str]],
    ) -> None:
        self.number = number
        if not isinstance(author, User):
            author = User(**author)
        self.author = author
        self.title = title
        self.url = url
        if not isinstance(createdAt, datetime):
            self.createdAt = dateutil.parser.parse(createdAt)
        else:
            self.createdAt = createdAt
        if not isinstance(updatedAt, datetime):
            self.updatedAt = dateutil.parser.parse(updatedAt)
        else:
            self.updatedAt = updatedAt
        if closedAt is not None and not isinstance(closedAt, datetime):
            self.closedAt = dateutil.parser.parse(closedAt)
        else:
            self.closedAt = closedAt


@dataclass
class PR(Issue):
    mergedAt: Optional[datetime]

    def __init__(
            self,
            number: int,
            author: Union[User, Json],
            title: str,
            url: str,
            createdAt: Union[datetime, str],
            updatedAt: Union[datetime, str],
            closedAt: Optional[Union[datetime, str]], mergedAt: Optional[Union[datetime, str]],
            ) -> None:
        super().__init__(
            number=number,
            author=author,
            title=title,
            url=url,
            createdAt=createdAt,
            updatedAt=updatedAt,
            closedAt=closedAt,
        )
        if mergedAt is not None and not isinstance(mergedAt, datetime):
            self.mergedAt = dateutil.parser.parse(mergedAt)
        else:
            self.mergedAt = mergedAt

@dataclass
class TimeRange:
    start: Optional[datetime] = None
    end: Optional[datetime] = None
    
    def __contains__(self, time: Optional[datetime]) -> bool:
        if time is None:
            return False
        if self.start is not None and time <= self.start:
            return False
        if self.end is not None and time >= self.end:
            return False
        return True

def main() -> None:
    parser = ArgumentParser(description="summarize repo activity (PR/issues) during a time period (requires gh)")
    parser.add_argument("--repo", type=str, help="the GitHub repo (defaults to the current repo)")
    parser.add_argument("--after", type=dateutil.parser.parse, help="summarize after this date")
    parser.add_argument("--before", type=dateutil.parser.parse, help="summarize before this date")
    parser.add_argument("--list", type=bool, help="list each PR/issue")
    args = Args(**parser.parse_args().__dict__)
    print(args)

    if args.repo is None:
        repo = detect_repo()
    else:
        repo = args.repo

    time_range = TimeRange(start=args.after, end=args.before)

    gh = pb.local["gh"]

    T = TypeVar("T", Issue, PR)

    def list(T: Type[T]) -> List[T]:
        response_str = gh[
            T.__name__.lower(),
            "list",
            "--repo", repo,
            "--state", "all",
            "--limit", int(1e6),
            "--json", ",".join(field.name for field in dataclasses.fields(T)),
        ]()
        response_json = json.loads(response_str)
        return [T(**item) for item in response_json]

    def list_collaborators() -> List[User]:
        response_str = gh["api", "-H", "Accept: application/vnd.github+json", f"/repos/{repo}/collaborators", "--jq", "[.[] | {login}]"]()
        response_json = json.loads(response_str)
        return [User(**item) for item in response_json]

    print(list_collaborators())
    print(list(PR))
    print(list(Issue))


if __name__ == "__main__":
    main()
