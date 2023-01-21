#!/usr/bin/env python3

from argparse import ArgumentParser
from dataclasses import dataclass, field
import dataclasses
from datetime import datetime
from functools import cache
import json
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Type, TypeVar, Union
from urllib.parse import urlparse
import dateutil.parser
import plumbum as pb


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

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, User):
            return NotImplemented
        return self.login == other.login

    def __hash__(self) -> int:
        return hash(self.login)


@dataclass
class Issue:
    number: int
    author: User
    title: str
    url: str
    createdAt: datetime
    updatedAt: datetime
    closedAt: Optional[datetime]

    @classmethod
    def name(cls) -> str:
        return "issue"

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

    @classmethod
    def name(cls) -> str:
        return "PR"

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


def detect_repo() -> str:
    git = pb.local["git"]
    remote_url = urlparse(git["config", "--get", "remote.origin.url"]())
    assert remote_url.netloc == "github.com"
    remote_path = Path(remote_url.path)
    return str(remote_path.with_suffix("")).lstrip("/")


def localize_tz(dt: Optional[datetime]) -> Optional[datetime]:
    if dt is None:
        return None
    return dt.astimezone(tz=dt.tzinfo)


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


@dataclass
class Args:
    repo: Optional[str] = None
    after: Optional[datetime] = None
    before: Optional[datetime] = None
    list: bool = False
    datetime_format: str = "%c"
    cache: bool = False


def main() -> None:
    parser = ArgumentParser(description="summarize repo activity (PR/issues) during a time period (requires gh)")
    parser.add_argument("--repo", type=str, help="the GitHub repo (defaults to the current repo)")
    parser.add_argument("--after", type=dateutil.parser.parse, help="summarize after this date")
    parser.add_argument("--before", type=dateutil.parser.parse, help="summarize before this date")
    parser.add_argument("--list", default=False, action='store_true', help="list each PR/issue")
    parser.add_argument("--datetime-format", type=str, help="a strftime format string", default="%c")
    parser.add_argument("--cache", default=False, action='store_true', help="cache gh API results")

    args = Args(**parser.parse_args().__dict__)

    if args.repo is None:
        repo = detect_repo()
    else:
        repo = args.repo

    args.after = localize_tz(args.after)
    args.before = localize_tz(args.before)
    time_range = TimeRange(start=args.after, end=args.before)

    gh = pb.local["gh"]

    T = TypeVar("T", Issue, PR)

    @cache
    def list(T: Type[T]) -> List[T]:
        name = T.__name__.lower()
        path = Path(f".repo/{repo}/{name}.json")
        if args.cache and path.exists():
            response_str = path.read_text()
        else:
            response_str = gh[
                name,
                "list",
                "--repo", repo,
                "--state", "all",
                "--limit", int(1e6),
                "--json", ",".join(field.name for field in dataclasses.fields(T)),
            ]()
            if args.cache:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(response_str)
        response_json = json.loads(response_str)
        return [T(**item) for item in response_json]

    @cache
    def collaborators() -> List[User]:
        response_str = gh["api", "-H", "Accept: application/vnd.github+json", f"/repos/{repo}/collaborators", "--jq", "[.[] | {login}]"]()
        response_json = json.loads(response_str)
        return [User(**item) for item in response_json]

    def opened(t: T) -> datetime:
        return t.createdAt

    def updated(t: T) -> datetime:
        return t.updatedAt

    def closed(t: T) -> Optional[datetime]:
        return t.closedAt

    def merged(t: PR) -> Optional[datetime]:
        return t.mergedAt

    def by_collaborators(author: User) -> bool:
        return author in collaborators()

    def by_community(author: User) -> bool:
        return author not in collaborators()

    def summarize(T: Type[T], get_time: Callable[[T], Optional[datetime]]) -> None:
        time_name = get_time.__name__

        in_time_range = [t for t in list(T) if get_time(t) in time_range]
        print(f"{time_name} {len(in_time_range)} {T.name()}s")

        for by in (by_collaborators, by_community):
            by_them = [t for t in in_time_range if by(t.author)]
            by_name = by.__name__.replace("_", " ")
            print(f"\t{by_name}: {time_name} {len(by_them)} {T.name()}s")
            if args.list:
                for t in by_them:
                    time = get_time(t).strftime(args.datetime_format)
                    print(f"\t\t#{t.number} ({time_name} {time}) by @{t.author.login} ({t.author.name}): {t.title}")

    summarize(PR, opened)
    summarize(PR, merged)

    summarize(Issue, opened)
    summarize(Issue, closed)


if __name__ == "__main__":
    main()
