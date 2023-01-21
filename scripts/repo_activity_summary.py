#!/usr/bin/env python3

from argparse import ArgumentParser
from dataclasses import dataclass
from datetime import datetime
from typing import Optional
import dateutil.parser

@dataclass
class Args:
    repo: Optional[str] = None
    after: Optional[datetime] = None
    before: Optional[datetime] = None
    list: bool = False

def main() -> None:
    parser = ArgumentParser(description="summarize repo activity (PR/issues) during a time period (requires gh)")
    parser.add_argument("--repo", type=str, help="the GitHub repo (defaults to the current repo)")
    parser.add_argument("--after", type=dateutil.parser.parse, help="summarize after this date")
    parser.add_argument("--before", type=dateutil.parser.parse, help="summarize before this date")
    parser.add_argument("--list", type=bool, help="list each PR/issue")
    args = Args(**parser.parse_args().__dict__)
    print(args)

if __name__ == "__main__":
    main()
