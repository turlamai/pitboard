#!/usr/bin/env python3
"""Fail closed on unrendered template tokens in exported Pitboard text.

Real article titles may contain standard HTML named character references,
including &uuml;. Only known HTML5 entities are allowed; unknown placeholders
like &term; and &rows; still fail the exported-artifact scan.
"""
from __future__ import annotations

import argparse
from html.entities import html5
from pathlib import Path
import re

NAMED_ENTITIES = {name[:-1] for name in html5 if name.endswith(";")}
TEMPLATE_TOKENS = re.compile(r"<unquote|~[asv]|&([a-zA-Z][a-zA-Z0-9]+);")
TEXT_SUFFIXES = {".html", ".xml", ".md", ".txt", ".svg"}


def find_leaks(text: str) -> list[str]:
    return [
        match.group(0)
        for match in TEMPLATE_TOKENS.finditer(text)
        if not (match.group(1) and match.group(1) in NAMED_ENTITIES)
    ]


def self_test() -> None:
    for valid in ("&amp;", "&lt;", "&uuml;", "&copy;", "literal data"):
        assert not find_leaks(valid), valid
    for leaked in ("&term;", "&rows;", "&unbound;", "<unquote", "~a", "~s", "~v"):
        assert find_leaks(leaked) == [leaked], leaked
    assert find_leaks("title &uuml; then &rows;") == ["&rows;"]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("site"))
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    self_test()
    if args.self_test:
        print("Exported-text template scanner regression cases passed")
        return 0
    if not args.root.is_dir():
        parser.error(f"export directory missing: {args.root}")
    errors = []
    checked = 0
    for path in args.root.rglob("*"):
        if path.suffix not in TEXT_SUFFIXES or not path.is_file():
            continue
        checked += 1
        for leak in find_leaks(path.read_text(encoding="utf-8")):
            errors.append(f"{path}: {leak!r}")
    if errors:
        print("\n".join(errors))
        return 1
    print(f"Exported template scan passed ({checked} text files)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
