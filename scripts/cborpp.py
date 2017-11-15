#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import errno
import pprint
import argparse

from common import die

try:
    import cbor2
except ImportError:
    # run `pip install cbor2` or `easy_install cbor2` to fix
    print("error: python package cbor2 is not installed.", file=sys.stderr)
    sys.exit(errno.ENOENT)


def _parse_args():
    """
    define and parse command line arguments here.
    """
    desc = 'Pretty print CBOR file output by AST extractor.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('cbor', type=argparse.FileType('rb'),
                        help="cbor file to pretty print.")
    parser.add_argument("--indent", "-i", dest="indent",
                        type=int, default=2, nargs='?',
                        help="spaces per indent.")
    parser.add_argument("--depth", "-d", dest="depth",
                        type=int, default=2, nargs='?',
                        help="max level of indentation.")
    return parser.parse_args()


TAGS = {
    0: "TagFunctionDecl",
    1: "TagParmVarDecl",
    2: "TagVarDecl",
    3: "TagRecordDecl",
    4: "TagFieldDecl",

    100: "TagCompoundStmt",

    200: "TagBinaryOperator",
    201: "TagUnaryOperator",
    202: "TagDeclRefExpr",
    203: "TagImplicitCastExpr",
    204: "TagCallExpr",

    400: "TagTypeUnknown",

    500: "TagInt",
    501: "TagShort",
    502: "TagLong",
    503: "TagLongLong",
    504: "TagUInt",

    505: "TagUShort",
    506: "TagULong",
    507: "TagULongLong",
    508: "TagPointer",
    509: "TagRecordType",

    510: "TagDouble",
    511: "TagLongDouble",
    512: "TagFloat",
    513: "TagConstantArrayType",
    514: "TagVariableArrayType",

    515: "TagIncompleteArrayType",
    516: "TagEnumType",
    517: "TagFunctionType",
    518: "TagTypeOfType",
    519: "TagTypedefType",

    520: "TagElaboratedType",
    521: "TagUChar",
    522: "TagSChar",
    523: "TagChar",
    524: "TagVoid",

    525: "TagBool",
    526: "TagDecayedType",
    527: "TagParenType",
}


def _main():
    args = _parse_args()
    try:
        array = cbor2.load(args.cbor)
    except cbor2.CBORDecodeError as de:
        die("CBOR decoding error:" + str(de))

    # translate tags
    for e in array:
        assert len(e) >= 2
        e[1] = TAGS[e[1]] if e[1] in TAGS else "MissingTag"

    pprint.pprint(array, indent=args.indent, depth=args.depth)


if __name__ == "__main__":
    _main()
