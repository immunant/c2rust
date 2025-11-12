#!/usr/bin/env -S uv run
# -*- coding: utf-8 -*-

import sys
import errno
import pprint
import argparse

from common import die

try:
    import cbor2
except ImportError:
    # add `cbor2` to `requirements.txt` to fix
    print("error: python package cbor2 is not installed.", file=sys.stderr)
    sys.exit(errno.ENOENT)


def _parse_args() -> argparse.Namespace:
    """
    define and parse command line arguments here.
    """
    desc = 'Pretty print CBOR file output by AST exporter.'
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

    5: "TagEnumDecl",
    6: "TagEnumConstantDecl",
    7: "TagTypedefDecl",
    8: "TagUnionDecl",

    100: "TagCompoundStmt",
    101: "TagReturnStmt",
    102: "TagIfStmt",
    103: "TagGotoStmt",
    104: "TagLabelStmt",

    105: "TagNullStmt",
    106: "TagForStmt",
    107: "TagWhileStmt",
    108: "TagSwitchStmt",
    109: "TagDeclStmt",

    110: "TagBreakStmt",
    111: "TagEnuTagCaseStmtmDecl",
    112: "TagContinueStmt",
    113: "TagDefaultStmt",
    114: "TagDoStmt",

    200: "TagBinaryOperator",
    201: "TagUnaryOperator",
    202: "TagDeclRefExpr",
    203: "TagImplicitCastExpr",
    204: "TagCallExpr",

    205: "TagInitListExpr",
    206: "TagImplicitValueInitExpr",
    207: "TagArraySubscriptExpr",
    208: "TagCStyleCastExpr",
    209: "TagConditionalOperator",

    210: "TagBinaryConditionalOperator",
    211: "TagMemberExpr",
    212: "TagParenExpr",
    213: "TagUnaryExprOrTypeTraitExpr",
    214: "TagCompoundLiteralExpr",

    215: "TagPredefinedExpr",

    300: "TagIntegerLiteral",
    301: "TagStringLiteral",
    302: "TagCharacterLiteral",
    303: "TagFloatingLiteral",

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
    509: "TagStructType",

    510: "TagUnionType",
    511: "TagDouble",
    512: "TagLongDouble",
    513: "TagFloat",
    514: "TagConstantArrayType",

    515: "TagVariableArrayType",
    516: "TagIncompleteArrayType",
    517: "TagEnumType",
    518: "TagFunctionType",
    519: "TagTypeOfType",

    520: "TagTypedefType",
    521: "TagElaboratedType",
    522: "TagUChar",
    523: "TagSChar",
    524: "TagChar",

    525: "TagVoid",
    526: "TagBool",
    527: "TagDecayedType",
    528: "TagParenType",
    529: "TagSWChar",

    530: "TagUWChar",
    531: "TagInt128",
    532: "TagUInt128",
    533: "TagBuiltinFn",
    534: "TagAttributedType",

    535: "TagBlockPointer",

    600: "TagUWChar",
    601: "TagInt128",
    602: "TagUInt128",
    603: "TagBuiltinFn",
    604: "TagAttributedType",
}


def _main() -> None:
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
