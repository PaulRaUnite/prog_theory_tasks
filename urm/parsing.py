from typing import List, Any

from urm.types import Statement, Zero, Succ, Trans, Jump


def parse_file(filename: str) -> List[Statement]:
    with open(filename) as file:
        return [parse_line(line) for line in file.readlines()]


def nat(x: Any) -> int:
    n = int(x)
    if n < 0:
        raise ValueError("natural numbers cannot be negative")
    return n


def parse_line(line: str) -> Statement:
    supported_statements = {'Z': Zero, 'S': Succ, 'T': Trans, 'J': Jump}

    code, _, raw_args = line.partition('(')
    code = code.strip()
    raw_args, _, _ = raw_args.strip().partition(')')

    try:
        args = (nat(arg) for arg in raw_args.split(','))
    except ValueError as e:
        raise ValueError("invalid statement format: '{}'".format(line)) from e

    try:
        constructor = supported_statements[code]
    except KeyError as ke:
        raise ValueError("unknown statement code: '{}'".format(line)) from ke

    return constructor(*args)
