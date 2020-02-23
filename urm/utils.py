from typing import List, Iterable

from urm.types import Statement, Zero, Succ, Trans, Jump


def used_memory_length(statements: Iterable[Statement]) -> int:
    """
    Returns size of used memory by the given program.
    """
    result = 0
    for s in statements:
        result = max(result, *s[:2])

    return result + 1


def shift_program(statements: Iterable[Statement], memory_shift: int, line_shift: int) -> Iterable[Statement]:
    """
    Rewrites statements to use other slice of memory and to be defined on other lines.
    """
    for s in statements:
        if isinstance(s, Zero):
            yield Zero(s.reg + memory_shift)
        elif isinstance(s, Succ):
            yield Succ(s.reg + memory_shift)
        elif isinstance(s, Trans):
            yield Trans(s.from_reg + memory_shift, s.to_reg + memory_shift)
        elif isinstance(s, Jump):
            yield Jump(s.reg1 + memory_shift, s.reg2 + memory_shift, s.goto + line_shift)
        else:
            raise ValueError("unknown statement type: {}", type(s))


def jumps_after_end(statements: List[Statement]) -> Iterable[Statement]:
    """
    Rewrites Jump statements to jump to the line right after the latest line.
    """
    length = len(statements)
    return jumps_to(statements, length + 1)


def jumps_before_start(statements: List[Statement]) -> Iterable[Statement]:
    """
    Rewrites Jump statements to jump to 0 line.
    """
    return jumps_to(statements, 0)


def jumps_to(statements: List[Statement], position: int) -> Iterable[Statement]:
    """
    Rewrites Jump statements to jump to the given position.
    """
    length = len(statements)
    for s in statements:
        if isinstance(s, Jump) and (s.goto > length or s.goto == 0):
            yield Jump(s.reg1, s.reg2, position)
        else:
            yield s


def marshal_program(statements: Iterable[Statement]) -> str:
    """
    Converts statement list to string.
    """
    return "\n".join(s.marshal() for s in statements)


def write_program(statements: Iterable[Statement], filename: str):
    """
    Writes statement list into file.
    """
    with open(filename, mode="w") as file:
        file.write(marshal_program(statements))


def gen_arg_copying(memory_shift: int, arg_len: int) -> List[Statement]:
    """
    Generates program that copies n input arguments to arguments memory of a program with the given memory shift.
    """
    return [Trans(arg_reg, arg_reg + memory_shift) for arg_reg in range(1, arg_len + 1)]


def gen_memory_zeroing(from_reg: int, to_reg: int) -> List[Statement]:
    """
    Generates sequence of statements which zeroes all registers in the range [from_reg, to_reg).
    """
    return [Zero(reg) for reg in range(from_reg, to_reg)]
