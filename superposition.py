import argparse
from typing import List

from urm.parsing import parse_file, nat
from urm.utils import Statement, marshal_program, Trans, used_memory_length, jumps_after_end, shift_program, \
    gen_arg_copying


def gen_sparse_copying(memory_shift: int, registers: List[int]) -> List[Statement]:
    """
    Generates program that copies given register programs to argument memory of a program with the given memory shift.
    """
    return [Trans(reg, memory_shift + i) for i, reg in enumerate(registers, start=1)]


def generate_superposition(g: List[Statement], fs: List[List[Statement]], arg_len: int) -> List[Statement]:
    """
    Generates superposition of the given g program and list f functions,
    so resulting program computes g(f1(args), ... fn(args)).

    The idea is simple: generate program that copies arguments for every subprogram and executes it,
    after all subprograms exited copy results to memory of composing function and execute it.
    Than copy its result to zero register and exit.
    """
    return_registers: List[int] = []  # return registers of subprograms
    superposition: List[Statement] = []  # resulting program

    memory_len = arg_len + 1  # occupied memory length
    for f in fs:
        return_registers.append(memory_len)  # save return register number for current subprogram

        f_memory_len = used_memory_length(f)  # calculate memory size of the program

        superposition.extend(gen_arg_copying(memory_len, arg_len))  # generate argument copying glue

        # make program jump to line right after program end for simple chaining
        # shift line positions and memory references in statements and add to resulting program
        superposition.extend(shift_program(jumps_after_end(f), memory_len, len(superposition)))

        memory_len += f_memory_len # extend memory size by exactly the size of f memory

    # generate result copying to g program
    superposition.extend(gen_sparse_copying(memory_len, return_registers))
    # move g program to end of memory and resulting superposition program
    superposition.extend(shift_program(jumps_after_end(g), memory_len, len(superposition)))
    # and add statement that copies result to 0
    superposition.append(Trans(memory_len, 0))
    return superposition


parser = argparse.ArgumentParser()
parser.add_argument("n", type=int)
parser.add_argument("g", type=str)
parser.add_argument("fs", type=str, nargs="*")

args = parser.parse_args()

g = parse_file(args.g)
fs = [parse_file(f_filename) for f_filename in args.fs]

print(marshal_program(generate_superposition(g, fs, nat(args.n))))
