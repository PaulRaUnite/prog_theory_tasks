import argparse
from typing import List

from urm.parsing import parse_file, nat
from urm.types import Statement, Trans, Jump, Succ, Zero
from urm.utils import marshal_program, gen_arg_copying, shift_program, jumps_after_end, \
    used_memory_length, gen_memory_zeroing


def generate_minimization(f: List[Statement], arg_len: int) -> List[Statement]:
    """
    Generates program that finds y for which f(x1, ..., xn, y) = 0.
    """

    minimization: List[Statement] = []

    f_memory_len = used_memory_length(f)
    aux_mem_len = arg_len + 2  # return register + args + loop counter

    # clear memory
    minimization.extend(gen_memory_zeroing(aux_mem_len + arg_len + 2, aux_mem_len + f_memory_len))
    # copy arguments to f
    minimization.extend(gen_arg_copying(aux_mem_len, arg_len + 1))
    # run f
    minimization.extend(shift_program(jumps_after_end(f), aux_mem_len, len(minimization)))

    minimization.extend([
        Jump(0, aux_mem_len, len(minimization) + 4),  # found solution, goto copying result
        Succ(arg_len + 1),  # counter += 1
        Jump(0, 0, 1),  # repeat loop
        Trans(aux_mem_len - 1, 0)  # copy result to 0 register
    ])

    return minimization


parser = argparse.ArgumentParser()
parser.add_argument("n", type=int)
parser.add_argument("f", type=str)

args = parser.parse_args()

f = parse_file(args.f)

print(marshal_program(generate_minimization(f, nat(args.n))))
