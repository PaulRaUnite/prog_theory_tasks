import argparse
from typing import List

from urm.parsing import parse_file, nat
from urm.types import Statement, Trans, Jump, Succ, Zero
from urm.utils import marshal_program, gen_arg_copying, shift_program, jumps_after_end, \
    used_memory_length, gen_memory_zeroing


def generate_recursion(f: List[Statement], g: List[Statement], arg_len: int) -> List[Statement]:
    """
    Generates program h that computes primitive recursion using given functions f and g such that

    h(x1,..., xn, 0) = f(x1,..., xn)
    h(x1,..., xn, y+1) = g(x1,..., xn, y, h(x1,..., xn, y))
    """
    recursion: List[Statement] = []
    f_g_max_memory_size = max(used_memory_length(f), used_memory_length(g))

    aux_mem_len = arg_len + 3  # return register + args + y + loop counter
    recursion.extend(gen_arg_copying(aux_mem_len, arg_len))  # copy arguments to f
    # compute f
    recursion.extend(shift_program(jumps_after_end(f), aux_mem_len, len(recursion)))

    loop_start_line = len(recursion) + 1

    f_g_result_register = aux_mem_len
    y_register = arg_len + 1
    counter_register = arg_len + 2

    recursion.extend([
        Trans(f_g_result_register, 0),  # copy result to 0
        Jump(y_register, counter_register, 0),  # check if should exit
        Succ(counter_register),  # counter+=1
        Zero(f_g_result_register),  # empty result register
    ])
    # zeroes non-argument memory of g
    recursion.extend(gen_memory_zeroing(aux_mem_len + arg_len + 3, aux_mem_len + f_g_max_memory_size))
    # copy arguments
    recursion.extend(gen_arg_copying(aux_mem_len, arg_len))

    g_y_register = aux_mem_len + arg_len + 1
    g_h_register = aux_mem_len + arg_len + 2
    recursion.extend([
        Trans(counter_register, g_y_register),  # copy counter
        Trans(0, g_h_register),  # copy result of previous loop
    ])
    # compute g
    recursion.extend(shift_program(jumps_after_end(g), aux_mem_len, len(recursion)))
    # jump to loop start (and condition to exit)
    recursion.append(Jump(0, 0, loop_start_line))

    return recursion


parser = argparse.ArgumentParser()
parser.add_argument("n", type=int)
parser.add_argument("f", type=str)
parser.add_argument("g", type=str)

args = parser.parse_args()

f = parse_file(args.f)
g = parse_file(args.g)

print(marshal_program(generate_recursion(f, g, nat(args.n))))
