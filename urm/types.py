from typing import NamedTuple, Union


class Zero(NamedTuple):
    reg: int

    def marshal(self) -> str:
        return "Z({})".format(self.reg)


class Succ(NamedTuple):
    reg: int

    def marshal(self) -> str:
        return "S({})".format(self.reg)


class Trans(NamedTuple):
    from_reg: int
    to_reg: int

    def marshal(self) -> str:
        return "T({}, {})".format(self.from_reg, self.to_reg)


class Jump(NamedTuple):
    reg1: int
    reg2: int
    goto: int

    def marshal(self) -> str:
        return "J({}, {}, {})".format(self.reg1, self.reg2, self.goto)


Statement = Union[Zero, Succ, Trans, Jump]
