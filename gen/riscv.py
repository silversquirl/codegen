#!/usr/bin/env python3
# RISC-V assembler generator
# Generates assembler functions based on descriptions from the riscv-opcodes repo
import itertools
import os
import subprocess
import sys

os.chdir(os.path.dirname(__file__))
if os.path.exists("riscv-opcodes"):
    subprocess.call(["git", "-C", "riscv-opcodes", "pull"])
else:
    subprocess.call(
        [
            "git",
            "clone",
            "https://github.com/riscv/riscv-opcodes",
        ]
    )

sys.path.append("./riscv-opcodes/")
import parse

exts = [
    "i",
    "m",
    "a",
    "f",
    "d",
]


insns = parse.create_inst_dict(
    map("_".join, itertools.product(["rv", "rv32", "rv64"], exts)),
    include_pseudo=True,
)

source = """
pub const OpcodeDesc = struct {
    name: [:0]const u8,
    match: u32,
    mask: u32,
    operands: []const Operand,
    variable_fields: []const Field,
    extensions: []const Extension,
};

pub const opcodes: []const OpcodeDesc = &.{
"""

operand_map = {
    "bimm12lo": "imm13",
    "bimm12hi": None,
    "imm12lo": "imm12",
    "imm12hi": None,
    "imm20": "imm32",
    "jimm20": "imm21",
}

all_operands = set()
all_fields = set()
all_exts = set()
for name, info in insns.items():
    fields: list[str] = info["variable_fields"]
    if "rm" in fields:
        # For some reason the ordering is broken here for some (all?) instructions
        fields.remove("rm")
        fields.insert(1, "rm")

    operands = []
    for field in fields:
        if field in operand_map:
            operand = operand_map[field]
            if operand is None:
                continue
        else:
            operand = field
        operands.append(operand)
        all_operands.add(operand)
    operands_zig = ", ".join(["." + f for f in operands])

    all_fields = all_fields.union(set(fields))
    fields_zig = ", ".join(["." + f for f in fields])

    all_exts = all_exts.union(set(info["extension"]))
    exts_zig = ", ".join(["." + ext for ext in info["extension"]])

    source += f""".{{
    .name = "{name}",
    .match = {info["match"]},
    .mask = {info["mask"]},
    .operands = &.{{ {operands_zig} }},
    .variable_fields = &.{{ {fields_zig} }},
    .extensions = &.{{ {exts_zig} }},
    }},
    """

source += """
};

pub const Operand = enum {
"""

for operand in sorted(all_operands):
    source += f"{operand},\n"

source += """
};

pub const Field = enum {
"""

for field in sorted(all_fields):
    source += f"{field},\n"

source += """
};

pub const Extension = enum {
"""

for ext in sorted(all_exts):
    source += f"{ext},\n"

source += "};\n"


with open("../src/asm/riscv/opcodes.zig", "w") as zig_file:
    fmt = subprocess.Popen(
        ["zig", "fmt", "--stdin"],
        stdin=subprocess.PIPE,
        stdout=zig_file,
    )
    fmt.communicate(source.encode("utf-8"))
