enum {
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8,  R9,  R10, R11, R12, R13, R14, R15
};

enum {
    X1, X2, X4, X8
};

enum {
    O, NO, B, NB, E, NE, NA, A, S, NS, P, NP, L, NL, NG, G,
    NAE = B, C = B, AE = NB, NC = NB, Z = E, NZ = NE, BE = NA,
    NBE = A, PE = P, PO = NP, NGE = L, GE = NL, LE = NG, NLE = G
};

enum {
    INDIRECT, INDIRECT_DISP8, INDIRECT_DISP32, DIRECT
};

uint64_t mod_rx_rm(uint64_t mod, uint64_t rx, uint64_t rm) {
    assert(mod < 4);
    assert(rx < 16);
    assert(rm < 16);
    return (rm & 7) | ((rx & 7) << 3) | (mod << 6); // 1
}

uint64_t rex_index(uint64_t rx, uint64_t base, uint64_t index) {
    return 0x48 | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2); // 1
}

uint64_t rex(uint64_t rx, uint64_t base) {
    return rex_index(rx, base, 0); // 1
}

uint64_t direct(uint64_t rx, uint64_t reg) {
    return mod_rx_rm(DIRECT, rx, reg); // 1
}

uint64_t indirect(uint64_t rx, uint64_t base) {
    assert((base & 7) != RSP);
    assert((base & 7) != RBP);
    return mod_rx_rm(INDIRECT, rx, base); // 1
}

uint64_t indirect_rip_disp32(uint64_t rx, uint64_t disp) {
    return mod_rx_rm(INDIRECT, rx, RBP) | (disp << 8); // 5
}

uint64_t indirect_disp8(uint64_t rx, uint64_t base, uint64_t disp) {
    assert((base & 7) != RSP);
    return mod_rx_rm(INDIRECT_DISP8, rx, base) | (disp << 8); // 2
}

uint64_t indirect_disp32(uint64_t rx, uint64_t base, uint64_t disp) {
    assert((base & 7) != RSP);
    return mod_rx_rm(INDIRECT_DISP32, rx, base) | (disp << 8); // 5
}

uint64_t indirect_index(uint64_t rx, uint64_t base, uint64_t index, uint64_t scale) {
    assert((base & 7) != RBP);
    return mod_rx_rm(INDIRECT, rx, RSP) | (mod_rx_rm(scale, index, base) << 8); // 2
}

uint64_t indirect_index_disp8(uint64_t rx, uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    return mod_rx_rm(INDIRECT_DISP8, rx, RSP) | (mod_rx_rm(scale, index, base) << 8) | (disp << 16); // 3
}

uint64_t indirect_index_disp32(uint64_t rx, uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    return mod_rx_rm(INDIRECT_DISP32, rx, RSP) | (mod_rx_rm(scale, index, base) << 8) | (disp << 16); // 6
}

uint64_t disp32(uint64_t rx, uint64_t disp) {
    return mod_rx_rm(INDIRECT, rx, RSP) | (mod_rx_rm(X1, RSP, RBP) << 8) | (disp << 16); // 6
}

typedef enum {
    REG, IMM, MEM
} OperandKind;

typedef struct {
    OperandKind kind;
    union {
        uint64_t reg;
        uint64_t imm;
        struct {
            uint64_t base;
            uint64_t index;
            uint64_t scale;
            uint64_t disp;
        };
    };
} Operand;

Operand reg(uint64_t r) {
    assert(r < 16);
    Operand x = {.kind = REG, .reg = r};
    return x;
}

Operand imm(uint64_t i) {
    Operand x = {.kind = IMM, .imm = i};
    return x;
}

Operand mem(uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    assert(base < 16);
    assert(index < 16 || index == -1);
    assert(scale < 4);
    Operand x = {.kind = MEM, .base = base, .index = index, .scale = scale, .disp = disp};
    return x;
}

Operand base(uint64_t base) {
    return mem(base, -1, X1, 0);
}

Operand base_index(uint64_t base, uint64_t index) {
    return mem(base, index, X1, 0);
}

Operand base_index_scale(uint64_t base, uint64_t index, uint64_t scale) {
    return mem(base, index, scale, 0);
}

Operand base_disp(uint64_t base, uint64_t disp) {
    return mem(base, -1, X1, disp);
}

Operand base_index_disp(uint64_t base, uint64_t index, uint64_t disp) {
    return mem(base, index, X1, disp);
}

Operand base_index_scale_disp(uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    return mem(base, index, scale, disp);
}

char *out;

void emit(uint64_t data, int len) {
    assert(len <= 8);
    *(uint64_t *)out = data;
    out += len;
}

enum {
    ADD
};

//    op    reg_rm  rm_reg  rm_imm8  rm_imm8x  rm_imm32  rm_imm32x
#define BINARY_OPS(_) \
    _(ADD,  0x03,   0x01,   0x83,    0x00,     0x81,     0x00)

#define BINARY_REG_RM(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = reg_rm; opcodelen = 1; rx = dest.reg; immlen = 0; break;

#define BINARY_RM_REG(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = rm_reg; opcodelen = 1; rx = src.reg; immlen = 0; break;

#define BINARY_RM_IMM8(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = rm_imm8; opcodelen = 1; rx = rm_imm8x; immlen = 1; break;

#define BINARY_RM_IMM32(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = rm_imm32; opcodelen = 1; rx = rm_imm32x; immlen = 4; break;

void asm_binary(uint64_t op, Operand dest, Operand src) {
    uint64_t opcode;
    int opcodelen;
    uint64_t rx;
    int immlen;
    if (src.kind == REG) {
        switch (op) {
            BINARY_OPS(BINARY_RM_REG)
        default:
            assert(0);
        }
    } else if (src.kind == MEM) {
        assert(dest.kind == REG);
        switch (op) {
            BINARY_OPS(BINARY_REG_RM)
        default:
            assert(0);
        }
    } else if (src.kind == IMM) {
        if (src.imm + 128 < 256) {
            switch (op) {
                BINARY_OPS(BINARY_RM_IMM8)
            default:
                assert(0);
            }
        } else {
            switch (op) {
                BINARY_OPS(BINARY_RM_IMM32)
            default:
                assert(0);
            }
        }
    } else {
        assert(0);
    }
    uint64_t prefix;
    int prefixlen;
    uint64_t addr;
    int addrlen;
    if (src.kind == MEM || dest.kind == MEM) {
        Operand mem = src.kind == MEM ? src : dest;
        if (mem.index == -1) {
            prefix = rex(rx, mem.base);
            prefixlen = 1;
            if (mem.disp || (mem.base & 7) == RBP) {
                if (mem.disp + 128 < 256) {
                    addr = indirect_disp8(rx, mem.base, mem.disp);
                    addrlen = 2;
                } else {
                    addr = indirect_disp32(rx, mem.base, mem.disp);
                    addrlen = 5;
                }
            } else {
                addr = indirect(rx, mem.base);
                addrlen = 1;
            }
        } else {
            prefix = rex_index(rx, mem.base, mem.index);
            prefixlen = 1;
            if (mem.disp || (mem.base & 7) == RBP) {
                if (mem.disp + 128 < 256) {
                    addr = indirect_index_disp8(rx, mem.base, mem.index, mem.scale, mem.disp);
                    addrlen = 3;
                } else {
                    addr = indirect_index_disp32(rx, mem.base, mem.index, mem.scale, mem.disp);
                    addrlen = 6;
                }
            } else {
                addr = indirect_index(rx, mem.base, mem.index, mem.scale);
                addrlen = 2;
            }
        }
    } else { 
        assert(dest.kind == REG);
        prefix = rex(rx, dest.reg);
        prefixlen = 1;
        addr = direct(rx, dest.reg);
        addrlen = 1;
    }
    uint64_t instr = prefix | (opcode << (8 * prefixlen)) | (addr << (8 * (prefixlen + opcodelen)));
    int instrlen = prefixlen + opcodelen + addrlen;
    emit(instr, instrlen);
    if (immlen) {
        emit(src.imm, immlen);
    }
}

void test_asm(void) {
    static char buf[1024];
    out = buf;
    #if 1
    asm_binary(ADD, reg(RAX), reg(R8));
    asm_binary(ADD, reg(R8), imm(-1));
    asm_binary(ADD, reg(R8), imm(-128));
    asm_binary(ADD, reg(R8), imm(127));
    asm_binary(ADD, reg(R8), imm(0x12345678));
    asm_binary(ADD, reg(RAX), imm(0xFFFFFFFF));
    #endif
    asm_binary(ADD, reg(RAX), base(R8));
    asm_binary(ADD, base_index(R8, R9), reg(RAX));
    asm_binary(ADD, base(RBX), reg(RAX));
    asm_binary(ADD, base(RBX), imm(-1));
    asm_binary(ADD, base(RBX), imm(0x12345678));
    asm_binary(ADD, reg(RAX), base_index(RBX, RCX));
    asm_binary(ADD, base_index(RBX, RCX), reg(RAX));
    asm_binary(ADD, base_index(RBX, RCX), imm(-1));
    asm_binary(ADD, base_index(RBX, RCX), imm(0x12345678));
    asm_binary(ADD, reg(RAX), base_index_scale(RBX, RCX, X4));
    asm_binary(ADD, base_index_scale(RBX, RCX, X4), reg(RAX));
    asm_binary(ADD, base_index_scale(RBX, RCX, X4), imm(-1));
    asm_binary(ADD, base_index_scale(RBX, RCX, X4), imm(0x12345678));
    asm_binary(ADD, base_index_scale_disp(RBX, RCX, X4, 0x12345678), imm(0x12345678));
    asm_binary(ADD, reg(RAX), base_disp(RBX, -1));
    asm_binary(ADD, base_disp(RBX, -1), reg(RAX));
    asm_binary(ADD, reg(RAX), base_disp(RBX, -128));
    asm_binary(ADD, base_disp(RBX, -128), reg(RAX));
    asm_binary(ADD, reg(RAX), base_disp(RBX, 127));
    asm_binary(ADD, base_disp(RBX, 127), reg(RAX));
    asm_binary(ADD, reg(RAX), base_disp(RBX, 0x1245678));
    #if 1
    asm_binary(ADD, base_disp(RBX, 0x1245678), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_disp(RBX, RCX, -1));
    asm_binary(ADD, base_index_disp(RBX, RCX, -1), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_disp(RBX, RCX, -128));
    asm_binary(ADD, base_index_disp(RBX, RCX, -128), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_disp(RBX, RCX, 127));
    asm_binary(ADD, base_index_disp(RBX, RCX, 127), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_disp(RBX, RCX, 0x12345678));
    asm_binary(ADD, base_index_disp(RBX, RCX, 0x12345678), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_scale_disp(RBX, RCX, X4, -1));
    asm_binary(ADD, base_index_scale_disp(RBX, RCX, X4, -1), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_scale_disp(RBX, RCX, X4, -128));
    asm_binary(ADD, base_index_scale_disp(RBX, RCX, X4, -128), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_scale_disp(RBX, RCX, X4, 127));
    asm_binary(ADD, base_index_scale_disp(RBX, RCX, X4, 127), reg(RAX));
    asm_binary(ADD, reg(RAX), base_index_scale_disp(RBX, RCX, X4, 0x12345678));
    asm_binary(ADD, base_index_scale_disp(RBX, RCX, X4, 0x12345678), reg(RAX));
    asm_binary(ADD, reg(RAX), base(RBP));
    asm_binary(ADD, reg(RAX), base_index(RBP, RBX));
    #endif
    __debugbreak(); // enter disassembly on break and set address to buf
}
