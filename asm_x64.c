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
        uint8_t reg;
        uint64_t imm;
        struct {
            uint8_t size;
            uint8_t base;
            uint8_t index;
            uint8_t scale;
            uint32_t disp;
        };
    };
} Operand;

Operand reg(uint8_t r) {
    assert(r < 16);
    Operand x = {.kind = REG, .reg = r};
    return x;
}

Operand imm(uint64_t i) {
    Operand x = {.kind = IMM, .imm = i};
    return x;
}

Operand mem(uint8_t base, uint8_t index, uint8_t scale, uint8_t disp) {
    assert(base < 16);
    assert(index <= 16);
    assert(scale < 4);
    Operand x = {.kind = MEM, .size = 8, .base = base, .index = index, .scale = scale, .disp = disp};
    return x;
}

Operand sized(Operand x, int size) {
    assert(x.kind == MEM);
    assert(size == 1 || size == 2 || size == 4 || size == 8);
    x.size = size;
    return x;
}

Operand byte_ptr(Operand x) {
    return sized(x, 1);
}

Operand word_ptr(Operand x) {
    return sized(x, 2);
}

Operand dword_ptr(Operand x) {
    return sized(x, 4);
}

Operand base(uint8_t base) {
    return mem(base, 16, X1, 0);
}

Operand base_index(uint8_t base, uint8_t index) {
    return mem(base, index, X1, 0);
}

Operand base_index_scale(uint8_t base, uint8_t index, uint8_t scale) {
    return mem(base, index, scale, 0);
}

Operand base_disp(uint8_t base, uint32_t disp) {
    return mem(base, 16, X1, disp);
}

Operand base_index_disp(uint8_t base, uint8_t index, uint32_t disp) {
    return mem(base, index, X1, disp);
}

Operand base_index_scale_disp(uint8_t base, uint8_t index, uint8_t scale, uint32_t disp) {
    return mem(base, index, scale, disp);
}

char *here;

void emit(uint64_t data, int len) {
    assert(len <= 8);
    *(uint64_t *)here = data;
    here += len;
}

enum {
    ADD, AND
};

//    op    reg_rm  rm_reg  rm_imm8  rm_imm8x  rm_imm32  rm_imm32x
#define BINARY_OPS(_) \
    _(ADD,  0x03,   0x01,   0x83,    0x00,     0x81,     0x00) \
    _(AND,  0x23,   0x21,   0x83,    0x04,     0x81,     0x04)

#define BINARY_REG_RM(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = reg_rm; opcodelen = 1; immlen = 0; rx = dest.reg; rm = src; break;

#define BINARY_RM_REG(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = rm_reg; opcodelen = 1; immlen = 0; rx = src.reg; rm = dest; break;

#define BINARY_RM_IMM8(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = rm_imm8; opcodelen = 1; immlen = 1; rx = rm_imm8x; rm = dest; break;

#define BINARY_RM_IMM32(op, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    case op: opcode = rm_imm32; opcodelen = 1; immlen = 4; rx = rm_imm32x; rm = dest; break;

INLINE void asm_rx_rm(uint64_t rx, Operand rm, uint64_t *prefix, int *prefixlen, uint64_t *addr, int *addrlen) {
    if (rm.kind == REG) {
        assert(rm.kind == REG);
        *prefix = rex(rx, rm.reg);
        *prefixlen = 1;
        *addr = direct(rx, rm.reg);
        *addrlen = 1;
    } else {
        assert(rm.kind == MEM);
        if (rm.index == 16) {
            *prefix = rex(rx, rm.base);
            *prefixlen = 1;
            if (rm.disp || (rm.base & 7) == RBP) {
                if (rm.disp + 128 < 256) {
                    *addr = indirect_disp8(rx, rm.base, rm.disp);
                    *addrlen = 2;
                } else {
                    *addr = indirect_disp32(rx, rm.base, rm.disp);
                    *addrlen = 5;
                }
            } else {
                *addr = indirect(rx, rm.base);
                *addrlen = 1;
            }
        } else {
            *prefix = rex_index(rx, rm.base, rm.index);
            *prefixlen = 1;
            if (rm.disp || (rm.base & 7) == RBP) {
                if (rm.disp + 128 < 256) {
                    *addr = indirect_index_disp8(rx, rm.base, rm.index, rm.scale, rm.disp);
                    *addrlen = 3;
                } else {
                    *addr = indirect_index_disp32(rx, rm.base, rm.index, rm.scale, rm.disp);
                    *addrlen = 6;
                }
            } else {
                *addr = indirect_index(rx, rm.base, rm.index, rm.scale);
                *addrlen = 2;
            }
        }
    }
}

void asm_binary(uint64_t op, Operand dest, Operand src) {
    uint64_t opcode, rx;
    int opcodelen, immlen;
    Operand rm;
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
    uint64_t prefix, addr;
    int prefixlen, addrlen;
    asm_rx_rm(rx, rm, &prefix, &prefixlen, &addr, &addrlen);
    uint64_t instr = prefix | (opcode << (8 * prefixlen)) | (addr << (8 * (prefixlen + opcodelen)));
    int instrlen = prefixlen + opcodelen + addrlen;
    emit(instr, instrlen);
    if (immlen) {
        emit(src.imm, immlen);
    }
}

uint32_t *asm_jump(const char *target) {
    uint32_t offset = (uint32_t)(target - (here + 5));
    emit(0xE9 | (offset << 8), 5);
    return (uint32_t *)(here - 4);
}

uint32_t *asm_jump_if(uint64_t cond, const char *target) {
    assert(cond < 16);
    uint32_t offset = (uint32_t)(target - (here + 6));
    emit(0x800F | (cond << 8) | (offset << 16), 6);
    return (uint32_t *)(here - 4);
}

void asm_patch_jump(uint32_t *jump_field, const char *target) {
    *jump_field = (uint32_t)(target - ((char *)jump_field + 4));
}

void test_asm(void) {
    static char buf[1024];
    here = buf;
    asm_jump(here);
    char *addr1 = here;
    uint32_t *field1 = asm_jump(0);
    uint32_t *field2 = asm_jump_if(Z, 0);
    char *addr2 = here;
    asm_jump_if(NZ, addr1);
    asm_patch_jump(field1, here);
    asm_patch_jump(field2, addr2);
    // asm_jump(127);
    // asm_jump(0x1234);
    for (int op = 0; op < 2; op++) {
        asm_binary(op, reg(RAX), reg(R8));
        asm_binary(op, reg(R8), imm(-1));
        asm_binary(op, reg(R8), imm(-128));
        asm_binary(op, reg(R8), imm(-129));
        asm_binary(op, reg(R8), imm(127));
        asm_binary(op, reg(R8), imm(128));
        asm_binary(op, reg(R8), imm(0x12345678));
        asm_binary(op, reg(RAX), imm(0xFFFFFFFF));
        asm_binary(op, reg(RAX), base(R8));
        asm_binary(op, base_index(R8, R9), reg(RAX));
        asm_binary(op, base(RBX), reg(RAX));
        asm_binary(op, base(RBX), imm(-1));
        asm_binary(op, base(RBX), imm(0x12345678));
        asm_binary(op, reg(RAX), base_index(RBX, RCX));
        asm_binary(op, base_index(RBX, RCX), reg(RAX));
        asm_binary(op, base_index(RBX, RCX), imm(-1));
        asm_binary(op, base_index(RBX, RCX), imm(0x12345678));
        asm_binary(op, reg(RAX), base_index_scale(RBX, RCX, X4));
        asm_binary(op, base_index_scale(RBX, RCX, X4), reg(RAX));
        asm_binary(op, base_index_scale(RBX, RCX, X4), imm(-1));
        asm_binary(op, base_index_scale(RBX, RCX, X4), imm(0x12345678));
        asm_binary(op, base_index_scale_disp(RBX, RCX, X4, 0x12345678), imm(0x12345678));
        asm_binary(op, reg(RAX), base_disp(RBX, -1));
        asm_binary(op, base_disp(RBX, -1), reg(RAX));
        asm_binary(op, reg(RAX), base_disp(RBX, -128));
        asm_binary(op, base_disp(RBX, -128), reg(RAX));
        asm_binary(op, reg(RAX), base_disp(RBX, 127));
        asm_binary(op, base_disp(RBX, 127), reg(RAX));
        asm_binary(op, reg(RAX), base_disp(RBX, 0x1245678));
        asm_binary(op, base_disp(RBX, 0x1245678), reg(RAX));
        asm_binary(op, reg(RAX), base_index_disp(RBX, RCX, -1));
        asm_binary(op, base_index_disp(RBX, RCX, -1), reg(RAX));
        asm_binary(op, reg(RAX), base_index_disp(RBX, RCX, -128));
        asm_binary(op, base_index_disp(RBX, RCX, -128), reg(RAX));
        asm_binary(op, reg(RAX), base_index_disp(RBX, RCX, 127));
        asm_binary(op, base_index_disp(RBX, RCX, 127), reg(RAX));
        asm_binary(op, reg(RAX), base_index_disp(RBX, RCX, 0x12345678));
        asm_binary(op, base_index_disp(RBX, RCX, 0x12345678), reg(RAX));
        asm_binary(op, reg(RAX), base_index_scale_disp(RBX, RCX, X4, -1));
        asm_binary(op, base_index_scale_disp(RBX, RCX, X4, -1), reg(RAX));
        asm_binary(op, reg(RAX), base_index_scale_disp(RBX, RCX, X4, -128));
        asm_binary(op, base_index_scale_disp(RBX, RCX, X4, -128), reg(RAX));
        asm_binary(op, reg(RAX), base_index_scale_disp(RBX, RCX, X4, 127));
        asm_binary(op, base_index_scale_disp(RBX, RCX, X4, 127), reg(RAX));
        asm_binary(op, reg(RAX), base_index_scale_disp(RBX, RCX, X4, 0x12345678));
        asm_binary(op, base_index_scale_disp(RBX, RCX, X4, 0x12345678), reg(RAX));
        asm_binary(op, reg(RAX), base(RBP));
        asm_binary(op, reg(RAX), base_index(RBP, RBX));
    }
    __debugbreak(); // Enter disassembly on break and set address to buf
}
