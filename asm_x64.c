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

INLINE uint64_t rexw(uint64_t rx, uint64_t base, uint64_t index) {
    return 0x48 | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2); // 1
}

INLINE uint64_t mod_rx_rm(uint64_t mod, uint64_t rx, uint64_t rm) {
    assert(mod < 4);
    assert(rx < 16);
    assert(rm < 16);
    return (rm & 7) | ((rx & 7) << 3) | (mod << 6); // 1
}

INLINE uint64_t direct(uint64_t rx, uint64_t reg) {
    return mod_rx_rm(DIRECT, rx, reg); // 1
}

INLINE uint64_t indirect(uint64_t rx, uint64_t base) {
    assert((base & 7) != RSP);
    assert((base & 7) != RBP);
    return mod_rx_rm(INDIRECT, rx, base); // 1
}

INLINE uint64_t indirect_rip_disp32(uint64_t rx, uint64_t disp) {
    return mod_rx_rm(INDIRECT, rx, RBP) | (disp << 8); // 5
}

INLINE uint64_t indirect_disp8(uint64_t rx, uint64_t base, uint64_t disp) {
    assert((base & 7) != RSP);
    return mod_rx_rm(INDIRECT_DISP8, rx, base) | (disp << 8); // 2
}

INLINE uint64_t indirect_disp32(uint64_t rx, uint64_t base, uint64_t disp) {
    assert((base & 7) != RSP);
    return mod_rx_rm(INDIRECT_DISP32, rx, base) | (disp << 8); // 5
}

INLINE uint64_t indirect_index(uint64_t rx, uint64_t base, uint64_t index, uint64_t scale) {
    assert((base & 7) != RBP);
    return mod_rx_rm(INDIRECT, rx, RSP) | (mod_rx_rm(scale, index, base) << 8); // 2
}

INLINE uint64_t indirect_index_disp8(uint64_t rx, uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    return mod_rx_rm(INDIRECT_DISP8, rx, RSP) | (mod_rx_rm(scale, index, base) << 8) | (disp << 16); // 3
}

INLINE uint64_t indirect_index_disp32(uint64_t rx, uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    return mod_rx_rm(INDIRECT_DISP32, rx, RSP) | (mod_rx_rm(scale, index, base) << 8) | (disp << 16); // 6
}

INLINE uint64_t disp32(uint64_t rx, uint64_t disp) {
    return mod_rx_rm(INDIRECT, rx, RSP) | (mod_rx_rm(X1, RSP, RBP) << 8) | (disp << 16); // 6
}

char *here;

INLINE void emit(uint64_t data, int len) {
    assert(len <= 8);
    *(uint64_t *)here = data;
    here += len;
}

INLINE void emit_instr(uint64_t rx, uint64_t base, uint64_t index, uint64_t opcode, int opcodelen, uint64_t ext, int extlen) {
    uint64_t prefix = rexw(rx, base, index);
    int prefixlen = 1;
    uint64_t instr = prefix | (opcode << (8 * prefixlen)) | (ext << (8 * (prefixlen + opcodelen)));
    int instrlen = prefixlen + opcodelen + extlen;
    emit(instr, instrlen);
}

//    op    reg_rm  rm_reg  rm_imm8  rm_imm8x  rm_imm32  rm_imm32x
#define BINARY_OPS(_) \
    _(ADD,  0x03,   0x01,   0x83,    0x00,     0x81,     0x00) \
    _(AND,  0x23,   0x21,   0x83,    0x04,     0x81,     0x04)

#define REG_RM(op1, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    if (op == op1) { opcode = reg_rm; opcodelen = 1; }

#define RM_REG(op1, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    if (op == op1) { opcode = rm_reg; opcodelen = 1; }

#define RM_IMM8(op1, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    if (op == op1) { opcode = rm_imm8; opcodelen = 1; rx = rm_imm8x; }

#define RM_IMM32(op1, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    if (op == op1) { opcode = rm_imm32; opcodelen = 1; rx = rm_imm32x; }

#define ENUM(x, ...) x,

enum {
    BINARY_OPS(ENUM)
};

INLINE void asm_rx_mem(uint64_t opcode, int opcodelen, uint64_t rx, uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    uint64_t addr;
    int addrlen;
    if (index == -1) {
        index = 0;
        if (disp || (base & 7) == RBP) {
            if (disp + 128 < 256) {
                addr = indirect_disp8(rx, base, disp);
                addrlen = 2;
            } else {
                addr = indirect_disp32(rx, base, disp);
                addrlen = 5;
            }
        } else {
            addr = indirect(rx, base);
            addrlen = 1;
        }
    } else {
        if (disp || (base & 7) == RBP) {
            if (disp + 128 < 256) {
                addr = indirect_index_disp8(rx, base, index, scale, disp);
                addrlen = 3;
            } else {
                addr = indirect_index_disp32(rx, base, index, scale, disp);
                addrlen = 6;
            }
        } else {
            addr = indirect_index(rx, base, index, scale);
            addrlen = 2;
        }
    }
    emit_instr(rx, base, index, opcode, opcodelen, addr, addrlen);
}

INLINE void asm_reg_reg(uint64_t op, uint64_t dest_reg, uint64_t src_reg) {
    uint64_t opcode;
    int opcodelen;
    BINARY_OPS(REG_RM);
    emit_instr(dest_reg, src_reg, 0, opcode, opcodelen, direct(dest_reg, src_reg), 1);
}

INLINE void asm_reg_mem(uint64_t op, uint64_t dest_reg, uint64_t src_base, uint64_t src_index, uint64_t src_scale, uint64_t src_disp) {
    uint64_t opcode;
    int opcodelen;
    BINARY_OPS(REG_RM);
    asm_rx_mem(opcode, opcodelen, dest_reg, src_base, src_index, src_scale, src_disp);
}

INLINE void asm_mem_reg(uint64_t op, uint64_t dest_base, uint64_t dest_index, uint64_t dest_scale, uint64_t dest_disp, uint64_t src_reg) {
    uint64_t opcode;
    int opcodelen;
    BINARY_OPS(RM_REG);
    asm_rx_mem(opcode, opcodelen, src_reg, dest_base, dest_index, dest_scale, dest_disp);
}

INLINE void asm_reg_imm(uint64_t op, uint64_t dest_reg, uint64_t src_imm) {
    uint64_t opcode, rx;
    int opcodelen, immlen;
    if (src_imm + 128 < 256) {
        BINARY_OPS(RM_IMM8);
        immlen = 1;
    } else {
        BINARY_OPS(RM_IMM32);
        immlen = 4;
    }
    emit_instr(rx, dest_reg, 0, opcode, opcodelen, direct(rx, dest_reg) | (src_imm << 8), 1 + immlen);
}

INLINE void asm_mem_imm(uint64_t op, uint64_t dest_base, uint64_t dest_index, uint64_t dest_scale, uint64_t dest_disp, uint64_t src_imm) {
    uint64_t opcode, rx;
    int opcodelen;
    if (src_imm + 128 < 256) {
        BINARY_OPS(RM_IMM8);
        asm_rx_mem(opcode, opcodelen, rx, dest_base, dest_index, dest_scale, dest_disp);
        emit(src_imm, 1);
    } else {
        BINARY_OPS(RM_IMM32);
        asm_rx_mem(opcode, opcodelen, rx, dest_base, dest_index, dest_scale, dest_disp);
        emit(src_imm, 4);
    }
}

INLINE uint32_t *asm_jump(const char *target) {
    uint32_t offset = (uint32_t)(target - (here + 2));
    if (offset + 128 < 256) {
        emit(0xEB | (offset << 8), 2);
    } else {
        emit(0xE9 | ((offset - 3) << 8), 5);
    }
    return target ? 0 : (uint32_t *)(here - 4);
}

INLINE uint32_t *asm_jump_if(uint64_t cond, const char *target) {
    assert(cond < 16);
    uint32_t offset = (uint32_t)(target - (here + 2));
    if (offset + 128 < 256) {
        emit(0x70 | cond | (offset << 8), 2);
    } else {
        emit(0x800F | (cond << 8) | ((offset  - 4) << 16), 6);
    }
    return target ? 0 : (uint32_t *)(here - 4);
}

INLINE void asm_patch_jump(uint32_t *jump_field, const char *target) {
    *jump_field = (uint32_t)(target - ((char *)jump_field + 4));
}
