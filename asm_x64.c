// x64 encoding

enum Reg {
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8,  R9,  R10, R11, R12, R13, R14, R15,
};

enum XmmReg {
    XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
};

enum Scale {
    X1, X2, X4, X8
};

enum Cond {
    O, NO, B, NB, E, NE, NA, A,
    S, NS, P, NP, L, NL, NG, G,

    LE = NG, GE = NL, BE = NA, AE = NB,
};

enum Mode {
    INDIRECT, INDIRECT_DISP8, INDIRECT_DISP32, DIRECT
};

INLINE uint64_t rexw(uint64_t rx, uint64_t base, uint64_t index) {
    assert(rx < 16);
    assert(base < 16);
    assert(index < 16);
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

INLINE int isimm8(uint64_t imm) {
    return imm + 128 < 256;
}

INLINE int isdisp8(uint64_t disp) {
    return disp + 128 < 256;
}

INLINE int isrel8(uint32_t rel) {
    return rel + 128 < 256;
}

char *here;

INLINE void emit(uint64_t data, int len) {
    assert(len <= 8);
    *(uint64_t *)here = data;
    here += len;
}

INLINE void emit3(uint64_t data1, int len1, uint64_t data2, int len2, uint64_t data3, int len3) {
    emit(data1 | (data2 << (8 * len1)) | (data3 << (8 * (len1 + len2))), len1 + len2 + len3);
}

INLINE void emit_instr(int64_t op, int oplen, uint64_t rx, uint64_t base, uint64_t index, uint64_t ext, int extlen) {
    emit3(rexw(rx, base, index), 1, op, oplen, ext, extlen);
}

typedef struct {
    uint64_t base;
    uint64_t index;
    uint64_t scale;
    uint64_t disp;
} Mem;

INLINE void x64_rx_mem(uint64_t op, int oplen, uint64_t rx, Mem mem) {
    uint64_t addr;
    int addrlen;
    if (mem.index == -1) {
        mem.index = 0;
        if (mem.disp || (mem.base & 7) == RBP) {
            if (isdisp8(mem.disp)) {
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
        if (mem.disp || (mem.base & 7) == RBP) {
            if (isdisp8(mem.disp)) {
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
    emit_instr(op, oplen, rx, mem.base, mem.index, addr, addrlen);
}

INLINE void x64_op_reg_reg(uint64_t op, int oplen, uint64_t dest_reg, uint64_t src_reg) {
    emit_instr(op, oplen, dest_reg, src_reg, 0, direct(dest_reg, src_reg), 1);
}

INLINE void x64_op_reg_mem(uint64_t op, int oplen, uint64_t dest_reg, Mem src_mem) {
    x64_rx_mem(op, oplen, dest_reg, src_mem);
}

INLINE void x64_op_mem_reg(uint64_t op, int oplen, Mem dest_mem, uint64_t src_reg) {
    x64_rx_mem(op, oplen, src_reg, dest_mem);
}

INLINE void x64_op_reg(uint64_t op, int oplen, uint64_t rx, uint64_t reg) {
    x64_op_reg_reg(op, oplen, rx, reg);
}

INLINE void sse_op_reg_reg(uint64_t op, int oplen, uint64_t prefix, uint64_t dest_reg, uint64_t src_reg) {
    emit(prefix, 1);
    x64_op_reg_reg(op, oplen, dest_reg, src_reg);
}

INLINE void sse_op_reg_mem(uint64_t op, int oplen, uint64_t prefix, uint64_t dest_reg, Mem src_mem) {
    emit(prefix, 1);
    x64_rx_mem(op, oplen, dest_reg, src_mem);
}

INLINE void x64_op_reg_imm(uint64_t op8, uint64_t op32, int oplen, uint64_t rx8, uint64_t rx32, uint64_t dest_reg, uint64_t src_imm) {
    uint64_t op, rx;
    int immlen;
    if ((isimm8(src_imm) && op8) || !op32) {
        op = op8;
        rx = rx8;
        immlen = 1;
    } else {
        op = op32;
        rx = rx32;
        immlen = 4;
    }
    emit_instr(op, oplen, rx, dest_reg, 0, direct(rx, dest_reg) | (src_imm << 8), 1 + immlen);
}

INLINE void x64_op_mem_imm(uint64_t op8, uint64_t op32, int oplen, uint64_t rx8, uint64_t rx32, Mem dest_mem, uint64_t src_imm) {
    if (op8 && isimm8(src_imm)) {
        x64_rx_mem(op8, oplen, rx8, dest_mem);
        emit(src_imm, 1);
    } else {
        x64_rx_mem(op32, oplen, rx32, dest_mem);
        emit(src_imm, 4);
    }
}

#define X64_OP_REG_REG(name, op, oplen) \
    void name##_reg_reg(uint64_t dest_reg, uint64_t src_reg) { \
        x64_op_reg_reg(op, oplen, dest_reg, src_reg); \
    }

#define X64_OP_REG_MEM(name, op, oplen) \
    void name##_reg_mem(uint64_t dest_reg, Mem src_mem) { \
        x64_op_reg_mem(op, oplen, dest_reg, src_mem); \
    }

#define X64_OP_REG_IMM(name, op8, op32, oplen, rx8, rx32) \
    void name##_reg_imm(uint64_t dest_reg, uint64_t src_imm) { \
        x64_op_reg_imm(op8, op32, oplen, rx8, rx32, dest_reg, src_imm); \
    }

#define X64_OP_MEM_REG(name, op, oplen) \
    void name##_mem_reg(Mem dest_mem, uint64_t src_reg) { \
        x64_op_mem_reg(op, oplen, dest_mem, src_reg); \
    }

#define X64_OP_MEM_IMM(name, op8, op32, oplen, rx8, rx32) \
    void name##_mem_imm(Mem dest_mem, uint64_t src_imm) { \
        x64_op_mem_imm(op8, op32, oplen, rx8, rx32, dest_mem, src_imm); \
    }

#define X64_OP_REG(name, op, oplen, rx) \
    void name##_reg(uint64_t reg) { \
        x64_op_reg(op, oplen, rx, reg); \
    }

#define SSE_OP_REG_REG(name, op, oplen, prefix) \
    void name##_reg_reg(uint64_t dest_reg, uint64_t src_reg) { \
        sse_op_reg_reg(op, oplen, prefix, dest_reg, src_reg); \
    }

#define SSE_OP_REG_MEM(name, op, oplen, prefix) \
    void name##_reg_mem(uint64_t dest_reg, Mem src_mem) { \
        sse_op_reg_mem(op, oplen, prefix, dest_reg, src_mem); \
    }

// x64 instructions

//    op      reg_rm  rm_reg  rm_imm8  rm_imm8x  rm_imm32  rm_imm32x
#define X64_BINARY_OPS(_) \
    _(add,    0x03,   0x01,   0x83,    0x00,     0x81,     0x00) \
    _(and,    0x23,   0x21,   0x83,    0x04,     0x81,     0x04) \
    _(mov,    0x8B,   0x89,   0x00,    0x00,     0xC7,     0x00)

//    op    rm     len
#define X64_UNARY_OPS(_) \
    _(neg,  0xF7,  0x03) \
    _(idiv, 0xF7,  0x07)

//    op    reg_rm   prefix
#define SSE_BINARY_OPS(_) \
    _(mulss, 0x580F, 0xF3) \
    _(andss, 0x590F, 0xF3)

#define X64_BINARY_FUNCS(op, rm_reg, reg_rm, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    X64_OP_REG_REG(op, reg_rm, 1) \
    X64_OP_REG_MEM(op, reg_rm, 1) \
    X64_OP_REG_IMM(op, rm_imm8, rm_imm32, 1, rm_imm8x, rm_imm32x) \
    X64_OP_MEM_REG(op, rm_reg, 1) \
    X64_OP_MEM_IMM(op, rm_imm8, rm_imm32, 1, rm_imm8x, rm_imm32x)

#define X64_UNARY_FUNCS(op, rm, rx) \
    X64_OP_REG(op, rm, 1, rx)

#define SSE_BINARY_FUNCS(op, reg_rm, prefix) \
    SSE_OP_REG_REG(op, reg_rm, 2, prefix) \
    SSE_OP_REG_MEM(op, reg_rm, 2, prefix)

X64_BINARY_OPS(X64_BINARY_FUNCS)
X64_UNARY_OPS(X64_UNARY_FUNCS)
SSE_BINARY_OPS(SSE_BINARY_FUNCS)

X64_OP_REG_REG(imul, 0xAF0F, 2)
X64_OP_REG_IMM(imul, 0x6B, 0x69, 1, 0, 0)
X64_OP_REG(shl, 0xD3, 1, 0x04)
X64_OP_REG_IMM(shl, 0xC1, 0, 1, 0x04, 0)

INLINE void movzx_reg_reg(uint64_t dest_reg, uint64_t src_reg, int src_size) {
    if (src_size == 1) {
        x64_op_reg_reg(0xB60F, 2, dest_reg, src_reg);
    } else if (src_size == 2) {
        x64_op_reg_reg(0xB70F, 2, dest_reg, src_reg);
    } else {
        assert(src_size == 4);
        uint8_t *start = here;
        x64_op_reg_reg(0x8B, 1, dest_reg, src_reg);
        *start &= ~8;
    }
}
INLINE void movzx_reg_mem(uint64_t dest_reg, Mem src_mem, int src_size) {
    if (src_size == 1) {
        x64_rx_mem(0xB60F, 2, dest_reg, src_mem);
    } else if (src_size == 2) {
        x64_rx_mem(0xB70F, 2, dest_reg, src_mem);
    } else {
        assert(src_size == 4);
        uint8_t *start = here;
        x64_rx_mem(0x8B, 1, dest_reg, src_mem);
        *start &= ~8;
    }
}

INLINE void movsx_reg_reg(uint64_t dest_reg, uint64_t src_reg, int src_size) {
    if (src_size == 1) {
        x64_op_reg_reg(0xBE0F, 2, dest_reg, src_reg);
    } else if (src_size == 2) {
        x64_op_reg_reg(0xBF0F, 2, dest_reg, src_reg);
    } else {
        assert(src_size == 4);
        x64_op_reg_reg(0x63, 1, dest_reg, src_reg);
    }
}

INLINE void movsx_reg_mem(uint64_t dest_reg, Mem src_mem, int src_size) {
    if (src_size == 1) {
        x64_rx_mem(0xBE0F, 2, dest_reg, src_mem);
    } else if (src_size == 2) {
        x64_rx_mem(0xBF0F, 2, dest_reg, src_mem);
    } else {
        assert(src_size == 4);
        x64_rx_mem(0x63, 1, dest_reg, src_mem);
    }
}

INLINE uint32_t *jmp(const char *target) {
    uint32_t rel = (uint32_t)(target - (here + 2));
    if (isrel8(rel)) {
        emit(0xEB | (rel << 8), 2);
        return 0;
    } else {
        emit(0xE9 | ((rel - 3) << 8), 5);
        return (uint32_t *)(here - 4);
    }
}

INLINE uint32_t *jmp_if(uint64_t cond, const char *target) {
    assert(cond < 16);
    uint32_t rel = (uint32_t)(target - (here + 2));
    if (isrel8(rel)) {
        emit(0x70 | cond | (rel << 8), 2);
        return 0;
    } else {
        emit(0x800F | (cond << 8) | ((rel - 4) << 16), 6);
        return (uint32_t *)(here - 4);
    }
}

INLINE void patch_jmp(uint32_t *jmp_rel, const char *target) {
    *jmp_rel = (uint32_t)(target - ((char *)jmp_rel + 4));
}
