// x64 encoding

#include <inttypes.h>
#include <assert.h>
#define INLINE inline

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

INLINE int isimm8(uint64_t imm) {
    return imm + 128 < 256;
}

INLINE int isdisp8(uint64_t disp) {
    return disp + 128 < 256;
}

INLINE int isrel8(uint32_t rel) {
    return rel + 128 < 256;
}

uint8_t *here;

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
    bool isripdisp{false};
    bool isindex{false};
    uint64_t base{0};
    uint64_t index{0};
    uint64_t scale{0};
    uint64_t disp{0};
} Mem;

INLINE void emit_op_rx_mem(uint64_t op, int oplen, uint64_t rx, Mem mem) {
    uint64_t addr;
    int addrlen;
    if (mem.isripdisp) {
        addr = indirect_rip_disp32(rx, mem.disp);
        addrlen = 5;
    } else if (mem.isindex) {
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
    } else {
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
    }
    emit_instr(op, oplen, rx, mem.base, mem.index, addr, addrlen);
}

INLINE void emit_op_reg_reg(uint64_t op, int oplen, uint64_t dest_reg, uint64_t src_reg) {
    emit_instr(op, oplen, dest_reg, src_reg, 0, direct(dest_reg, src_reg), 1);
}

INLINE void emit_op_reg_mem(uint64_t op, int oplen, uint64_t dest_reg, Mem src_mem) {
    emit_op_rx_mem(op, oplen, dest_reg, src_mem);
}

INLINE void emit_op_mem_reg(uint64_t op, int oplen, Mem dest_mem, uint64_t src_reg) {
    emit_op_rx_mem(op, oplen, src_reg, dest_mem);
}

INLINE void emit_op_reg(uint64_t op, int oplen, uint64_t rx, uint64_t reg) {
    emit_op_reg_reg(op, oplen, rx, reg);
}

INLINE void emit_op_mem(uint64_t op, int oplen, uint64_t rx, Mem mem) {
    emit_op_rx_mem(op, oplen, rx, mem);
}

INLINE void emit_sse_op_reg_reg(uint64_t op, int oplen, uint64_t prefix, uint64_t dest_reg, uint64_t src_reg) {
    emit(prefix, 1);
    emit_op_reg_reg(op, oplen, dest_reg, src_reg);
}

INLINE void emit_sse_op_reg_mem(uint64_t op, int oplen, uint64_t prefix, uint64_t dest_reg, Mem src_mem) {
    emit(prefix, 1);
    emit_op_rx_mem(op, oplen, dest_reg, src_mem);
}

INLINE void emit_sse_op_mem_reg(uint64_t op, int oplen, uint64_t prefix, Mem dest_mem, uint64_t src_reg) {
    emit(prefix, 1);
    emit_op_rx_mem(op, oplen, src_reg, dest_mem);
}

INLINE void emit_op_reg_imm(uint64_t op8, uint64_t op32, int oplen, uint64_t rx8, uint64_t rx32, uint64_t dest_reg, uint64_t src_imm) {
    uint64_t op, rx;
    int immlen;
    if ((isimm8(src_imm) && op8) || !op32) {
        assert(op8);
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

INLINE void emit_op_mem_imm(uint64_t op8, uint64_t op32, int oplen, uint64_t rx8, uint64_t rx32, Mem dest_mem, uint64_t src_imm) {
    if ((isimm8(src_imm) && op8) || !op32) {
        assert(op8);
        emit_op_rx_mem(op8, oplen, rx8, dest_mem);
        emit(src_imm, 1);
    } else {
        emit_op_rx_mem(op32, oplen, rx32, dest_mem);
        emit(src_imm, 4);
    }
}

// x64 instruction emitters

#define X64_OP_REG(name, op, oplen, rx) \
    INLINE void name##_reg(uint64_t reg) { \
        emit_op_reg(op, oplen, rx, reg); \
    }

#define X64_OP_MEM(name, op, oplen, rx) \
    INLINE void name##_mem(Mem mem) { \
        emit_op_mem(op, oplen, rx, mem); \
    }

#define X64_OP_REG_REG(name, op, oplen) \
    INLINE void name##_reg_reg(uint64_t dest_reg, uint64_t src_reg) { \
        emit_op_reg_reg(op, oplen, dest_reg, src_reg); \
    }

#define X64_OP_REG_MEM(name, op, oplen) \
    INLINE void name##_reg_mem(uint64_t dest_reg, Mem src_mem) { \
        emit_op_reg_mem(op, oplen, dest_reg, src_mem); \
    }

#define X64_OP_MEM_REG(name, op, oplen) \
    INLINE void name##_mem_reg(Mem dest_mem, uint64_t src_reg) { \
        emit_op_mem_reg(op, oplen, dest_mem, src_reg); \
    }

#define X64_OP_REG_IMM(name, op8, op32, oplen, rx8, rx32) \
    INLINE void name##_reg_imm(uint64_t dest_reg, uint64_t src_imm) { \
        emit_op_reg_imm(op8, op32, oplen, rx8, rx32, dest_reg, src_imm); \
    }

#define X64_OP_MEM_IMM(name, op8, op32, oplen, rx8, rx32) \
    INLINE void name##_mem_imm(Mem dest_mem, uint64_t src_imm) { \
        emit_op_mem_imm(op8, op32, oplen, rx8, rx32, dest_mem, src_imm); \
    }

#define SSE_OP_REG_REG(name, op, oplen, prefix) \
    INLINE void name##_reg_reg(uint64_t dest_reg, uint64_t src_reg) { \
        emit_sse_op_reg_reg(op, oplen, prefix, dest_reg, src_reg); \
    }

#define SSE_OP_MEM_REG(name, op, oplen, prefix) \
    INLINE void name##_mem_reg(Mem dest_mem, uint64_t src_reg) { \
        emit_sse_op_mem_reg(op, oplen, prefix, dest_mem, src_reg); \
    }

#define SSE_OP_REG_MEM(name, op, oplen, prefix) \
    INLINE void name##_reg_mem(uint64_t dest_reg, Mem src_mem) { \
        emit_sse_op_reg_mem(op, oplen, prefix, dest_reg, src_mem); \
    }

#define X64_OP_RM(name, op, oplen, rx) \
    X64_OP_REG(name, op, oplen, rx) \
    X64_OP_MEM(name, op, oplen, rx)

#define X64_OP_REG_RM(name, op, oplen) \
    X64_OP_REG_REG(name, op, oplen) \
    X64_OP_REG_MEM(name, op, oplen)

#define X64_OP_RM_IMM(name, op8, op32, oplen, rx8, rx32) \
    X64_OP_REG_IMM(name, op8, op32, oplen, rx8, rx32) \
    X64_OP_MEM_IMM(name, op8, op32, oplen, rx8, rx32)

#define SSE_OP_REG_RM(name, op, oplen, prefix) \
    SSE_OP_REG_REG(name, op, oplen, prefix) \
    SSE_OP_REG_MEM(name, op, oplen, prefix)

// x64 instruction definitions

#define X64_UNARY_TABLE(_) \
    _(neg,    0xF7,    0x03) \
    _(idiv,   0xF7,    0x07) \
//  _(name,   rm,      rx)

#define X64_BINARY_TABLE(_) \
    _(add,    0x03,    0x01,   0x83,    0x00,     0x81,     0x00) \
    _(sub,    0x2B,    0x29,   0x83,    0x05,     0x81,     0x05) \
    _(cmp,    0x3B,    0x39,   0x83,    0x07,     0x81,     0x07) \
    _(and,    0x23,    0x21,   0x83,    0x04,     0x81,     0x04) \
    _(mov,    0x8B,    0x89,   0x00,    0x00,     0xC7,     0x00) \
//  _(name,   reg_rm,  rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x)

#define SSE_BINARY_TABLE(_) \
    _(mulss,  0x580F,  0xF3) \
    _(andss,  0x590F,  0xF3) \
    _(movss,  0x100F,  0xF3) \
//  _(name,   reg_rm,  prefix)

#define X64_UNARY_OPS(name, rm, rx) \
    X64_OP_RM(name, rm, 1, rx)

#define X64_BINARY_OPS(name, reg_rm, rm_reg, rm_imm8, rm_imm8x, rm_imm32, rm_imm32x) \
    X64_OP_REG_RM(name, reg_rm, 1) \
    X64_OP_MEM_REG(name, rm_reg, 1) \
    X64_OP_RM_IMM(name, rm_imm8, rm_imm32, 1, rm_imm8x, rm_imm32x)

#define SSE_BINARY_OPS(name, reg_rm, prefix) \
    SSE_OP_REG_RM(name, reg_rm, 2, prefix)

X64_UNARY_TABLE(X64_UNARY_OPS)
X64_BINARY_TABLE(X64_BINARY_OPS)
SSE_BINARY_TABLE(SSE_BINARY_OPS)
SSE_OP_MEM_REG(movss, 0x110F, 2, 0xF3)
X64_OP_REG_REG(imul, 0xAF0F, 2)
X64_OP_REG_IMM(imul, 0x6B, 0x69, 1, 0, 0)
X64_OP_RM(shl, 0xD3, 1, 0x04)
X64_OP_REG_IMM(shl, 0xC1, 0, 1, 0x04, 0)
X64_OP_MEM_REG(mov8, 0x88, 1);
X64_OP_MEM_REG(mov32, 0x89, 1);
X64_OP_REG_RM(movsx8, 0xBE0F, 2);
X64_OP_REG_RM(movsx16, 0xBF0F, 2);
X64_OP_REG_RM(movsx32, 0xBF0F, 2);
X64_OP_REG_RM(movzx8, 0xB60F, 2);
X64_OP_REG_RM(movzx16, 0xB70F, 2);

INLINE void mov16_mem_reg(Mem dest_mem, uint64_t src_reg) {
    emit(0x66, 1);
    uint8_t *start = here;
    emit_op_mem_reg(0x89, 1, dest_mem, src_reg);
    *start &= ~8;
}

INLINE void movzx32_reg_reg(uint64_t dest_reg, uint64_t src_reg) {
    uint8_t *start = here;
    emit_op_reg_reg(0x8B, 1, dest_reg, src_reg);
    *start &= ~8;
}

INLINE void movzx32_reg_mem(uint64_t dest_reg, Mem src_mem) {
    uint8_t *start = here;
    emit_op_rx_mem(0x8B, 1, dest_reg, src_mem);
    *start &= ~8;
}

INLINE void cmov_reg_reg_if(uint64_t cond, uint64_t dest_reg, uint64_t src_reg) {
    emit_op_reg_reg(0x400F | (cond << 8), 2, dest_reg, src_reg);
}

INLINE void set8_reg_if(uint64_t cond, uint64_t dest_reg) {
    assert(cond < 16);
    emit_op_reg(0x900F | (cond << 8), 2, 0, dest_reg);
}

INLINE uint32_t *jmp(const uint8_t *target) {
    uint32_t rel = (uint32_t)(target - (here + 2));
    if (isrel8(rel)) {
        emit(0xEB | (rel << 8), 2);
        return 0;
    } else {
        emit(0xE9 | ((rel - 3) << 8), 5);
        return (uint32_t *)(here - 4);
    }
}

INLINE uint32_t *jmp_if(uint64_t cond, const uint8_t*target) {
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

INLINE void ret() {
    emit(0xc3, 1);
}

INLINE void push_reg(uint64_t src_reg) {
    emit(0x50 | src_reg, 1);
}

INLINE void push_imm(uint64_t imm) {
    emit(0x68, 1);
    emit(imm, 4);
}

INLINE void pop_reg(uint64_t dst_reg) {
    emit(0x58 | dst_reg, 1);
}

INLINE void patch_rel(uint32_t *rel_ptr, const uint8_t *target) {
    *rel_ptr = (uint32_t)(target - ((uint8_t *)rel_ptr + 4));
}

// addressing helpers

INLINE Mem base(uint64_t base) {
    return (Mem){.base = base};
}

INLINE Mem base_disp(uint64_t base, uint64_t disp) {
    return (Mem){.base = base, .disp = disp};
}

INLINE Mem base_index(uint64_t base, uint64_t index) {
    return (Mem){.isindex = 1, .base = base, .index = index};
}

INLINE Mem base_index_disp(uint64_t base, uint64_t index, uint64_t disp) {
    return (Mem){.isindex = 1, .base = base, .index = index, .disp = disp};
}

INLINE Mem base_index_scale(uint64_t base, uint64_t index, uint64_t scale) {
    return (Mem){.isindex = 1, .base = base, .index = index, .scale = scale};
}

INLINE Mem base_index_scale_disp(uint64_t base, uint64_t index, uint64_t scale, uint64_t disp) {
    return (Mem){.isindex = 1, .base = base, .index = index, .scale = scale, .disp = disp};
}

INLINE Mem rip_disp(uint64_t disp) {
    return (Mem){.isripdisp = 1, .disp = disp};
}

void example(void) {
    mov_reg_reg(RAX, R9);
    mov8_mem_reg(base(RAX), R9);
    mov16_mem_reg(base(RAX), R9);
    mov32_mem_reg(base(RAX), R9);
    movss_mem_reg(base(RAX), XMM10);
    add_reg_mem(RAX, rip_disp(0x1234));
    neg_reg(R9);
    idiv_reg(RAX);
    imul_reg_reg(RDX, R9);
    mulss_reg_reg(XMM9, XMM10);
    mov_reg_mem(R9, base(R10));
    mov_reg_imm(RAX, 0x12345678);
    mov_reg_imm(RAX, -128);
    mov_mem_imm(base(RAX), 0x12345678);
    mulss_reg_reg(XMM0, XMM9);
    mulss_reg_reg(XMM0, XMM9);
    mulss_reg_mem(XMM3, base_index_scale(RBX, RCX, X8));
    shl_reg(RAX);
    shl_reg_imm(R9, 0xA);
    movzx8_reg_reg(RAX, R9);
    movzx16_reg_reg(RAX, R9);
    movzx32_reg_reg(RAX, R9);
    movzx8_reg_mem(RAX, base_index_scale(RBX, RCX, X8));
    movzx16_reg_mem(RAX, base_index_scale(RBX, RCX, X8));
    movzx32_reg_mem(RAX, base_index_scale(RBX, RCX, X8));
    const uint8_t *start = here;
    uint32_t *rel_ptr = jmp(0);
    set8_reg_if(NE, R9);
    cmov_reg_reg_if(LE, RAX, R9);
    jmp_if(E, start);
    patch_rel(rel_ptr, here);
    and_reg_reg(RAX, R9);
}
