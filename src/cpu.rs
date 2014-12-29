#![feature(macro_rules)]
#![macro_escape]
use mem::MemMapper;
use disasm::Disasm;

// Initial values for cpu registers
const PC_START: u16 = 0x8000;
const SP_START: u8 = 0xfd;
const P_START: u8 = 0x24;

// Flags used on the p register of the cpu. The bit at
// the 5th position is always set.
const CARRY_FLAG: u8 = 1 << 0;
const ZERO_FLAG: u8 = 1 << 1;
const INTERRUPT_FLAG: u8 = 1 << 2;
const DECIMAL_FLAG: u8 = 1 << 3;
const BREAK_FLAG: u8 = 1 << 4;
const OVERFLOW_FLAG: u8 = 1 << 6;
const NEGATIVE_FLAG: u8 = 1 << 7;

pub enum Mode{
    ACCUMULATOR,
    IMMEDIATE,
    ZERO_PAGE,
    ZERO_PAGE_X,
    ZERO_PAGE_Y,
    ABSOLUTE,
    ABSOLUTE_X,
    ABSOLUTE_Y,
    INDIRECT,
    INDIRECT_X,
    INDIRECT_Y,
}

pub struct Regs {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub p: u8,
}

impl Regs {
    pub fn new() -> Regs {
        Regs{
            a: 0,
            x: 0,
            y: 0,
            pc: PC_START,
            sp: SP_START,
            p: P_START,
        }
    }

    pub fn is_set(&self, flag: u8) -> bool {
        self.p & flag == flag
    }
}

pub struct Cpu {
    regs: Regs,
    mem: MemMapper,
}

// Mapping from byte codes to operations are from here:
// http://nesdev.com/6502.txt
//
// The idea of using a macro to be used by the cpu and disassembler
// was taken from Patrick Walton's sprocketnes emulator

#[macro_export]
macro_rules! decode_op {
    ($op:expr, $this:ident) => {
        match $op {
            // ADC
            0x69 => { let v = $this.get_value(Mode::IMMEDIATE); $this.adc(v) }
            0x65 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.adc(v) }
            0x75 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.adc(v) }
            0x6d => { let v = $this.get_value(Mode::ABSOLUTE); $this.adc(v) }
            0x7d => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.adc(v) }
            0x79 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.adc(v) }
            0x61 => { let v = $this.get_value(Mode::INDIRECT_X); $this.adc(v) }
            0x71 => { let v = $this.get_value(Mode::INDIRECT_Y); $this.adc(v) }

            // AND
            0x29 => { let v = $this.get_value(Mode::IMMEDIATE); $this.and(v) }
            0x25 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.and(v) }
            0x35 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.and(v) }
            0x2d => { let v = $this.get_value(Mode::ABSOLUTE); $this.and(v) }
            0x3d => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.and(v) }
            0x39 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.and(v) }
            0x21 => { let v = $this.get_value(Mode::INDIRECT_X); $this.and(v) }
            0x31 => { let v = $this.get_value(Mode::INDIRECT_Y); $this.and(v) }

            // ASL
            0x0a => {
                let v = $this.get_value(Mode::ACCUMULATOR);
                $this.asl(v, Mode::ACCUMULATOR)
            }

            0x06 => {
                let v = $this.get_value(Mode::ZERO_PAGE);
                $this.asl(v, Mode::ZERO_PAGE)
            }

            0x16 => {
                let v = $this.get_value(Mode::ZERO_PAGE_X);
                $this.asl(v, Mode::ZERO_PAGE_X)
            }

            0x0e => {
                let v = $this.get_value(Mode::ABSOLUTE);
                $this.asl(v, Mode::ABSOLUTE)
            }

            0x1e => {
                let v = $this.get_value(Mode::ABSOLUTE_X);
                $this.asl(v, Mode::ABSOLUTE_X)
            }

            // BCC
            0x90 => { $this.bcc() }

            // BCS
            0xb0 => { $this.bcs() }

            // BEQ
            0xf0 => { $this.beq() }

            // BIT
            0x24 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.bit(v) }
            0x2c => { let v = $this.get_value(Mode::ABSOLUTE); $this.bit(v) }

            // BMI
            0x30 => { $this.bmi() }

            // BNE
            0xd0 => { $this.bne() }

            // BPL
            0x10 => { $this.bpl() }

            // BRK
            0x00 => { $this.brk() }

            // BVC
            0x50 => { $this.bvc() }

            // BVS
            0x70 => { $this.bvs() }

            // CLC
            0x18 => { $this.clc() }

            // CLD
            0xd8 => { $this.cld() }

            // CLI
            0x58 => { $this.cli() }

            // CLV
            0xb8 => { $this.clv() }

            // CMP
            0xc9 => { let v = $this.get_value(Mode::IMMEDIATE); $this.cmp(v) }
            0xc5 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.cmp(v) }
            0xd5 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.cmp(v) }
            0xcd => { let v = $this.get_value(Mode::ABSOLUTE); $this.cmp(v) }
            0xdd => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.cmp(v) }
            0xd9 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.cmp(v) }
            0xc1 => { let v = $this.get_value(Mode::INDIRECT_X); $this.cmp(v) }
            0xd1 => { let v = $this.get_value(Mode::INDIRECT_Y); $this.cmp(v) }

            // CPX
            0xe0 => { let v = $this.get_value(Mode::IMMEDIATE); $this.cpx(v) }
            0xe4 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.cpx(v) }
            0xec => { let v = $this.get_value(Mode::ABSOLUTE); $this.cpx(v) }

            // CPY
            0xc0 => { let v = $this.get_value(Mode::IMMEDIATE); $this.cpy(v) }
            0xc4 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.cpy(v) }
            0xcc => { let v = $this.get_value(Mode::ABSOLUTE); $this.cpy(v) }

            // DEC
            0xc6 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.dec(v) }
            0xd6 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.dec(v) }
            0xce => { let v = $this.get_value(Mode::ABSOLUTE); $this.dec(v) }
            0xde => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.dec(v) }

            // DEX
            0xca => { $this.dex() }

            // DEY
            0x88 => { $this.dey() }

            // EOR
            0x49 => { let v = $this.get_value(Mode::IMMEDIATE); $this.eor(v) }
            0x45 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.eor(v) }
            0x55 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.eor(v) }
            0x40 => { let v = $this.get_value(Mode::ABSOLUTE); $this.eor(v) }
            0x5d => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.eor(v) }
            0x59 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.eor(v) }
            0x41 => { let v = $this.get_value(Mode::INDIRECT_X); $this.eor(v) }
            0x51 => { let v = $this.get_value(Mode::INDIRECT_Y); $this.eor(v) }

            // INC
            0xe6 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.inc(v) }
            0xf6 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.inc(v) }
            0xee => { let v = $this.get_value(Mode::ABSOLUTE); $this.inc(v) }
            0xfe => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.inc(v) }

            // INX
            0xe8 => { $this.inx() }

            // INY
            0xc8 => { $this.iny() }

            // JMP
            0x4c => { let v = $this.get_value(Mode::ABSOLUTE); $this.jmp(v) }
            0x6c => { let v = $this.get_value(Mode::INDIRECT); $this.jmp(v) }

            // JSR
            0x20 => { let v = $this.get_value(Mode::ABSOLUTE); $this.jsr(v) }

            // Loads

            // LDA
            0xa9 => { let v = $this.get_value(Mode::IMMEDIATE); $this.lda(v) }
            0xa5 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.lda(v) }
            0xb5 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.lda(v) }
            0xad => { let v = $this.get_value(Mode::ABSOLUTE); $this.lda(v) }
            0xbd => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.lda(v) }
            0xb9 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.lda(v) }
            0xa1 => { let v = $this.get_value(Mode::INDIRECT_X); $this.lda(v) }
            0xb1 => { let v = $this.get_value(Mode:: INDIRECT_Y); $this.lda(v) }

            // LDX
            0xa2 => { let v = $this.get_value(Mode::IMMEDIATE); $this.ldx(v) }
            0xa6 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.ldx(v) }
            0xb6 => { let v = $this.get_value(Mode::ZERO_PAGE_Y); $this.ldx(v) }
            0xae => { let v = $this.get_value(Mode::ABSOLUTE); $this.ldx(v) }
            0xbe => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.ldx(v) }

            // LDY
            0xa0 => { let v = $this.get_value(Mode::IMMEDIATE); $this.ldy(v) }
            0xa4 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.ldy(v) }
            0xb4 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.ldy(v) }
            0xac => { let v = $this.get_value(Mode::ABSOLUTE); $this.ldy(v) }
            0xbc => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.ldy(v) }

            // LSR
            0x4a => {
                let v = $this.get_value(Mode::ACCUMULATOR);
                $this.lsr(v, Mode::ACCUMULATOR)}
            0x46 => {
                let v = $this.get_value(Mode::ZERO_PAGE);
                $this.lsr(v, Mode::ZERO_PAGE) }
            0x56 => {
                let v = $this.get_value(Mode::ZERO_PAGE_X);
                $this.lsr(v, Mode::ZERO_PAGE_X) }
            0x4e => {
                let v = $this.get_value(Mode::ABSOLUTE);
                $this.lsr(v, Mode::ABSOLUTE) }
            0x5e => {
                let v = $this.get_value(Mode::ABSOLUTE_X);
                $this.lsr(v, Mode::ABSOLUTE_X) }

            // NOP
            0xea => { $this.nop() }

            // ORA
            0x09 => { let v = $this.get_value(Mode::IMMEDIATE); $this.ora(v) }
            0x05 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.ora(v) }
            0x15 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.ora(v) }
            0x0d => { let v = $this.get_value(Mode::ABSOLUTE); $this.ora(v) }
            0x1d => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.ora(v) }
            0x19 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.ora(v) }
            0x01 => { let v = $this.get_value(Mode::INDIRECT_X); $this.ora(v) }
            0x11 => { let v = $this.get_value(Mode::INDIRECT_Y); $this.ora(v) }

            // PHA
            0x48 => { $this.pha() }

            // PHP
            0x08 => { $this.php() }

            // PLA
            0x68 => { $this.pla() }

            // PLP
            0x28 => { $this.plp() }

            // ROL
            0x2a => {
                let v = $this.get_value(Mode::ACCUMULATOR);
                $this.rol(v, Mode::ACCUMULATOR) }
            0x26 => {
                let v = $this.get_value(Mode::ZERO_PAGE);
                $this.rol(v, Mode::ZERO_PAGE) }
            0x36 => {
                let v = $this.get_value(Mode::ZERO_PAGE_X);
                $this.rol(v, Mode::ZERO_PAGE_X) }
            0x2e => {
                let v = $this.get_value(Mode::ABSOLUTE);
                $this.rol(v, Mode::ABSOLUTE) }
            0x3e => {
                let v = $this.get_value(Mode::ABSOLUTE_X);
                $this.rol(v, Mode::ABSOLUTE_X) }

            // ROR
            0x6a => {
                let v = $this.get_value(Mode::ACCUMULATOR);
                $this.ror(v, Mode::ACCUMULATOR) }
            0x66 => {
                let v = $this.get_value(Mode::ZERO_PAGE);
                $this.ror(v, Mode::ZERO_PAGE) }
            0x76 => {
                let v = $this.get_value(Mode::ZERO_PAGE_X);
                $this.ror(v, Mode::ZERO_PAGE_X) }
            0x6e => {
                let v = $this.get_value(Mode::ABSOLUTE);
                $this.ror(v, Mode::ABSOLUTE) }
            0x7e => {
                let v = $this.get_value(Mode::ABSOLUTE_X);
                $this.ror(v, Mode::ABSOLUTE_X) }

            // RTI
            0x4d => { $this.rti() }

            // RTS
            0x60 => { $this.rts() }

            // SBC
            0xe9 => { let v = $this.get_value(Mode::IMMEDIATE); $this.sbc(v) }
            0xe5 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.sbc(v) }
            0xf5 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.sbc(v) }
            0xed => { let v = $this.get_value(Mode::ABSOLUTE); $this.sbc(v) }
            0xfd => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.sbc(v) }
            0xf9 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.sbc(v) }
            0xe1 => { let v = $this.get_value(Mode::INDIRECT_X); $this.sbc(v) }
            0xf1 => { let v = $this.get_value(Mode::INDIRECT_Y); $this.sbc(v) }

            // SEC
            0x38 => { $this.sec() }

            // SED
            0xf8 => { $this.sed() }

            // SEI
            0x78 => { $this.sei() }

            // STA
            0x85 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.sta(v) }
            0x95 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.sta(v) }
            0x80 => { let v = $this.get_value(Mode::ABSOLUTE); $this.sta(v) }
            0x9d => { let v = $this.get_value(Mode::ABSOLUTE_X); $this.sta(v) }
            0x99 => { let v = $this.get_value(Mode::ABSOLUTE_Y); $this.sta(v) }
            0x81 => { let v = $this.get_value(Mode::INDIRECT_X); $this.sta(v) }
            0x91 => { let v = $this.get_value(Mode::INDIRECT_Y); $this.sta(v) }

            // STX
            0x86 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.stx(v) }
            0x96 => { let v = $this.get_value(Mode::ZERO_PAGE_Y); $this.stx(v) }
            0x8e => { let v = $this.get_value(Mode::ABSOLUTE); $this.stx(v) }

            // STY
            0x84 => { let v = $this.get_value(Mode::ZERO_PAGE); $this.sty(v) }
            0x94 => { let v = $this.get_value(Mode::ZERO_PAGE_X); $this.sty(v) }
            0x8c => { let v = $this.get_value(Mode::ABSOLUTE); $this.sty(v) }

            // TAX
            0xaa => { $this.tax() }

            // TAY
            0xa8 => { $this.tay() }

            // TSX
            0xba => { $this.tsx() }

            // TXA
            0x8a => { $this.txa() }

            // TXS
            0x9a => { $this.txs() }

            // TYA
            0x98 => { $this.tya() }

            _ => panic!("UNKNOWN OP")
        }
    }
}

impl Cpu {
    pub fn new(p: &Path) -> Cpu {
        Cpu{
            regs: Regs::new(),
            mem: MemMapper::new(p),
        }
    }

    pub fn trace(&mut self) {
        let mut d = Disasm::new(&self.regs, &self.mem);
        println!(
            "{:04X} {}\t A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.regs.pc,
            d.disasm(),
            self.regs.a,
            self.regs.x,
            self.regs.y,
            self.regs.p,
            self.regs.sp,
        )
    }

    pub fn is_set(&self, flag: u8) -> bool {
        self.regs.is_set(flag)
    }

    pub fn unset(&mut self, flag: u8) {
        self.regs.p &= !flag
    }

    pub fn set(&mut self, flag: u8, on: bool) {
        if on {
            self.regs.p |= flag
        } else {
            self.regs.p &= !flag
        }
    }

    // Checks wether the sign and zero flags should be set.
    // This is grouped together because the two operations
    // occur together a lot in the instruction set.
    pub fn set_sz(&mut self, val: u8) {
        self.set(ZERO_FLAG, val == 0);
        self.set(NEGATIVE_FLAG, (val & 0x80) != 0)
    }

    pub fn step(&mut self) {
        self.trace();
        let op = self.loadb_bump_pc();
        decode_op!(op, self);
    }

    pub fn pushb(&mut self, val: u8) {
        self.mem.writeb(0x100 + self.regs.sp as u16, val);
        self.regs.sp -= 1
    }

    pub fn popb(&mut self) -> u8 {
        let v = self.mem.loadb(0x100 + self.regs.sp as u16);
        self.regs.sp += 1;
        return v
    }

    pub fn loadb(&self, addr: u16) -> u8 {
        self.mem.loadb(addr)
    }

    pub fn writeb(&mut self, addr: u16, val: u8) {
        self.mem.writeb(addr, val)
    }

    // The lowest byte is first
    pub fn loadw(&self, addr: u16) -> u16 {
        let lower = self.loadb(addr) as u16;
        let upper = self.loadb(addr+1) as u16;
        return (upper << 8) | lower
    }

    pub fn loadb_bump_pc(&mut self) -> u8 {
        let val = self.loadb(self.regs.pc);
        self.regs.pc += 1;
        return val
    }

    pub fn loadw_bump_pc(&mut self) -> u16 {
        let val = self.loadw(self.regs.pc);
        self.regs.pc += 2;
        return val
    }

    pub fn get_value(&mut self, mode: Mode) -> u16 {
        match mode {
            Mode::ACCUMULATOR => { self.regs.a as u16 }
            Mode::IMMEDIATE => { self.loadb_bump_pc() as u16 }
            Mode::INDIRECT => { let v = self.loadw_bump_pc(); self.loadw(v) }
            Mode::ZERO_PAGE => { self.loadb_bump_pc() as u16 }
            Mode::ZERO_PAGE_X => { (self.loadb_bump_pc() + self.regs.x) as u16 }
            Mode::ZERO_PAGE_Y => { (self.loadb_bump_pc() + self.regs.y) as u16 }
            Mode::ABSOLUTE => { self.loadw_bump_pc() }
            Mode::ABSOLUTE_X => { self.loadw_bump_pc() + (self.regs.x as u16) }
            Mode::ABSOLUTE_Y => { self.loadw_bump_pc() + (self.regs.y as u16) }
            Mode::INDIRECT_X => {
                let v = self.loadb_bump_pc();
                self.loadw((v + self.regs.x) as u16)
            }

            Mode::INDIRECT_Y => {
                let v = self.loadb_bump_pc() as u16;
                self.loadw(v) + self.regs.y as u16
            }
        }
    }

    // This is based on the sprocketnes implementation
    pub fn adc(&mut self, val: u16) {
        let mut result = self.regs.a as u32 + val as u32;
        if self.is_set(CARRY_FLAG) {
            result += 1;
        }

        self.set(CARRY_FLAG, result & 0x100 != 0);
        let result = result as u8;
        let a = self.regs.a;
        self.set(OVERFLOW_FLAG, (a ^ val as u8) & 0x80 == 0 && (a ^ result) & 0x80 == 0x80);
        self.set_sz(result);
        self.regs.a = result
    }

    pub fn and(&mut self, val: u16) {
        let new_val = (val as u8) + self.regs.a;
        self.set_sz(new_val);
        self.regs.a = new_val;
    }

    pub fn asl(&mut self, addr: u16, m: Mode) {
        match m {
            Mode::ACCUMULATOR => {
                let a = self.regs.a;
                self.set(CARRY_FLAG, a & 0x80 == 0x80);
                self.regs.a <<= 1 }
            _ => {
                let v = self.loadb(addr);
                self.set(CARRY_FLAG, v & 0x80 == 0x80);
                self.writeb(addr, v << 1); }
        }
    }

    pub fn bra_base(&mut self, cond: bool) {
        let v = self.loadb_bump_pc();
        if cond {
            self.regs.pc = (self.regs.pc as i32 + v as i32) as u16;
        }
    }

    pub fn bcs(&mut self) {
        let is_set = self.is_set(CARRY_FLAG);
        self.bra_base(is_set)
    }

    pub fn bcc(&mut self) {
        let is_set = !self.is_set(CARRY_FLAG);
        self.bra_base(is_set)
    }

    pub fn beq(&mut self) {
        let is_set = self.is_set(ZERO_FLAG);
        self.bra_base(is_set)
    }

    pub fn bne(&mut self) {
        let is_set = !self.is_set(ZERO_FLAG);
        self.bra_base(is_set)
    }

    pub fn bmi(&mut self) {
        let is_set = self.is_set(NEGATIVE_FLAG);
        self.bra_base(is_set)
    }

    pub fn bpl(&mut self) {
        let is_set = !self.is_set(NEGATIVE_FLAG);
        self.bra_base(is_set)
    }

    pub fn bvs(&mut self) {
        let is_set = self.is_set(OVERFLOW_FLAG);
        self.bra_base(is_set)
    }

    pub fn bvc(&mut self) {
        let is_set = !self.is_set(OVERFLOW_FLAG);
        self.bra_base(is_set)
    }

    pub fn bit(&mut self, val: u16) {
        self.set(NEGATIVE_FLAG, (val as u8 & 0x80) != 0);
        self.regs.p |= (val as u8 & 0x40);
        let a = self.regs.a;
        self.set(ZERO_FLAG, (val as u8 & a) == 0)
    }

    pub fn brk(&mut self) {
        self.regs.pc += 1;
        let pc = self.regs.pc;
        self.pushb((pc >> 8) as u8 & 0xff);
        self.pushb(pc as u8 & 0xff);
        self.set(BREAK_FLAG, true);
        let p = self.regs.p;
        self.pushb(p);
        self.set(INTERRUPT_FLAG, true);
        let mut addr = self.loadb(0xfffe) as u16;
        addr |= self.loadb(0xffff) as u16 << 8;
        self.regs.pc = addr
    }

    pub fn clc(&mut self) {
        self.unset(CARRY_FLAG)
    }

    pub fn cld(&mut self) {
        self.unset(DECIMAL_FLAG)
    }

    pub fn cli(&mut self) {
        self.unset(INTERRUPT_FLAG)
    }

    pub fn clv(&mut self) {
        self.unset(OVERFLOW_FLAG)
    }

    pub fn cmp(&mut self, val: u16) {
        let v = self.regs.a - val as u8;
        self.set(CARRY_FLAG, v < 0x100);
        self.set_sz(v)
    }

    pub fn cpx(&mut self, val: u16) {
        let v = self.regs.x - val as u8;
        self.set(CARRY_FLAG, v < 0x100);
        self.set_sz(v)
    }

    pub fn cpy(&mut self, val: u16) {
        let v = self.regs.y - val as u8;
        self.set(CARRY_FLAG, v < 0x100);
        self.set_sz(v)
    }

    pub fn dec(&mut self, addr: u16) {
        let v = (self.loadb(addr) - 1) & 0xff;
        self.set_sz(v);
        self.writeb(addr, v)
    }

    pub fn dex(&mut self) {
        let mut val = self.regs.x;
        val = (val - 1) & 0xff;
        self.set_sz(val);
        self.regs.x = val
    }

    pub fn dey(&mut self) {
        let mut val = self.regs.x;
        val = (val - 1) & 0xff;
        self.set_sz(val);
        self.regs.y = val
    }

    pub fn eor(&mut self, val: u16) {
        let v = val as u8 ^ self.regs.a;
        self.set_sz(v);
        self.regs.a = v
    }

    pub fn inc(&mut self, addr: u16) {
        let v = (self.loadb(addr) + 1) & 0xff;
        self.set_sz(v);
        self.writeb(addr, v)
    }

    pub fn inx(&mut self) {
        let v = (self.regs.x + 1) & 0xff;
        self.set_sz(v);
        self.regs.x = v
    }

    pub fn iny(&mut self) {
        let v = (self.regs.y + 1) & 0xff;
        self.set_sz(v);
        self.regs.y = v
    }

    pub fn jmp(&mut self, val: u16) {
        self.regs.pc = val
    }

    pub fn jsr(&mut self, val: u16) {
        self.regs.pc -= 1;
        let b1 = (self.regs.pc >> 8) as u8;
        self.pushb(b1);
        let b2 = self.regs.pc as u8;
        self.pushb(b2);
        self.regs.pc = val
    }

    pub fn lda(&mut self, val: u16) {
        self.set_sz(val as u8);
        self.regs.a = val as u8;
    }

    pub fn ldx(&mut self, val: u16) {
        self.set_sz(val as u8);
        self.regs.x = val as u8;
    }

    pub fn ldy(&mut self, val: u16) {
        self.set_sz(val as u8);
        self.regs.y = val as u8;
    }

    pub fn lsr(&mut self, addr: u16, m: Mode) {
        match m {
            Mode::ACCUMULATOR => {
                let a = self.regs.a;
                self.set(CARRY_FLAG, a & 0x01 == 1);
                self.regs.a >>= 1 }
            _ => {
                let v = self.loadb(addr);
                self.set(CARRY_FLAG, v & 0x01 == 1);
                self.writeb(addr, v >> 1); }
        }
    }

    pub fn nop(&mut self) {}

    pub fn ora(&mut self, val: u16) {
        let v = val as u8 | self.regs.a;
        self.set_sz(v);
        self.regs.a = v
    }

    pub fn pha(&mut self) {
        let a = self.regs.a;
        self.pushb(a)
    }

    pub fn php(&mut self) {
        let p = self.regs.p;
        self.pushb(p)
    }

    pub fn pla(&mut self) {
        let a = self.popb();
        self.set_sz(a);
        self.regs.a = a
    }

    pub fn plp(&mut self) {
        let p = self.popb();
        self.regs.p = p
    }

    pub fn rol(&mut self, addr: u16, m: Mode) {
        // TODO: Clean this up
        match m {
            Mode::ACCUMULATOR => {
                let mut v = self.regs.a as u16;
                v <<= 1;
                if self.is_set(CARRY_FLAG) {
                    v |= 0x1;
                }
                self.set(CARRY_FLAG, v > 0xff);
                self.set_sz(v as u8);
                self.regs.a = v as u8
            }

            _ => {
                let mut v = self.loadb(addr);
                v <<= 1;
                if self.is_set(CARRY_FLAG) {
                    v |= 0x1;
                }
                self.set(CARRY_FLAG, v > 0xff);
                self.set_sz(v as u8);
                self.writeb(addr, v);
            }
        }
    }

    pub fn ror(&mut self, addr: u16, m: Mode) {
        // TODO: Clean this up
        match m {
            Mode::ACCUMULATOR => {
                let mut v = self.regs.a as u16;
                if self.is_set(CARRY_FLAG) {
                    v |= 0x100;
                }
                self.set(CARRY_FLAG, v & 0x01 == 1);
                v >>= 1;
                self.set_sz(v as u8);
                self.regs.a = v as u8
            }

            _ => {
                let mut v = self.loadb(addr) as u16;
                if self.is_set(CARRY_FLAG) {
                    v |= 0x100;
                }
                self.set(CARRY_FLAG, v & 0x01 == 1);
                v >>= 1;
                self.set_sz(v as u8);
                self.writeb(addr, v as u8)
            }
        }
    }

    pub fn rti(&mut self) {
        let p = self.popb();
        self.regs.p = p;
        let mut addr = self.popb() as u16;
        addr |= self.popb() as u16 << 8;
        self.regs.pc = addr
    }

    pub fn rts(&mut self) {
        let mut addr = self.popb() as u16;
        addr += (self.popb() as u16 << 8) + 1;
        self.regs.pc = addr
    }

    pub fn sbc(&mut self, val: u16) {}

    pub fn sec(&mut self) {
        self.set(CARRY_FLAG, true)
    }

    pub fn sed(&mut self) {
        self.set(DECIMAL_FLAG, true)
    }

    pub fn sei(&mut self) {
        self.set(INTERRUPT_FLAG, true)
    }

    pub fn sta(&mut self, addr: u16) {
        self.mem.writeb(addr, self.regs.a)
    }

    pub fn stx(&mut self, addr: u16) {
        self.mem.writeb(addr, self.regs.x)
    }

    pub fn sty(&mut self, addr: u16) {
        self.mem.writeb(addr, self.regs.y)
    }

    pub fn tax(&mut self) {
        let v = self.regs.a;
        self.set_sz(v);
        self.regs.x = v
    }

    pub fn tay(&mut self) {
        let v = self.regs.a;
        self.set_sz(v);
        self.regs.y = v
    }

    pub fn tsx(&mut self) {
        let v = self.regs.sp;
        self.set_sz(v);
        self.regs.x = v
    }

    pub fn txa(&mut self) {
        let v = self.regs.x;
        self.set_sz(v);
        self.regs.a = v
    }

    pub fn txs(&mut self) {
        let v = self.regs.x;
        self.regs.sp = v
    }

    pub fn tya(&mut self) {
        let v = self.regs.y;
        self.set_sz(v);
        self.regs.a = v
    }
}
