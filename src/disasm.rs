use cpu::Mode;
use cpu::Regs;
use mem::MemMapper;

pub struct Disasm<'a> {
    pub regs: Regs,
    pub mem: &'a MemMapper
}

impl<'a> Disasm<'a> {
    pub fn new(regs: &Regs, mem: &'a MemMapper) -> Disasm<'a> {
        let new_regs = Regs{
            a: regs.a,
            x: regs.x,
            y: regs.y,
            pc: regs.pc,
            sp: regs.sp,
            p: regs.p,
        };

        Disasm{ regs: new_regs, mem: mem }
    }

    pub fn disasm(&mut self) -> String {
        let op = self.loadb_bump_pc();
        decode_op!(op, self)
    }

    pub fn loadb(&self, addr: u16) -> u8 {
        self.mem.loadb(addr)
    }

    pub fn loadb_bump_pc(&mut self) -> u8 {
        let v = self.loadb(self.regs.pc);
        self.regs.pc += 1;
        return v
    }

    pub fn loadw(&self, addr: u16) -> u16 {
        let lower = self.loadb(addr) as u16;
        let upper = self.loadb(addr+1) as u16;
        return (upper << 8) | lower
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

    pub fn adc(&mut self, val: u16) -> String {
        "ADC".to_string()
    }

    pub fn and(&mut self, val: u16) -> String {
        "AND".to_string()
    }

    pub fn asl(&mut self, val: u16, m: Mode) -> String {
        "ASL".to_string()
    }

    pub fn bra_val(&mut self) -> u16 {
        let v = self.loadb_bump_pc();
        (self.regs.pc as i32 + v as i32) as u16

    }

    pub fn bcs(&mut self) -> String {
        format!("BCS {:x}", self.bra_val()).to_string()
    }

    pub fn bcc(&mut self) -> String {
        format!("BCC {:x}", self.bra_val()).to_string()
    }

    pub fn beq(&mut self) -> String {
        format!("BEQ {:x}", self.bra_val()).to_string()
    }

    pub fn bne(&mut self) -> String {
        "BNE".to_string()
    }

    pub fn bmi(&mut self) -> String {
        "BMI".to_string()
    }

    pub fn bpl(&mut self) -> String {
        "BPL".to_string()
    }

    pub fn bvs(&mut self) -> String {
        "BVS".to_string()
    }

    pub fn bvc(&mut self) -> String {
        "BVC".to_string()
    }

    pub fn bit(&mut self, val: u16) -> String {
        "BIT".to_string()
    }

    pub fn brk(&mut self) -> String {
        "BRK".to_string()
    }

    pub fn clc(&mut self) -> String {
        "CLC".to_string()
    }

    pub fn cld(&mut self) -> String {
        "CLD".to_string()
    }

    pub fn cli(&mut self) -> String {
        "CLI".to_string()
    }

    pub fn clv(&mut self) -> String {
        "CLV".to_string()
    }

    pub fn cmp(&mut self, val: u16) -> String {
        "CMP".to_string()
    }

    pub fn cpx(&mut self, val: u16) -> String {
        "CPX".to_string()
    }

    pub fn cpy(&mut self, val: u16) -> String {
        "CPY".to_string()
    }

    pub fn dec(&mut self, val: u16) -> String {
        "DEC".to_string()
    }

    pub fn dex(&mut self) -> String {
        "DEX".to_string()
    }

    pub fn dey(&mut self) -> String {
        "DEY".to_string()
    }

    pub fn eor(&mut self, val: u16) -> String {
        "EOR".to_string()
    }

    pub fn inc(&mut self, val: u16) -> String {
        "INC".to_string()
    }

    pub fn inx(&mut self) -> String {
        "INX".to_string()
    }

    pub fn iny(&mut self) -> String {
        "INY".to_string()
    }

    pub fn jmp(&mut self, addr: u16) -> String {
        format!("JMP {:x}", addr).to_string()
    }

    pub fn jsr(&mut self, addr: u16) -> String {
        format!("JSR {:x}", addr).to_string()
    }

    pub fn lda(&mut self, val: u16) -> String {
        "LDA".to_string()
    }

    pub fn ldx(&mut self, val: u16) -> String {
        "LDX".to_string()
    }

    pub fn ldy(&mut self, val: u16) -> String {
        "LDY".to_string()
    }

    pub fn lsr(&mut self, val: u16, m: Mode) -> String {
        "LSR".to_string()
    }

    pub fn nop(&mut self) -> String {
        "NOP".to_string()
    }

    pub fn ora(&mut self, val: u16) -> String {
        "ORA".to_string()
    }

    pub fn pha(&mut self) -> String {
        "PHA".to_string()
    }

    pub fn php(&mut self) -> String {
        "PHP".to_string()
    }

    pub fn pla(&mut self) -> String {
        "PLA".to_string()
    }

    pub fn plp(&mut self) -> String {
        "PLP".to_string()
    }

    pub fn rol(&mut self, val: u16, m: Mode) -> String {
        "ROL".to_string()
    }

    pub fn ror(&mut self, val: u16, m: Mode) -> String {
        "ROR".to_string()
    }

    pub fn rti(&mut self) -> String {
        "RTI".to_string()
    }

    pub fn rts(&mut self) -> String {
        "RTS".to_string()
    }

    pub fn sbc(&mut self, val: u16) -> String {
        "SBC".to_string()
    }

    pub fn sec(&mut self) -> String {
        "SEC".to_string()
    }

    pub fn sed(&mut self) -> String {
        "SED".to_string()
    }

    pub fn sei(&mut self) -> String {
        "SEI".to_string()
    }

    pub fn sta(&mut self, addr: u16) -> String {
        "STA".to_string()
    }

    pub fn stx(&mut self, addr: u16) -> String {
        "STX".to_string()
    }

    pub fn sty(&mut self, addr: u16) -> String {
        "STY".to_string()
    }

    pub fn tax(&mut self) -> String {
        "TAX".to_string()
    }

    pub fn tay(&mut self) -> String {
        "TAY".to_string()
    }

    pub fn tsx(&mut self) -> String {
        "TSX".to_string()
    }

    pub fn txa(&mut self) -> String {
        "TXA".to_string()
    }

    pub fn txs(&mut self) -> String {
        "TXS".to_string()
    }

    pub fn tya(&mut self) -> String {
        "TYA".to_string()
    }
}
