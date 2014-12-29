#![feature(macro_rules)]

use cpu::Cpu;

pub mod cpu;
pub mod rom;
pub mod mem;
pub mod ppu;
pub mod apu;
pub mod romMapper;
pub mod disasm;

const TEST_ROM: &'static str = "tests/nestest.nes";

fn main() {
    let p = Path::new(TEST_ROM);
    let mut cpu = Cpu::new(&p);
    for x in range(0i, 8991) {
        cpu.step();
    }
}
