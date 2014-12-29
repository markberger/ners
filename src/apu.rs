pub struct Apu;

impl Apu {
    pub fn new() -> Apu {
        Apu
    }
    pub fn loadb(&self, addr: u16) -> u8 {
        0
    }

    pub fn writeb(&mut self, addr: u16, val: u8) {}
}
