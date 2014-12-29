pub struct Ppu;

impl Ppu {
    pub fn new() -> Ppu {
        Ppu
    }
    pub fn loadb(&self, addr: u16) -> u8 {
        0
    }

    pub fn writeb(&mut self, addr: u16, val: u8) {}
}
