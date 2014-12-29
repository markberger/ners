use rom::INesRom;

pub struct RomMapper {
    rom: INesRom,
}

impl RomMapper {
    pub fn new(p: &Path) -> RomMapper {
        RomMapper{
            rom: INesRom::new(p),
        }
    }

    pub fn loadb(&self, addr: u16) -> u8 {
        if self.rom.prg_data.len() > 1024 * 16 {
            self.rom.prg_data[(addr & 0x7fff) as uint]
        } else {
            self.rom.prg_data[(addr & 0x3fff) as uint]
        }
    }

    pub fn writeb(&mut self, addr: u16, val: u8) {}
}
