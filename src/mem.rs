use ppu::Ppu;
use apu::Apu;
use romMapper::RomMapper;

pub struct Ram {
    pub val: [u8, ..0x800]
}

impl Ram {
    pub fn new() -> Ram {
        Ram{ val: [0, ..0x800] }
    }

    pub fn loadb(&self, addr: u16) -> u8 {
        self.val[addr as uint & 0x7ff]
    }

    pub fn writeb(&mut self, addr: u16, val: u8) {
        self.val[addr as uint & 0x7ff] = val
    }
}

pub struct MemMapper {
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    pub romMapper: RomMapper,
}

impl MemMapper {
    pub fn new(p: &Path) -> MemMapper {
        MemMapper{
            ram: Ram::new(),
            ppu: Ppu::new(),
            apu: Apu::new(),
            romMapper: RomMapper::new(p),
        }
    }

    // See the following link for details:
    // http://wiki.nesdev.com/w/index.php/CPU_memory_map
    pub fn loadb(&self, addr: u16) -> u8 {
        if addr < 0x2000 {
            self.ram.loadb(addr)
        } else if addr < 0x4000 {
            self.ppu.loadb(addr)
        } else if addr < 0x4016 {
            self.apu.loadb(addr)
        } else if addr < 0x4020 {
            panic!("HAVE NOT HOOKED UP INPUT YET")
        } else {
            self.romMapper.loadb(addr)
        }
    }

    pub fn writeb(&mut self, addr: u16, val: u8) {
        if addr < 0x2000 {
            self.ram.writeb(addr, val)
        } else if addr < 0x4000 {
            self.ppu.writeb(addr, val)
        } else if addr < 0x4016 {
            self.apu.writeb(addr, val)
        } else if addr < 0x4020 {
            panic!("HAVE NOT HOOKED UP INPUT YET")
        } else {
            self.romMapper.writeb(addr, val)
        }
    }
}
