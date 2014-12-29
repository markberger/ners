use std::io::File;
use std::vec::Vec;
// Based on information from:
//     * http://sadistech.com/nesromtool/romdoc.html
//     * http://wiki.nesdev.com/w/index.php/INES

// 16-byte header at the beginning of a .nes file.
//     * prg_rom_size: Size of PRG ROM in 16KB units
//     * chr_rom_size: Size of CHR ROM in 8 KB units
//        (Value 0 means the board uses CHR RAM)
//     * Size of PRG RAM in 8 KB units
//        (Value 0 infers 8 KB for compatibility)

struct INesHeader {
    head: [u8, ..4],
    prg_rom_size: u8,
    chr_rom_size: u8,
    flags_six: u8,
    flags_seven: u8,
    prg_ram_size: u8,
    flags_nine: u8,
    flags_ten: u8,
    zeros: [u8, ..5],
}

impl INesHeader {
    pub fn new(h: &[u8]) -> INesHeader {
        // TODO: Check h is actually 16 bytes
        INesHeader {
            head: [h[0], h[1], h[2], h[3]],
            prg_rom_size: h[4],
            chr_rom_size: h[5],
            flags_six: h[6],
            flags_seven: h[7],
            prg_ram_size: h[8],
            flags_nine: h[9],
            flags_ten: h[10],
            zeros: [h[11], h[12], h[13], h[14], h[15]],
        }
    }

    pub fn has_trainer(&self) -> bool {
        self.flags_six & 0x4 == 0x4
    }

    pub fn prg_rom_size(&self) -> uint {
        self.prg_rom_size as uint
    }

    pub fn chr_rom_size(&self) -> uint {
        self.chr_rom_size as uint
    }

    pub fn mapper(&self) -> uint {
        ((self.flags_seven & 0xF0) & self.flags_six >> 4) as uint
    }
}

pub struct INesRom {
    header: INesHeader,
    pub trainer: Vec<u8>,
    pub prg_data: Vec<u8>,
    pub chr_data: Vec<u8>,
}

impl INesRom {
    pub fn new(p: &Path) -> INesRom {
        let contents = File::open(p).read_to_end().unwrap();
        let header = INesHeader::new(contents.slice(0, 16));

        let mut ines = INesRom{
            header: header,
            trainer: Vec::new(),
            prg_data: Vec::new(),
            chr_data: Vec::new(),
        };

        ines.prg_data.push_all(contents.slice(16, 16 + 16384 * ines.header.prg_rom_size()));
        return ines
    }
}
