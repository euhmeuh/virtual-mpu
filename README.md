# virtual-mpu
Old Microprocessor Emulator and Assembler

### Installation

`raco pkg install virtual-mpu`


### Assembler Usage

#### Generate a [S-Record file](https://en.wikipedia.org/wiki/SREC_(file_format)) (Motorola format):
`virtual-mpu assemble to-s-record myfile.asm > myfile.srec` 

#### Display result in hexadecimal in the terminal (useful for debugging):
`virtual-mpu assemble to-hex myfile.asm` 

#### Put result into a program binary:
`virtual-mpu assemble to-binary myfile.asm > myfile.bin` 


### Emulator usage

#### Start the emulator (WIP)

*(this does not work yet)*

`virtual-mpu emulate machines/mymachine.rkt mykernel.bin`

### Supported MPUs:

- Motorola 6802 (used mostly in pinball machines)

### Planned:

- MOS 6502 (used in 8bit Atari models, Apple II, NES, Commodore 64...)
- Motorola 68000, aka. m68k (used in Apple Lisa, Macintosh, Amiga, Neo Geo, Mega Drive, Atari Jaguar, Saturn...)
- Zilog Z80 (used in ZX Spectrum, Amstrad CPC, Sega Master System, Nintendo Game Boy, GBC, GBA)
- Intel 8080 (used in Altair 8800, Space Invaders)
