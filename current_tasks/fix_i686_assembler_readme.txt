Fix i686 assembler README.md for correctness

Issues to fix:
1. File inventory line counts are wrong (encoder.rs: says ~2370, actual 3282; elf_writer.rs: says ~1233, actual 95; parser.rs: says ~1100, actual 1708)
2. elf_writer.rs is described as a full ELF writer but it's actually a 95-line thin wrapper around shared elf_writer_common.rs (1565 lines)
3. Key Data Structures table lists phantom types (Elf32ByteWriter, Sym32Entry) that don't exist in the codebase
4. Missing @INDNTPOFF modifier in TLS table
5. Pipeline diagram doesn't mention the shared ElfWriterCore
