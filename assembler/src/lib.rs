mod opcode;

use opcode::OpCodeWithArgs;

use xelis_types::Value;
use xelis_bytecode::{Chunk, Module};

#[derive(Debug)]
pub enum AssemblerError {
    OpCode(&'static str),
    ExpectedChunk,
}

// Assembler to convert source code into bytecode
pub struct Assembler<'a> {
    module: Module,
    source: &'a str,
    chunks_labels: Vec<&'a str>,
    jump_labels: Vec<(&'a str, u32)>,
}

impl<'a> Assembler<'a> {
    // Create a new assembler with the given source code
    pub fn new(source: &'a str) -> Self {
        Assembler {
            module: Module::new(),
            source,
            chunks_labels: Vec::new(),
            jump_labels: Vec::new(),
        }
    }

    // Add a constant to the module and return its index
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.module.add_constant(value)
    }

    // Assemble the source code into bytecode
    pub fn assemble(mut self) -> Result<Module, AssemblerError> {
        let mut chunk = None;
        for line in self.source.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("//") {
                // Ignore comments and empty lines
            } else if line.starts_with("#") {
                // Push the previous chunk and create a new one
                if let Some(chunk) = chunk.take() {
                    self.module.add_chunk(chunk);
                }

                chunk = Some(Chunk::new());
                self.chunks_labels.push(&line[1..]);
            } else if line.starts_with(":") {
                // Register a jump label for the next instruction
                let c = chunk.as_mut().ok_or(AssemblerError::ExpectedChunk)?;
                self.jump_labels.push((&line[1..], c.index() as u32));
            } else {
                let op = OpCodeWithArgs::from_str_with_labels(line, &self.chunks_labels, &self.jump_labels)
                    .map_err(AssemblerError::OpCode)?;

                op.write_to_chunk(chunk.as_mut().ok_or(AssemblerError::ExpectedChunk)?);
            }
        }

        if let Some(chunk) = chunk.take() {
            self.module.add_chunk(chunk);
        }

        Ok(self.module)
    }
}

#[cfg(test)]
mod tests {
    use xelis_bytecode::OpCode;

    use super::*;

    #[test]
    fn test_assemble() {
        let source = r#"
            #main
            CONSTANT 0
            COPY
            ADD
        "#;

        let assembler = Assembler::new(source);
        let module = assembler.assemble().unwrap();

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(chunk.get_instructions(), &[
            OpCode::Constant.as_byte(), 0, 0,
            OpCode::Copy.as_byte(),
            OpCode::Add.as_byte(),
        ]);
    }

    #[test]
    fn test_assemble_multiple_chunks() {
        let source = r#"
            #main
            CONSTANT 0
            COPY
            ADD

            #other
            CONSTANT 1
            COPY
            ADD
        "#;

        let assembler = Assembler::new(source);
        let module = assembler.assemble().unwrap();

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(chunk.get_instructions(), &[
            OpCode::Constant.as_byte(), 0, 0,
            OpCode::Copy.as_byte(),
            OpCode::Add.as_byte(),
        ]);

        let chunk = module.get_chunk_at(1).unwrap();
        assert_eq!(chunk.get_instructions(), &[
            OpCode::Constant.as_byte(), 1, 0,
            OpCode::Copy.as_byte(),
            OpCode::Add.as_byte(),
        ]);
    }

    #[test]
    fn test_chunks_labels() {
        let source = r#"
            #other
            CONSTANT 1
            COPY
            ADD

            #main
            INVOKECHUNK #other false 0
        "#;

        let assembler = Assembler::new(source);
        let module = assembler.assemble().unwrap();

        assert_eq!(module.chunks().len(), 2);
        assert_eq!(
            module.chunks().last().unwrap().get_instructions(),
            &[OpCode::InvokeChunk.as_byte(), 0, 0, 0, 0]
        );
    }

    #[test]
    fn test_jump_labels() {
        let source = r#"
            #main
            CONSTANT 1
            COPY
            ADD
            :label
            JUMP :label
        "#;

        let assembler = Assembler::new(source);
        let module = assembler.assemble().unwrap();

        assert_eq!(module.chunks().len(), 1);
        assert_eq!(
            module.chunks().last().unwrap().get_instructions(),
            &[
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Copy.as_byte(),
                OpCode::Add.as_byte(),
                OpCode::Jump.as_byte(), 5, 0, 0, 0,
            ]
        );
    }
}