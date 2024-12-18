use crate::{iterator::PathIterator, stack::Stack, Backend, ChunkManager, VMError};
use xelis_types::{Value, Path};

use super::InstructionResult;

pub fn iterable_length<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let len = value.as_ref().as_vec()?.len();
    stack.push_stack_unchecked(Path::Owned(Value::U32(len as u32)));
    Ok(InstructionResult::Nothing)
}

pub fn iterator_begin<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let iterator = PathIterator::new(value)?;
    manager.add_iterator(iterator);
    Ok(InstructionResult::Nothing)
}

pub fn iterator_next<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let addr = manager.read_u32()?;
    if let Some(value) = manager.next_iterator()? {
        stack.push_stack(value)?;
    } else {
        manager.set_index(addr as usize);
    }
    Ok(InstructionResult::Nothing)
}

pub fn iterator_end<'a>(_: &Backend<'a>, _: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    manager.pop_iterator()?;
    Ok(InstructionResult::Nothing)
}

pub fn new_range<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let end = stack.pop_stack()?;
    let start = stack.pop_stack()?;

    if !start.as_ref().is_number() {
        return Err(VMError::InvalidRangeType);
    }

    let start_type = start.as_ref().get_type()?;
    if start_type != end.as_ref().get_type()? {
        return Err(VMError::InvalidRangeType);
    }

    let value = Value::Range(Box::new(start.into_owned()), Box::new(end.into_owned()), start_type);
    stack.push_stack_unchecked(Path::Owned(value));
    Ok(InstructionResult::Nothing)
}