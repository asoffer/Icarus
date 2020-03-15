#ifndef ICARUS_IR_INSTRUCTION_BASE_H
#define ICARUS_IR_INSTRUCTION_BASE_H

#include <string>

#include "base/cast.h"
#include "base/clone.h"
#include "base/untyped_buffer.h"

// TODO rename this file so that when you forget that for dependency reasons you
// should try to include this instead of instructions.h, you reach for this one
// anyway.
namespace ir {

struct InstructionInliner;
struct ByteCodeWriter;

// Instruction:
// This is the base class for all instructions in the intermediate
// representation. 
struct Instruction : base::Clone<Instruction, void>, base::Cast<Instruction> {
  virtual ~Instruction() {}
  virtual std::string to_string() const { return "[[unknown]]"; }

  // Each instruction must specify how it should be serialized into byte-code.
  // The byte-code is the format consumed by the interpretter.
  virtual void WriteByteCode(ByteCodeWriter*) const = 0;

  // Each instruction must specify how it should be inlined. Typically this
  // involves updating register numbers as tracked by the `InstructionInliner`
  // parameter.
  virtual void Inline(InstructionInliner const&) = 0;
};

}  // namespace ir

#endif  //  ICARUS_IR_INSTRUCTION_BASE_H
