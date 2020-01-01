#ifndef ICARUS_IR_INSTRUCTIONS_BASE_H
#define ICARUS_IR_INSTRUCTIONS_BASE_H

#include <string>

#include "base/cast.h"
#include "base/clone.h"
#include "base/untyped_buffer.h"

// TODO rename this file so that when you forget that for dependency reasons you
// should try to include this instead of instructions.h, you reach for this one
// anyway.
namespace ir {

struct Inliner;
struct ByteCodeWriter;

struct Instruction : base::Clone<Instruction, void>, base::Cast<Instruction> {
  virtual ~Instruction() {}
  virtual std::string to_string() const { return "[[unknown]]"; }

  virtual void WriteByteCode(ByteCodeWriter*) const = 0;
  virtual void Inline(Inliner const&)               = 0;
};


}  // namespace ir

#endif  //  ICARUS_IR_INSTRUCTIONS_BASE_H
