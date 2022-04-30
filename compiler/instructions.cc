#include "compiler/instructions.h"

#include "compiler/builtin_module.h"
#include "core/call.h"
#include "ir/instruction/arithmetic.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/core.h"
#include "ir/instruction/instructions.h"
#include "ir/instruction/set.h"
#include "ir/value/char.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/function_instructions.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/pointer_instructions.h"
#include "type/slice.h"
#include "type/struct.h"

namespace compiler {

std::vector<ir::Block> InterpretScopeAtCompileTime(
    module::SharedContext const& shared_context, ir::Scope s,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const& arguments) {
  ir::CompleteResultBuffer arguments_buffer;
  core::BindArguments(s.type()->parameters(), arguments,
                      [&](type::QualType param,
                          type::Typed<ir::CompleteResultRef> argument) mutable {
                        arguments_buffer.append_raw(argument->raw());
                      });

  std::vector<ir::Block> result;
  ir::interpreter::Interpreter interpreter(&shared_context);
  interpreter.push_frame(&*s, std::move(arguments_buffer),
                         {reinterpret_cast<ir::addr_t>(&result)});

  bool ok = interpreter();
  ASSERT(ok == true);
  return result;
}

}  // namespace compiler
