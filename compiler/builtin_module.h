#ifndef ICARUS_COMPILER_BUILTIN_MODULE_H
#define ICARUS_COMPILER_BUILTIN_MODULE_H

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/set.h"
#include "ir/interpreter/execution_context.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope_context.h"
#include "ir/value/slice.h"
#include "module/builtin.h"
#include "type/type.h"

namespace compiler {

struct AbortInstruction
    : base::Extend<AbortInstruction>::With<base::BaseTraverseExtension,
                                           base::BaseSerializeExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "abort";

  void Apply(interpreter::ExecutionContext&) const { std::abort(); }
};

struct AlignmentInstruction
    : base::Extend<AlignmentInstruction>::With<base::BaseTraverseExtension,
                                               base::BaseSerializeExtension,
                                               ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = alignment %1$s";

  uint64_t Resolve() const {
    return type.value().alignment(interpreter::kArchitecture).value();
  }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct BytesInstruction
    : base::Extend<BytesInstruction>::With<base::BaseTraverseExtension,
                                           base::BaseSerializeExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = bytes %1$s";

  uint64_t Resolve() const {
    return type.value().bytes(interpreter::kArchitecture).value();
  }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct HasBlockInstruction
    : base::Extend<HasBlockInstruction>::With<base::BaseTraverseExtension,
                                              base::BaseSerializeExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = has_block %1$s %2$s";

  bool Resolve() const {
    return context.value().find(name.value()) != ir::Block::Invalid();
  }

  ir::RegOr<ir::ScopeContext> context;
  ir::RegOr<ir::Slice> name;
  ir::Reg result;
};

using BuiltinInstructions =
    ir::InstructionSet<AbortInstruction, AlignmentInstruction, BytesInstruction,
                       HasBlockInstruction>;

// Returns a BuiltinModule consisting of all nodes built in as language
// intrinsics.
std::unique_ptr<module::BuiltinModule> MakeBuiltinModule();

}  // namespace compiler

#endif  // ICARUS_COMPILER_BUILTIN_MODULE_H
