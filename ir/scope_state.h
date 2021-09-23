#ifndef ICARUS_IR_SCOPE_STATE_H
#define ICARUS_IR_SCOPE_STATE_H

#include <functional>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "ir/blocks/basic.h"
#include "ir/value/label.h"
#include "ir/value/result_buffer.h"
#include "ir/value/scope.h"
#include "type/qual_type.h"

namespace ir {

struct ScopeState {
  // A (possibly trivial) label for this block so that yield statements nested
  // inside this scope can jump to it.
  Label label;
  Scope scope;
  type::QualType result_type;
  BasicBlock* block;
  // A map keyed on the names of blocks that appear in this ScopeNode and
  // whose mapped values are the corresponding entry block for that scope.
  absl::flat_hash_map<std::string_view, BasicBlock*> names;

  // If present, the register is the address of the scope state.
  std::optional<ir::Reg> state;

  // TODO: The callable should probably take an ir::PartialResultRef rather than
  // an ir::Reg.
  absl::flat_hash_map<
      std::string,
      std::vector<std::function<void(ir::BasicBlock const*, ir::Reg)>>>
      set_phis;
};

}  // namespace ir

#endif  //  ICARUS_IR_SCOPE_STATE_H
