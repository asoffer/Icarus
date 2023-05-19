#ifndef ICARUS_SEMANTIC_ANALYSIS_FUNCTION_DATA_H
#define ICARUS_SEMANTIC_ANALYSIS_FUNCTION_DATA_H

#include "ast/declaration.h"
#include "nth/container/flyweight_map.h"
#include "vm/function.h"

namespace semantic_analysis {

enum class EmitKind { Reference, Value, Statement };

struct TemporarilySet {
  explicit constexpr TemporarilySet(EmitKind &kind, EmitKind k)
      : kind_(kind), value_(std::exchange(kind_, k)) {}
  ~TemporarilySet() { kind_ = value_; }

 private:
  EmitKind &kind_;
  EmitKind value_;
};

struct FunctionData {
  FunctionData(vm::Function &function,
               nth::flyweight_map<ast::Declaration::Id const *, size_t>
                   &variable_offsets)
      : function_(function), variable_offsets_(variable_offsets) {}

  vm::Function &function() { return function_; }

  EmitKind &kind() { return kind_; }

  size_t OffsetFor(ast::Declaration::Id const *id) const {
    auto iter = variable_offsets_.find(id);
    ASSERT(iter != variable_offsets_.end());
    return iter->second;
  }

  nth::flyweight_map<ast::Declaration::Id const *, size_t> &offsets() const {
    return variable_offsets_;
  }

 private:
  vm::Function &function_;
  EmitKind kind_ = EmitKind::Statement;
  nth::flyweight_map<ast::Declaration::Id const *, size_t> &variable_offsets_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_FUNCTION_DATA_H
