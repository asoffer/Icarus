#ifndef ICARUS_COMPILER_WORK_ITEM_H
#define ICARUS_COMPILER_WORK_ITEM_H

#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/meta.h"

namespace compiler {
struct Context;

// A `WorkItem` is a description of a task to be completed by the compiler
// infrastructure. Each `WorkItem` consists of an enumerator describing the kind
// of work to be done, along with the AST node on which the work should be done.
struct WorkItem : base::Extend<WorkItem>::With<base::AbslHashExtension> {
  enum class Kind {
    VerifyType,
    VerifyEnumBody,
    VerifyFunctionBody,
    VerifyStructBody,
    CompleteStructData,
    CompleteStruct,
    CompleteEnum,
    EmitFunctionBody,
    EmitShortFunctionBody,
    EmitJumpBody,
  };

  static WorkItem VerifyBodyOf(auto const *n, Context *ctx) {
    constexpr auto type = base::meta<std::decay_t<decltype(*n)>>;
    if constexpr (type == base::meta<ast::EnumLiteral>) {
      return {.kind = Kind::VerifyEnumBody, .node = n, .context = ctx};
    } else if constexpr (type == base::meta<ast::FunctionLiteral>) {
      return {.kind = Kind::VerifyFunctionBody, .node = n, .context = ctx};
    } else if constexpr (type == base::meta<ast::StructLiteral>) {
      return {.kind = Kind::VerifyStructBody, .node = n, .context = ctx};
    } else {
      static_assert(base::always_false(type));
    }
  }

  Kind kind;
  ast::Node const *node;
  Context *context;
};

// Represents a possibly non-exhaustive collection of work items that need to be
// completed, along with an indicator of whether or not it has been completed
// yet.
struct WorkSet {
  // Inserts the work item if it was not already present and returns whether or
  // not that item has previously been completed.
  bool AddWorkItem(WorkItem w) {
    return work_.try_emplace(w, false).first->second;
  }

  void MarkAsComplete(WorkItem w) { work_.insert_or_assign(w, true); }

 private:
  absl::flat_hash_map<WorkItem, bool> work_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_WORK_ITEM_H
