#include "compiler/compiler.h"

#include "ast/ast.h"
#include "base/log.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "ir/interpreter/evaluate.h"
#include "ir/value/value.h"
#include "type/generic_struct.h"
#include "type/jump.h"

namespace compiler {

WorkItem::Result WorkItem::Process() const {
  Compiler c(resources);
  switch (kind) {
    case Kind::VerifyEnumBody:
      return c.VerifyBody(&node->as<ast::EnumLiteral>());
    case Kind::VerifyFunctionBody:
      return c.VerifyBody(&node->as<ast::FunctionLiteral>());
    case Kind::VerifyStructBody:
      return c.VerifyBody(&node->as<ast::StructLiteral>());
    case Kind::CompleteStructMembers:
      return c.CompleteStruct(&node->as<ast::StructLiteral>());
    case Kind::EmitJumpBody: return c.EmitJumpBody(&node->as<ast::Jump>());
  }
}

Compiler::Compiler(PersistentResources const &resources)
    : resources_(resources) {}
Compiler Compiler::WithPersistent() const { return Compiler(resources_); }

void Compiler::CompleteDeferredBodies() {
  while (true) {
    if (state_.deferred_work.empty()) { return; }
    for (auto &work : state_.deferred_work) { std::move (*work)(); }
  }
}

static ir::CompiledFn MakeThunk(Compiler &c, ast::Expression const *expr,
                                type::Type type) {
  LOG("MakeThunk", "Thunk for %s: %s", expr->DebugString(), type.to_string());
  ir::CompiledFn fn(type::Func({}, {type}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    c.builder().CurrentBlock() = fn.entry();

    auto val = c.EmitValue(expr);
    // TODO wrap this up into SetRet(vector)
    std::vector<type::Type> extracted_types;
    if (auto *tup = type.if_as<type::Tuple>()) {
      extracted_types = tup->entries_;
    } else {
      extracted_types = {type};
    }

    if (type != type::Void) { ASSERT(val.empty() == false); }
    // TODO is_big()?

    type::Type t = extracted_types[0];
    LOG("MakeThunk", "%s %s", t, t.get()->is_big() ? "true" : "false");
    if (t.get()->is_big()) {
      // TODO must `r` be holding a register?
      // TODO guaranteed move-elision

      c.EmitMoveInit(
          type::Typed<ir::Reg>(c.builder().GetRet(0, t), type::Ptr(t)),
          type::Typed<ir::Value>(val, t));

    } else if (auto const *gs = t.if_as<type::GenericStruct>()) {
      c.builder().SetRet(0, gs);
    } else {
      c.builder().SetRet(0, type::Typed<ir::Value>(val, t));
    }
    c.builder().ReturnJump();
  }

  ASSERT(fn.work_item == nullptr);
  fn.WriteByteCode<interpreter::instruction_set_t>();

  return fn;
}

base::expected<ir::Value, interpreter::EvaluationFailure> Compiler::Evaluate(
    type::Typed<ast::Expression const *> expr, bool must_complete) {
  Compiler c             = MakeChild(resources_);
  c.state_.must_complete = must_complete;
  auto result = interpreter::Evaluate(MakeThunk(c, *expr, expr.type()));
  if (not result) { return result; }
  c.CompleteWorkQueue();
  return result;
}

ir::ModuleId Compiler::EvaluateModuleWithCache(ast::Expression const *expr) {
  // TODO: Implement caching behavior.
  if (auto maybe_mod = EvaluateOrDiagnoseAs<ir::ModuleId>(expr)) {
    return *maybe_mod;
  } else {
    return ir::ModuleId::Invalid();
  }
}

Context::InsertSubcontextResult Compiler::Instantiate(
    ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  Context scratchpad = context().ScratchpadSubcontext();
  Compiler c({
      .data                = scratchpad,
      .diagnostic_consumer = diag(),
      .importer            = importer(),
  });
  return context().InsertSubcontext(node, c.ComputeParamsFromArgs(node, args),
                                    std::move(scratchpad));
}

Context::FindSubcontextResult Compiler::FindInstantiation(
    ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  Context scratchpad = context().ScratchpadSubcontext();
  Compiler c({
      .data                = scratchpad,
      .diagnostic_consumer = diag(),
      .importer            = importer(),
  });
  return context().FindSubcontext(node, c.ComputeParamsFromArgs(node, args));
}

void Compiler::ProcessExecutableBody(base::PtrSpan<ast::Node const> nodes,
                                     ir::CompiledFn *main_fn) {
  if (nodes.empty()) {
    ICARUS_SCOPE(ir::SetCurrent(*main_fn, builder())) {
      EmitIrForStatements(*this, nodes);
      builder().ReturnJump();
    }
  } else {
    ICARUS_SCOPE(ir::SetCurrent(*main_fn, builder())) {
      ast::ModuleScope *mod_scope =
          &nodes.front()->scope()->as<ast::ModuleScope>();

      MakeAllStackAllocations(*this, mod_scope);
      EmitIrForStatements(*this, nodes);
      MakeAllDestructions(*this, mod_scope);
      // TODO determine under which scenarios destructors can be skipped.

      builder().ReturnJump();
    }
  }
  CompleteDeferredBodies();
}

}  // namespace compiler
