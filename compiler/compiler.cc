#include "compiler/compiler.h"

#include "ast/ast.h"
#include "base/log.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
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
    case Kind::EmitFunctionBody:
      return c.EmitFunctionBody(&node->as<ast::FunctionLiteral>());
    case Kind::EmitShortFunctionBody:
      return c.EmitShortFunctionBody(&node->as<ast::ShortFunctionLiteral>());
  }
}

Compiler::Compiler(PersistentResources const &resources)
    : resources_(resources) {}

void Compiler::CompleteDeferredBodies() { state_.Complete(); }

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
    if (type != type::Void) { ASSERT(val.empty() == false); }
    // TODO is_big()?

    LOG("MakeThunk", "%s %s", type, type.is_big() ? "true" : "false");
    if (type.is_big()) {
      // TODO must `r` be holding a register?
      // TODO guaranteed move-elision

      c.EmitMoveInit(type::Typed<ir::Reg>(ir::Reg::Out(0), type),
                     type::Typed<ir::Value>(val, type));

    } else if (auto const *gs = type.if_as<type::GenericStruct>()) {
      c.builder().SetRet(0, gs);
    } else {
      c.builder().SetRet(0, type::Typed<ir::Value>(val, type));
    }
    c.builder().ReturnJump();
  }

  fn.WriteByteCode<instruction_set_t>();

  return fn;
}

interpreter::EvaluationResult Compiler::Evaluate(
    type::Typed<ast::Expression const *> expr, bool must_complete) {
  Compiler c             = MakeChild(resources_);
  c.state_.must_complete = must_complete;
  auto thunk             = MakeThunk(c, *expr, expr.type());
  c.CompleteWorkQueue();
  c.CompleteDeferredBodies();
  return interpreter::Evaluate<instruction_set_t>(
      std::move(thunk));
}

base::untyped_buffer Compiler::EvaluateToBufferOrDiagnose(
    type::Typed<ast::Expression const *> expr) {
  // TODO: The diagnosis part.
  Compiler c = MakeChild(resources_);
  auto thunk = MakeThunk(c, *expr, expr.type());
  c.CompleteWorkQueue();
  c.CompleteDeferredBodies();
  return interpreter::EvaluateToBuffer<instruction_set_t>(
      std::move(thunk));
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
  auto &ctx = node->scope()
                  ->Containing<ast::ModuleScope>()
                  ->module()
                  ->as<CompiledModule>()
                  .context();
  LOG("Instantiate", "Instantiating %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad = ctx.ScratchpadSubcontext();
  Compiler c({
      .data                = scratchpad,
      .diagnostic_consumer = diag(),
      .importer            = importer(),
  });

  return ctx.InsertSubcontext(node, c.ComputeParamsFromArgs(node, args),
                              std::move(scratchpad));
}

Context::FindSubcontextResult Compiler::FindInstantiation(
    ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  auto &ctx = node->scope()
                  ->Containing<ast::ModuleScope>()
                  ->module()
                  ->as<CompiledModule>()
                  .context();
  LOG("FindInstantiation", "Finding %s: %s", node->DebugString(),
      ctx.DebugString());
  Context scratchpad = ctx.ScratchpadSubcontext();
  Compiler c({
      .data                = scratchpad,
      .diagnostic_consumer = diag(),
      .importer            = importer(),
  });

  return ctx.FindSubcontext(node, c.ComputeParamsFromArgs(node, args));
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
