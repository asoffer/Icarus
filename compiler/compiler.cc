#include "compiler/compiler.h"

#include "ast/ast.h"
#include "base/log.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "diagnostic/consumer/buffering.h"
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

static std::pair<ir::CompiledFn, base::untyped_buffer> MakeThunk(
    Compiler &c, ast::Expression const *expr, type::Type type) {
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

    if (type.is_big()) {
      // TODO must `r` be holding a register?
      // TODO guaranteed move-elision

      c.EmitMoveInit(type::Typed<ir::Reg>(ir::Reg::Out(0), type),
                     type::Typed<ir::Value>(val, type));

    } else if (auto const *gs = type.if_as<type::GenericStruct>()) {
      c.builder().CurrentBlock()->Append(ir::SetReturnInstruction<type::Type>{
          .index = 0,
          .value = ir::RegOr<type::Type>(gs),
      });
    } else {
      ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double, type::Type,
                 ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn, ir::Jump, ir::Block,
                 ir::GenericFn, interface::Interface>(type, [&]<typename T>() {
        c.builder().CurrentBlock()->Append(ir::SetReturnInstruction<T>{
            .index = 0,
            .value = val.get<ir::RegOr<T>>(),
        });
      });
    }
    c.builder().ReturnJump();
  }
  LOG("MakeThunk", "%s", fn);

  return std::pair<ir::CompiledFn, base::untyped_buffer>(std::move(fn),
                                                         EmitByteCode(fn));
}

interpreter::EvaluationResult Compiler::Evaluate(
    type::Typed<ast::Expression const *> expr, bool must_complete) {
  Compiler c             = MakeChild(resources_);
  c.state_.must_complete  = must_complete;
  auto [thunk, byte_code] = MakeThunk(c, *expr, expr.type());
  ir::NativeFn::Data data{
      .fn        = &thunk,
      .type      = thunk.type(),
      .byte_code = byte_code.begin(),
  };
  c.CompleteWorkQueue();
  c.CompleteDeferredBodies();
  return EvaluateAtCompileTime(ir::NativeFn(&data));
}

std::variant<base::untyped_buffer, std::vector<diagnostic::ConsumedMessage>>
Compiler::EvaluateToBufferOrDiagnose(
    type::Typed<ast::Expression const *> expr, bool must_complete) {
  // TODO: The diagnosis part.
  diagnostic::BufferingConsumer buffering_consumer(&diag());
  Compiler c              = MakeChild(PersistentResources{
      .data                = context(),
      .diagnostic_consumer = buffering_consumer,
      .importer            = importer(),
  });
  c.state_.must_complete  = must_complete;

  auto [thunk, byte_code] = MakeThunk(c, *expr, expr.type());
  ir::NativeFn::Data data{
      .fn        = &thunk,
      .type      = thunk.type(),
      .byte_code = byte_code.begin(),
  };
  c.CompleteWorkQueue();
  c.CompleteDeferredBodies();
  if (buffering_consumer.empty()) {
    return EvaluateAtCompileTimeToBuffer(ir::NativeFn(&data));
  } else {
    return std::move(buffering_consumer).extract();
  }
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
                  .context(&context().module());
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
                  .context(&context().module());
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
