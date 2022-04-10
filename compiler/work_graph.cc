#include "compiler/work_graph.h"

#include "compiler/emit/initialize.h"
#include "compiler/emit/scaffolding.h"
#include "compiler/instructions.h"
#include "compiler/verify/verify.h"
#include "ir/value/native_fn.h"

namespace compiler {
namespace {

ir::NativeFunctionInformation MakeNativeFunctionInformation(
    Compiler &c, ast::Expression const *expr, type::Type type) {
  LOG("MakeNativeFunctionInformation",
      "NativeFunctionInformation for %s: %s %p", expr->DebugString(),
      type.to_string(), &c.context());
  ir::Subroutine fn(type::Func({}, {type}));
  c.push_current(&fn);
  absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
  c.current_block()     = fn.entry();

  ir::PartialResultBuffer buffer;
  c.EmitToBuffer(expr, buffer);

  if (type.is_big()) {
    ASSERT(buffer.num_entries() != 0);
    // TODO: guaranteed move-elision
    MoveInitializationEmitter emitter(c);
    emitter(type, ir::Reg::Output(0), buffer);
  } else {
    ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
               uint16_t, uint32_t, uint64_t, float, double, type::Type,
               ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn, ir::GenericFn,
               ir::UnboundScope, ir::ScopeContext, ir::Block,
               interface::Interface>(type, [&]<typename T>() {
      c.current_block()->Append(ir::SetReturnInstruction<T>{
          .index = 0,
          .value = buffer.get<T>(0),
      });
    });
  }
  c.current_block()->set_jump(ir::JumpCmd::Return());
  LOG("MakeNativeFunctionInformation", "%s", fn);

  auto byte_code = EmitByteCode(fn);

  return ir::NativeFunctionInformation{.fn        = std::move(fn),
                                       .byte_code = std::move(byte_code)};
}

}  // namespace

bool IsConstantDeclaration(ast::Node const *n) {
  auto const *decl = n->if_as<ast::Declaration>();
  if (not decl) { return false; }
  return (decl->flags() & ast::Declaration::f_IsConst);
}

bool IsNotConstantDeclaration(ast::Node const *n) {
  return not IsConstantDeclaration(n);
}

bool VerifyNodesSatisfying(std::predicate<ast::Node const *> auto &&predicate,
                           Context &context, WorkGraph &work_graph,
                           base::PtrSpan<ast::Node const> nodes,
                           bool stop_on_first_error) {
  CompilationData data{.context        = &context,
                       .work_resources = work_graph.work_resources(),
                       .resources      = work_graph.resources()};
  Compiler c(&data);

  bool found_error = false;
  for (ast::Node const *node : nodes) {
    if (not predicate(node)) { continue; }
    auto qts = VerifyType(c, node);
    for (auto const &qt : qts) {
      if (qt.HasErrorMark()) {
        found_error = true;
        if (stop_on_first_error) { return false; }
      }
    }
  }
  return not found_error;
}

std::optional<ir::Subroutine> CompileModule(
    Context &context, PersistentResources const &resources,
    base::PtrSpan<ast::Node const> nodes) {
  WorkGraph w(resources);
  ir::Subroutine f(type::Func({core::AnonymousParameter(type::QualType::NonConstant(
                                  type::Slc(type::Slc(type::Char))))},
                              {}));

  bool success = w.ExecuteCompilationSequence(
      context, nodes,
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        size_t error_count = w.resources().diagnostic_consumer->num_consumed();
        if (not VerifyNodesSatisfying(IsConstantDeclaration, context, w, nodes,
                                      true)) {
          return false;
        }
        if (not VerifyNodesSatisfying(IsNotConstantDeclaration, context, w,
                                      nodes)) {
          return false;
        }

        return error_count == w.resources().diagnostic_consumer->num_consumed();
      },
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        CompilationData data{.context        = &context,
                             .work_resources = w.work_resources(),
                             .resources      = w.resources()};
        Compiler c(&data);
        ast::Scope const &mod_scope = w.resources().module->scope();

        c.push_current(&f);
        auto scaffolding_cleanup = EmitScaffolding(c, f, mod_scope);
        absl::Cleanup cleanup    = [&] { c.state().current.pop_back(); };
        if (not nodes.empty()) { EmitIrForStatements(c, &mod_scope, nodes); }
        c.current_block()->set_jump(ir::JumpCmd::Return());

        return true;
      });
  if (not success) { return std::nullopt; }
  return f;
}

bool WorkGraph::Execute(WorkItem const &w) {
  if (auto dep_iter = dependencies_.find(w); dep_iter != dependencies_.end()) {
    absl::Cleanup c = [&] { dependencies_.erase(w); };
    auto deps       = std::move(dep_iter->second);
    for (auto const &n : deps) {
      if (not Execute(n)) { return false; }
    }
  }

  if (resources_.work->AddWorkItem(w)) {
    LOG("WorkGraph", "Ignoring work %u on %s", (int)w.kind,
        w.node->DebugString());
    return true;
  }

  CompilationData data{.context        = w.context,
                       .work_resources = work_resources(),
                       .resources      = resources_};
  Compiler c(&data);

  bool result;
  LOG("WorkGraph", "Starting work %u on %s (%p)", (int)w.kind,
      w.node->DebugString(), this);
  switch (w.kind) {
    case WorkItem::Kind::VerifyType:
      VerifyType(c, w.node);
      result = resources().diagnostic_consumer->num_consumed() == 0;
      break;
    case WorkItem::Kind::VerifyEnumBody:
      result = VerifyBody(c, &w.node->as<ast::EnumLiteral>());
      break;
    case WorkItem::Kind::VerifyFunctionBody:
      result = not w.context->ClaimVerifyBodyTask(
                   &w.node->as<ast::FunctionLiteral>()) or
               VerifyBody(c, &w.node->as<ast::FunctionLiteral>());
      break;
    case WorkItem::Kind::VerifyStructBody:
      result = VerifyBody(c, &w.node->as<ast::StructLiteral>());
      break;
    case WorkItem::Kind::CompleteStructData:
      if (auto const *s = w.node->if_as<ast::StructLiteral>()) {
        result = Execute({.kind    = WorkItem::Kind::VerifyStructBody,
                          .node    = w.node,
                          .context = w.context}) and
                 CompleteStructData(c, s);
      } else {
        result = Execute({.kind    = WorkItem::Kind::VerifyStructBody,
                          .node    = w.node,
                          .context = w.context}) and
                 CompleteStructData(
                     c, &w.node->as<ast::ParameterizedStructLiteral>());
      }
      break;
    case WorkItem::Kind::CompleteStruct:
      if (auto const *s = w.node->if_as<ast::StructLiteral>()) {
        result = Execute({.kind    = WorkItem::Kind::CompleteStructData,
                          .node    = w.node,
                          .context = w.context}) and
                 c.CompleteStruct(s);
      } else {
        result =
            Execute({.kind    = WorkItem::Kind::CompleteStructData,
                     .node    = w.node,
                     .context = w.context}) and
            c.CompleteStruct(&w.node->as<ast::ParameterizedStructLiteral>());
      }
      break;
    case WorkItem::Kind::CompleteEnum:
      result = c.CompleteEnum(&w.node->as<ast::EnumLiteral>());
      break;
    case WorkItem::Kind::VerifyParameterizedStructLiteralBody:
      result = VerifyBody(c, &w.node->as<ast::ParameterizedStructLiteral>());
      break;
    case WorkItem::Kind::EmitParameterizedStructFunction:
      result =
          Execute({.kind = WorkItem::Kind::VerifyParameterizedStructLiteralBody,
                   .node = w.node,
                   .context = w.context}) and
          c.EmitParameterizedStructFunctionBody(
              &w.node->as<ast::ParameterizedStructLiteral>());
      break;
    case WorkItem::Kind::EmitScopeBody:
      result = c.EmitScopeBody(&w.node->as<ast::ScopeLiteral>());
      break;
    case WorkItem::Kind::EmitFunctionBody:
      result = Execute({.kind    = WorkItem::Kind::VerifyFunctionBody,
                        .node    = w.node,
                        .context = w.context}) and
               c.EmitFunctionBody(&w.node->as<ast::FunctionLiteral>());
      break;
    case WorkItem::Kind::EmitShortFunctionBody:
      result =
          c.EmitShortFunctionBody(&w.node->as<ast::ShortFunctionLiteral>());
      break;
  }
  if (result) { resources_.work->MarkAsComplete(w); }
  LOG("WorkGraph", "Ending work %u on %s (result = %s)", (int)w.kind,
      w.node->DebugString(), result ? "true" : "false");
  return result;
}

// Factor out the common bits of these two functions.
std::variant<ir::CompleteResultBuffer, std::vector<diagnostic::ConsumedMessage>>
WorkGraph::EvaluateToBuffer(module::SharedContext const &shared_context,
                            Context &context,
                            type::Typed<ast::Expression const *> expr) {
  if (auto qt = context.qual_types(*expr)[0];
      qt == type::QualType::Error() or qt.HasErrorMark()) {
    // TODO: Give some explanation about failing to evaluate due to preexisting
    // errors. This probably shouldn't be an error itself.
    return std::vector<diagnostic::ConsumedMessage>{};
  }

  diagnostic::BufferingConsumer buffering_consumer(
      resources().diagnostic_consumer);
  auto r                = resources();
  r.diagnostic_consumer = &buffering_consumer;
  WorkGraph w(r);

  CompilationData compilation_data{.context        = &context,
                                   .work_resources = w.work_resources(),
                                   .resources      = w.resources()};
  Compiler c(&compilation_data);
  c.state().scaffolding.emplace_back();
  absl::Cleanup cleanup = [&] { c.state().scaffolding.pop_back(); };

  ir::NativeFunctionInformation info =
      MakeNativeFunctionInformation(c, *expr, expr.type());

  for (auto const &[item, deps] : w.dependencies_) {
    if (item.kind == WorkItem::Kind::EmitFunctionBody or
        item.kind == WorkItem::Kind::EmitParameterizedStructFunction) {
      Execute(item);
    }
  }

  // Anything that hasn't been completed gets shunted to the parent.
  for (auto &[item, deps] : w.dependencies_) { emplace(item, std::move(deps)); }
  w.dependencies_.clear();

  if (buffering_consumer.empty()) {
    return EvaluateAtCompileTimeToBuffer(shared_context, ir::NativeFn(&info));
  } else {
    return std::move(buffering_consumer).extract();
  }
}

}  // namespace compiler
