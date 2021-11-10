#include "compiler/work_graph.h"
#include "compiler/instructions.h"
#include "ir/builder.h"

namespace compiler {
namespace {

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
                           bool stop_on_first_error = false) {
  Compiler c(&context, work_graph.resources());
  c.set_work_resources(work_graph.work_resources());

  bool found_error = false;
  for (ast::Node const *node : nodes) {
    if (not predicate(node)) { continue; }
    auto qts = c.VerifyType(node);
    for (auto const &qt : qts) {
      if (qt.HasErrorMark()) {
        found_error = true;
        if (stop_on_first_error) { return false; }
      }
    }
  }
  return not found_error;
}

std::pair<ir::CompiledFn, ir::ByteCode> MakeThunk(Compiler &c,
                                                  ast::Expression const *expr,
                                                  type::Type type) {
  LOG("MakeThunk", "Thunk for %s: %s %p", expr->DebugString(), type.to_string(), &c.context());
  ir::CompiledFn fn(type::Func({}, {type}));
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    // TODO this is essentially a copy of the body of
    // FunctionLiteral::EmitToBuffer Factor these out together.
    c.builder().CurrentBlock() = fn.entry();

    ir::PartialResultBuffer buffer;
    c.EmitToBuffer(expr, buffer);

    // TODO: Treating slices specially is a big hack. We need to fix treating
    // these things special just because they're big.
    if (type.is_big()) {
      ASSERT(buffer.num_entries() != 0);
      // TODO: guaranteed move-elision
      c.EmitMoveInit(type::Typed<ir::Reg>(ir::Reg::Out(0), type), buffer);
    } else {
      ApplyTypes<bool, ir::Char, ir::Integer, int8_t, int16_t, int32_t, int64_t,
                 uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                 type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                 ir::Jump, ir::Block, ir::GenericFn, interface::Interface>(
          type, [&]<typename T>() {
            c.builder().CurrentBlock()->Append(ir::SetReturnInstruction<T>{
                .index = 0,
                .value = buffer.get<T>(0),
            });
          });
    }
    c.builder().ReturnJump();
  }
  LOG("MakeThunk", "%s", fn);

  return std::pair(std::move(fn), EmitByteCode(fn));
}

}  // namespace

bool CompileLibrary(Context &context, PersistentResources const &resources,
                    base::PtrSpan<ast::Node const> nodes) {
  WorkGraph w(resources);
  return w.ExecuteCompilationSequence(
      context, nodes,
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        if (not VerifyNodesSatisfying(IsConstantDeclaration, context, w, nodes,
                                      true)) {
          return false;
        }
        return VerifyNodesSatisfying(IsNotConstantDeclaration, context, w,
                                     nodes);
      },
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        for (auto const *node : nodes) {
          Compiler c(&context, w.resources());
          c.set_work_resources(w.work_resources());
          c.EmitVoid(node);
        }
        return true;
      });
}

std::optional<ir::CompiledFn> CompileExecutable(
    Context &context, PersistentResources const &resources,
    base::PtrSpan<ast::Node const> nodes) {
  WorkGraph w(resources);
  ir::CompiledFn f = ir::CompiledFn(type::Func({}, {}), {});

  bool success = w.ExecuteCompilationSequence(
      context, nodes,
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        if (not VerifyNodesSatisfying(IsConstantDeclaration, context, w,
                                      nodes, true)) {
          return false;
        }
        return VerifyNodesSatisfying(IsNotConstantDeclaration, context, w,
                                     nodes);
      },
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        Compiler c(&context, w.resources());
        c.set_work_resources(w.work_resources());
        ICARUS_SCOPE(ir::SetCurrent(f, c.builder())) {
          if (nodes.empty()) {
            EmitIrForStatements(c, nodes);
          } else {
            ast::ModuleScope const &mod_scope = w.resources().module->scope();
            MakeAllStackAllocations(c, &mod_scope);
            EmitIrForStatements(c, nodes);
            MakeAllDestructions(c, &mod_scope);
            // TODO determine under which scenarios destructors can be skipped.
          }
          c.builder().ReturnJump();
        }
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

  Compiler c(w.context, resources_);
  c.set_work_resources(work_resources());
  bool result;
  LOG("WorkGraph", "Starting work %u on %s", (int)w.kind, w.node->DebugString());
  switch (w.kind) {
    case WorkItem::Kind::VerifyType:
      c.VerifyType(w.node);
      result = resources().diagnostic_consumer->num_consumed() == 0;
      break;
    case WorkItem::Kind::VerifyDesignatedInitializerBody:
      result = c.VerifyBody(&w.node->as<ast::DesignatedInitializer>());
      break;
    case WorkItem::Kind::VerifyEnumBody:
      result = c.VerifyBody(&w.node->as<ast::EnumLiteral>());
      break;
    case WorkItem::Kind::VerifyFunctionBody:
      result = c.VerifyBody(&w.node->as<ast::FunctionLiteral>());
      break;
    case WorkItem::Kind::VerifyStructBody:
      result = c.VerifyBody(&w.node->as<ast::StructLiteral>());
      break;
    case WorkItem::Kind::CompleteStructMembers:
      result = c.CompleteStruct(&w.node->as<ast::StructLiteral>());
      break;
    case WorkItem::Kind::CompleteEnum:
      result = c.CompleteEnum(&w.node->as<ast::EnumLiteral>());
      break;
    case WorkItem::Kind::EmitJumpBody:
      result = c.EmitJumpBody(&w.node->as<ast::Jump>());
      break;
    case WorkItem::Kind::EmitFunctionBody:
      result = c.EmitFunctionBody(&w.node->as<ast::FunctionLiteral>());
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

std::variant<ir::CompleteResultBuffer, std::vector<diagnostic::ConsumedMessage>>
WorkGraph::EvaluateToBuffer(Context &context,
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

  Compiler c(&context, w.resources());
  c.set_work_resources(w.work_resources());

  auto [thunk, byte_code] = MakeThunk(c, *expr, expr.type());
  ir::NativeFn::Data data{
      .fn        = &thunk,
      .type      = thunk.type(),
      .byte_code = byte_code.begin(),
  };

  w.complete();

  if (buffering_consumer.empty()) {
    return EvaluateAtCompileTimeToBuffer(ir::NativeFn(&data));
  } else {
    return std::move(buffering_consumer).extract();
  }
}

}  // namespace compiler
