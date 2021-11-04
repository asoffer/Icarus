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

void VerifyNodesSatisfying(std::predicate<ast::Node const *> auto &&predicate,
                           Context &context, WorkGraph &work_graph,
                           base::PtrSpan<ast::Node const> nodes) {
  Compiler c(&context, work_graph.resources());
  c.set_work_resources(work_graph.work_resources());

  for (ast::Node const *node : nodes) {
    if (not predicate(node)) { continue; }
    c.VerifyType(node);
  }
}

std::pair<ir::CompiledFn, ir::ByteCode> MakeThunk(Compiler &c,
                                                  ast::Expression const *expr,
                                                  type::Type type) {
  LOG("MakeThunk", "Thunk for %s: %s", expr->DebugString(), type.to_string());
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

void CompileLibrary(Context &context, PersistentResources const &resources,
                    base::PtrSpan<ast::Node const> nodes) {
  WorkGraph w(resources);
  w.ExecuteCompilationSequence(
      context, nodes,
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        VerifyNodesSatisfying(IsConstantDeclaration, context, w, nodes);
        VerifyNodesSatisfying(IsNotConstantDeclaration, context, w, nodes);
      },
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        for (auto const *node : nodes) {
          Compiler c(&context, w.resources());
          c.set_work_resources(w.work_resources());
          c.EmitVoid(node);
        }
      });
}

ir::CompiledFn CompileExecutable(Context &context,
                                 PersistentResources const &resources,
                                 base::PtrSpan<ast::Node const> nodes) {
  WorkGraph w(resources);
  ir::CompiledFn f = ir::CompiledFn(type::Func({}, {}), {});

  w.ExecuteCompilationSequence(
      context, nodes,
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        VerifyNodesSatisfying(IsConstantDeclaration, context, w, nodes);
        VerifyNodesSatisfying(IsNotConstantDeclaration, context, w, nodes);
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
      });
  return f;
}

bool WorkGraph::Execute(WorkItem const &w) {
  auto dep_iter = dependencies_.find(w);
  if (dep_iter != dependencies_.end()) {
    auto nh = dependencies_.extract(dep_iter);
    for (auto const &n : nh.mapped()) {
      if (not Execute(n)) {
        work_[w] = false;
        return false;
      }
    }
  }

  auto [work_iter, inserted] = work_.try_emplace(w);
  if (not inserted) { return work_iter->second; }
  Compiler c(w.context, resources_);
  c.set_work_resources(work_resources());
  work_iter->second = [&] {
    switch (w.kind) {
      case WorkItem::Kind::VerifyType:
        c.VerifyType(w.node);
        return resources().diagnostic_consumer->num_consumed() == 0;
      case WorkItem::Kind::VerifyEnumBody:
        return c.VerifyBody(&w.node->as<ast::EnumLiteral>());
      case WorkItem::Kind::VerifyFunctionBody:
        return c.VerifyBody(&w.node->as<ast::FunctionLiteral>());
      case WorkItem::Kind::VerifyStructBody:
        return c.VerifyBody(&w.node->as<ast::StructLiteral>());
      case WorkItem::Kind::CompleteStructMembers:
        return c.CompleteStruct(&w.node->as<ast::StructLiteral>());
      case WorkItem::Kind::EmitJumpBody:
        return c.EmitJumpBody(&w.node->as<ast::Jump>());
      case WorkItem::Kind::EmitFunctionBody:
        return c.EmitFunctionBody(&w.node->as<ast::FunctionLiteral>());
      case WorkItem::Kind::EmitShortFunctionBody:
        return c.EmitShortFunctionBody(
            &w.node->as<ast::ShortFunctionLiteral>());
    }
  }();
  return work_iter->second;
}

std::variant<ir::CompleteResultBuffer, std::vector<diagnostic::ConsumedMessage>>
WorkGraph::EvaluateToBuffer(Context &context,
                            type::Typed<ast::Expression const *> expr,
                            bool must_complete) {
  // TODO: The diagnosis part.
  diagnostic::BufferingConsumer buffering_consumer(
      resources().diagnostic_consumer);
  auto r                = resources();
  r.diagnostic_consumer = &buffering_consumer;
  WorkGraph w(r);

  Compiler c(&context, w.resources());
  c.set_work_resources(w.work_resources());

  // c.state_.must_complete = must_complete;

  auto [thunk, byte_code] = MakeThunk(c, *expr, expr.type());
  ir::NativeFn::Data data{
      .fn        = &thunk,
      .type      = thunk.type(),
      .byte_code = byte_code.begin(),
  };

  w.complete();

  if (buffering_consumer.empty()) {
    auto result = EvaluateAtCompileTimeToBuffer(ir::NativeFn(&data));
    w.complete();
    return result;
  } else {
    return std::move(buffering_consumer).extract();
  }
}

}  // namespace compiler
