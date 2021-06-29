#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "core/arguments.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::FunctionLiteral const *node,
                            base::untyped_buffer &out) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = Compiler(resources()),
         node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn {
          auto find_subcontext_result = c.FindInstantiation(node, args);
          auto &context               = find_subcontext_result.context;

          auto [f, inserted] = context.add_func(node);
          Compiler compiler({
              .data                = context,
              .diagnostic_consumer = c.diag(),
              .importer            = c.importer(),
          });
          if (inserted) {
            compiler.Enqueue({.kind      = WorkItem::Kind::EmitFunctionBody,
                              .node      = node,
                              .resources = compiler.resources()});
          }

          compiler.CompleteWorkQueue();
          compiler.CompleteDeferredBodies();

          return f;
        });
    out.append(ir::RegOr<ir::GenericFn>(gen_fn));
    return;
  }

  // TODO: Check the result of body verification.
  // TODO: Check for whether or not we actually need to do the verification is
  // handled inside VerifyBody, at least for now.
  VerifyBody(node);

  // TODO Use correct constants
  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    Enqueue({.kind      = WorkItem::Kind::EmitFunctionBody,
             .node      = node,
             .resources = resources_});
  }
  out.append(ir::RegOr<ir::Fn>(f));
  return;
}

void Compiler::EmitMoveInit(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitValue(node).get<ir::RegOr<ir::Fn>>(), *to[0]);
}

void Compiler::EmitCopyInit(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitValue(node).get<ir::RegOr<ir::Fn>>(), *to[0]);
}

void Compiler::EmitMoveAssign(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitValue(node).get<ir::RegOr<ir::Fn>>(), *to[0]);
}

void Compiler::EmitCopyAssign(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitValue(node).get<ir::RegOr<ir::Fn>>(), *to[0]);
}

WorkItem::Result Compiler::EmitFunctionBody(ast::FunctionLiteral const *node) {
  LOG("EmitFunctionBody", "%s", node->DebugString());

  ir::NativeFn ir_func = context().FindNativeFn(node);
  ASSERT(static_cast<bool>(ir_func) == true);

  ICARUS_SCOPE(ir::SetCurrent(ir_func, builder())) {
    builder().CurrentBlock() = builder().CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      absl::Span<ast::Declaration::Id const> ids = param.value->ids();
      ASSERT(ids.size() == 1u);
      builder().set_addr(&ids[0], ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*this, &node->body_scope());
    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        type::Type out_decl_type = context().qual_types(out_decl)[0].type();
        auto alloc               = out_decl_type.is_big() ? ir::Reg::Out(i)
                                            : builder().Alloca(out_decl_type);

        ASSERT(out_decl->ids().size() == 1u);
        builder().set_addr(&out_decl->ids()[0], alloc);
        if (out_decl->IsDefaultInitialized()) {
          EmitDefaultInit(type::Typed<ir::Reg>(alloc, out_decl_type));
        } else {
          EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>>(alloc, out_decl_type),
                         type::Typed<ir::Value>(EmitValue(out_decl->init_val()),
                                                out_decl_type));
        }
      }
    }

    EmitIrForStatements(*this, node->stmts());
    if (builder().block_termination_state() !=
        ir::Builder::BlockTerminationState::kReturn) {
      MakeAllDestructions(*this, &node->body_scope());
      builder().ReturnJump();
    }
  }

  context().WriteByteCode(ir_func);
  return WorkItem::Result::Success;
}

}  // namespace compiler
