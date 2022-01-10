#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instantiate.h"
#include "compiler/instructions.h"
#include "core/arguments.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::FunctionLiteral const *node,
                            ir::PartialResultBuffer &out) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [node, data = this->data()](
            WorkResources const &wr,
            core::Arguments<type::Typed<ir::CompleteResultRef>> const
                &args) mutable -> ir::NativeFn {
          Compiler c(&data);
          c.set_work_resources(wr);
          auto find_subcontext_result = FindInstantiation(c, node, args);
          auto &context               = find_subcontext_result.context;

          auto [f, inserted] = context.add_func(node);

          PersistentResources resources = c.resources();
          if (inserted) {
            c.Enqueue({.kind    = WorkItem::Kind::EmitFunctionBody,
                       .node    = node,
                       .context = &context});
          }

          return f;
        });
    out.append(gen_fn);
    return;
  }

  // TODO Use correct constants
  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    Enqueue({.kind    = WorkItem::Kind::EmitFunctionBody,
             .node    = node,
             .context = &context()});
  }
  out.append(ir::Fn(f));
  return;
}

void Compiler::EmitMoveInit(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

void Compiler::EmitCopyInit(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

void Compiler::EmitMoveAssign(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

void Compiler::EmitCopyAssign(
    ast::FunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

bool Compiler::EmitFunctionBody(ast::FunctionLiteral const *node) {
  LOG("EmitFunctionBody", "%s", node->DebugString());

  ir::NativeFn ir_func = context().FindNativeFn(node);
  ASSERT(static_cast<bool>(ir_func) == true);

  ICARUS_SCOPE(SetCurrent(ir_func, builder())) {
    builder().CurrentBlock() = builder().CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      absl::Span<ast::Declaration::Id const> ids = param.value->ids();
      ASSERT(ids.size() == 1u);
      state().set_addr(&ids[0], ir::Reg::Arg(i++));
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
        state().set_addr(&out_decl->ids()[0], alloc);
        if (out_decl->IsDefaultInitialized()) {
          EmitDefaultInit(type::Typed<ir::Reg>(alloc, out_decl_type));
        } else {
          ir::PartialResultBuffer buffer;
          EmitToBuffer(out_decl->init_val(), buffer);
          EmitCopyAssign(
              type::Typed<ir::RegOr<ir::addr_t>>(alloc, out_decl_type),
              type::Typed(buffer[0], out_decl_type));
        }
      }
    }

    EmitIrForStatements(*this, node->stmts());
    MakeAllDestructions(*this, &node->body_scope());
    builder().ReturnJump();
  }

  context().ir().WriteByteCode<EmitByteCode>(ir_func);
  return true;
}

}  // namespace compiler
