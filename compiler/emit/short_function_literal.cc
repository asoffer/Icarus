#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/scaffolding.h"
#include "compiler/instantiate.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "core/arguments.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ShortFunctionLiteral const *node,
                            ir::PartialResultBuffer &out) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [node, data = this->data()](
            WorkResources const &wr,
            core::Arguments<type::Typed<ir::CompleteResultRef>> const
                &args) mutable -> ir::Fn {
          Compiler c(&data);
          c.set_work_resources(wr);
          auto find_subcontext_result = FindInstantiation(c, node, args);
          auto &context               = find_subcontext_result.context;

          auto [placeholder, inserted] = context.MakePlaceholder(node);

          PersistentResources resources = c.resources();
          if (inserted) {
            c.Enqueue({.kind    = WorkItem::Kind::EmitShortFunctionBody,
                       .node    = node,
                       .context = &context},
                      {});
          }

          return ir::Fn(resources.module->id(), placeholder);
        });
    out.append(gen_fn);
    return;
  }

  auto [placeholder, inserted] = context().MakePlaceholder(node);
  if (inserted) {
    Enqueue({.kind    = WorkItem::Kind::EmitShortFunctionBody,
             .node    = node,
             .context = &context()});
  }
  out.append(ir::Fn(resources().module->id(), placeholder));
  return;
}

void Compiler::EmitMoveInit(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) {
    current_block()->Append(ir::StoreInstruction<ir::GenericFn>{
        .value    = EmitAs<ir::GenericFn>(node),
        .location = *to[0],
    });
  } else {
    current_block()->Append(ir::StoreInstruction<ir::Fn>{
        .value    = EmitAs<ir::Fn>(node),
        .location = *to[0],
    });
  }
}

void Compiler::EmitCopyInit(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) {
    current_block()->Append(ir::StoreInstruction<ir::GenericFn>{
        .value    = EmitAs<ir::GenericFn>(node),
        .location = *to[0],
    });
  } else {
    current_block()->Append(ir::StoreInstruction<ir::Fn>{
        .value    = EmitAs<ir::Fn>(node),
        .location = *to[0],
    });
  }
}

void Compiler::EmitMoveAssign(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) {
    current_block()->Append(ir::StoreInstruction<ir::GenericFn>{
        .value    = EmitAs<ir::GenericFn>(node),
        .location = *to[0],
    });
  } else {
    current_block()->Append(ir::StoreInstruction<ir::Fn>{
        .value    = EmitAs<ir::Fn>(node),
        .location = *to[0],
    });
  }
}

void Compiler::EmitCopyAssign(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) {
    current_block()->Append(ir::StoreInstruction<ir::GenericFn>{
        .value    = EmitAs<ir::GenericFn>(node),
        .location = *to[0],
    });
  } else {
    current_block()->Append(ir::StoreInstruction<ir::Fn>{
        .value    = EmitAs<ir::Fn>(node),
        .location = *to[0],
    });
  }
}

bool Compiler::EmitShortFunctionBody(ast::ShortFunctionLiteral const *node) {
  type::Function const *fn_type =
      &context().qual_types(node)[0].type().as<type::Function>();
  ir::Subroutine subroutine(fn_type);

  {
    push_current(&subroutine);
    absl::Cleanup c = [&] { state().current.pop_back(); };
    auto cleanup    = EmitScaffolding(*this, subroutine, node->body_scope());

    current_block() = subroutine.entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->parameters()) {
      absl::Span<ast::Declaration::Id const> ids = param.value.ids();
      ASSERT(ids.size() == 1u);
      state().set_addr(&ids[0], ir::Reg::Parameter(i++));
    }

    type::Type ret_type = fn_type->return_types()[0];
    type::Typed<ir::RegOr<ir::addr_t>> typed_alloc(
        ir::RegOr<ir::addr_t>(ir::Reg::Output(0)), ret_type);
    EmitMoveInit(node->body(), absl::MakeConstSpan(&typed_alloc, 1));

    DestroyTemporaries();

    current_block()->set_jump(ir::JumpCmd::Return());
  }
  ir::LocalFnId placeholder = context().Placeholder(node);
  context().ir().Insert(placeholder, std::move(subroutine));
  return true;
}

}  // namespace compiler
