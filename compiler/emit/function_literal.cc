#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "compiler/emit/scaffolding.h"
#include "compiler/instantiate.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
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
                &args) mutable -> ir::Fn {
          Compiler c(&data);
          c.set_work_resources(wr);
          auto find_subcontext_result = FindInstantiation(c, node, args);
          auto &context               = find_subcontext_result.context;

          auto [placeholder, inserted] = context.MakePlaceholder(node);

          PersistentResources resources = c.resources();
          if (inserted) {
            c.Enqueue({.kind    = WorkItem::Kind::EmitFunctionBody,
                       .node    = node,
                       .context = &context});
          }

          return ir::Fn(resources.module->id(), placeholder);
        });
    out.append(gen_fn);
    return;
  }

  auto [placeholder, inserted] = context().MakePlaceholder(node);
  if (inserted) {
    Enqueue({.kind    = WorkItem::Kind::EmitFunctionBody,
             .node    = node,
             .context = &context()});
  }
  out.append(ir::Fn(resources().module->id(), placeholder));
  return;
}

void Compiler::EmitMoveInit(
    ast::FunctionLiteral const *node,
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
    ast::FunctionLiteral const *node,
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
    ast::FunctionLiteral const *node,
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
    ast::FunctionLiteral const *node,
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

// TODO: Parameters should be renumbered to not waste space on const values
bool Compiler::EmitFunctionBody(ast::FunctionLiteral const *node) {
  LOG("EmitFunctionBody", "%s", node->DebugString());

  ir::Subroutine subroutine(
      &context().qual_types(node)[0].type().as<type::Function>());

  {
    push_current(&subroutine);
    absl::Cleanup c = [&] { state().current.pop_back(); };
    auto cleanup    = EmitScaffolding(*this, subroutine, node->body_scope());

    size_t i = 0;
    for (auto const &param : node->parameters()) {
      absl::Span<ast::Declaration::Id const> ids = param.value.ids();
      ASSERT(ids.size() == 1u);
      state().set_addr(&ids[0], ir::Reg::Parameter(i++));
    }

    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        type::Type out_decl_type = context().qual_types(out_decl)[0].type();
        auto alloc               = out_decl_type.is_big() ? ir::Reg::Output(i)
                                            : subroutine.Alloca(out_decl_type);

        ASSERT(out_decl->ids().size() == 1u);
        state().set_addr(&out_decl->ids()[0], alloc);
        if (out_decl->IsDefaultInitialized()) {
          DefaultInitializationEmitter emitter(*this);
          emitter(out_decl_type, alloc);
        } else {
          ir::PartialResultBuffer buffer;
          EmitToBuffer(out_decl->init_val(), buffer);
          CopyAssignmentEmitter emitter(*this);
          emitter(out_decl_type, alloc, type::Typed(buffer[0], out_decl_type));
        }
      }
    }

    EmitIrForStatements(*this, &node->body_scope(), node->stmts());
    current_block()->set_jump(ir::JumpCmd::Return());

    LOG("EmitFunctionBody", "%s", subroutine);
  }
  ir::LocalFnId placeholder = context().Placeholder(node);
  context().ir().Insert(placeholder, std::move(subroutine));
  return true;
}

}  // namespace compiler
