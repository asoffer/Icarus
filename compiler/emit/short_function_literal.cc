#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/scaffolding.h"
#include "compiler/instantiate.h"
#include "compiler/instructions.h"
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
                &args) mutable -> ir::NativeFn {
          Compiler c(&data);
          c.set_work_resources(wr);
          auto find_subcontext_result = FindInstantiation(c, node, args);
          auto &context               = find_subcontext_result.context;

          auto [f, inserted]            = context.add_func(node);
          PersistentResources resources = c.resources();
          if (inserted) {
            c.Enqueue({.kind    = WorkItem::Kind::EmitShortFunctionBody,
                       .node    = node,
                       .context = &context},
                      {});
          }

          return f;
        });
    out.append(gen_fn);
    return;
  }

  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    Enqueue({.kind    = WorkItem::Kind::EmitShortFunctionBody,
             .node    = node,
             .context = &context()});
  }
  out.append(ir::Fn(f));
  return;
}

void Compiler::EmitMoveInit(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = EmitAs<ir::Fn>(node),
      .location = *to[0],
  });
}

void Compiler::EmitCopyInit(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = EmitAs<ir::Fn>(node),
      .location = *to[0],
  });
}

void Compiler::EmitMoveAssign(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = EmitAs<ir::Fn>(node),
      .location = *to[0],
  });
}

void Compiler::EmitCopyAssign(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = EmitAs<ir::Fn>(node),
      .location = *to[0],
  });
}

bool Compiler::EmitShortFunctionBody(ast::ShortFunctionLiteral const *node) {
  ir::NativeFn ir_func = context().FindNativeFn(node);
  push_current(&*ir_func);
  absl::Cleanup c = [&] { state().current.pop_back(); };
  auto cleanup         = EmitScaffolding(*this, *ir_func, node->body_scope());

  current_block() = current().subroutine->entry();

  // TODO arguments should be renumbered to not waste space on const values
  size_t i = 0;
  for (auto const &param : node->params()) {
    absl::Span<ast::Declaration::Id const> ids = param.value->ids();
    ASSERT(ids.size() == 1u);
    state().set_addr(&ids[0], ir::Reg::Arg(i++));
  }

  type::Type ret_type = ir_func.type()->return_types()[0];
  if (ret_type.is_big()) {
    type::Typed<ir::RegOr<ir::addr_t>> typed_alloc(
        ir::RegOr<ir::addr_t>(ir::Reg::Out(0)), ret_type);
    EmitMoveInit(node->body(), absl::MakeConstSpan(&typed_alloc, 1));
  } else {
    ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
               uint16_t, uint32_t, uint64_t, float, double, type::Type,
               ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn, ir::GenericFn,
               interface::Interface>(ret_type, [&]<typename T>() {
      auto value = this->EmitAs<T>(node->body());
      current_block()->Append(ir::SetReturnInstruction<T>{
          .index = 0,
          .value = value,
      });
    });
  }

  DestroyTemporaries();

  current_block()->set_jump(ir::JumpCmd::Return());

  context().ir().WriteByteCode<EmitByteCode>(ir_func);
  return true;
}

}  // namespace compiler
