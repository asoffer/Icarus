#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
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

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

void Compiler::EmitCopyInit(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

void Compiler::EmitMoveAssign(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

void Compiler::EmitCopyAssign(
    ast::ShortFunctionLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  if (node->is_generic()) { NOT_YET(); }

  builder().Store(EmitAs<ir::Fn>(node), *to[0]);
}

bool Compiler::EmitShortFunctionBody(ast::ShortFunctionLiteral const *node) {
  ir::NativeFn ir_func = context().FindNativeFn(node);
  ASSERT(static_cast<bool>(ir_func) == true);

  ICARUS_SCOPE(SetCurrent(ir_func, builder())) {
    builder().CurrentBlock() = builder().CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      absl::Span<ast::Declaration::Id const> ids = param.value->ids();
      ASSERT(ids.size() == 1u);
      builder().set_addr(&ids[0], ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*this, &node->body_scope());

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
        builder().CurrentBlock()->Append(ir::SetReturnInstruction<T>{
            .index = 0,
            .value = value,
        });
      });
    }

    builder().FinishTemporariesWith([this](type::Typed<ir::Reg> r) {
      if (r.type().get()->HasDestructor()) { EmitDestroy(r); }
    });

    MakeAllDestructions(*this, &node->body_scope());
    builder().ReturnJump();
  }

  context().ir().WriteByteCode<EmitByteCode>(ir_func);
  return true;
}

}  // namespace compiler
