#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/copy_move_assignment.h"

namespace compiler {
namespace {

void EmitIndexOverload(Compiler &c, ast::Index const *node,
                       ir::PartialResultBuffer &out) {
  // TODO: We claim ownership but later release the ownership. This is
  // safe and correct, but it's also a bit of a lie. It would be better
  // if we had a mechanism to hide ownership.
  std::array<ast::Call::Argument, 2> arguments{
      ast::Call::Argument("", std::unique_ptr<ast::Expression>(
                                  const_cast<ast::Expression *>(node->lhs()))),
      ast::Call::Argument("", std::unique_ptr<ast::Expression>(
                                  const_cast<ast::Expression *>(node->rhs())))};

  type::Type result_type = c.context().qual_types(node)[0].type();
  type::Typed<ir::RegOr<ir::addr_t>> result(c.state().TmpAlloca(result_type),
                                            result_type);

  EmitCall(c, c.context().CallMetadata(node).resolved(), {}, arguments,
           absl::MakeConstSpan(&result, 1));

  for (auto &argument : arguments) {
    auto &&[name, expr] = std::move(argument).extract();
    expr.release();
  }
  out.append(PtrFix(c.current(), result->reg(), result_type));
}

}  // namespace

void Compiler::EmitToBuffer(ast::Index const *node,
                            ir::PartialResultBuffer &out) {
  type::QualType qt = context().qual_types(node->lhs())[0];
  if (auto const *s = qt.type().if_as<type::Slice>()) {
    if (qt.quals() >= type::Qualifiers::Storage()) {
      out.append(PtrFix(current(), EmitRef(node),
                        context().qual_types(node)[0].type()));
    } else {
      auto data = current_block()->Append(ir::LoadInstruction{
          .type   = type::BufPtr(s->data_type()),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitAs<ir::addr_t>(node->lhs()),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      });

      // TODO: Remove assumption that the pointer difference type is int64_t.
      auto index =
          EmitCast(*this, context().typed(node->rhs()),
                   type::PointerDifferenceType(resources().architecture))
              .back()
              .get<int64_t>();

      auto incr = current_block()->Append(
          ir::PtrIncrInstruction{.addr   = data,
                                 .index  = index,
                                 .ptr    = type::Ptr(s->data_type()),
                                 .result = current().subroutine->Reserve()});
      out.append(PtrFix(current(), incr, s->data_type()));
    }
  } else if (auto const *array_type = qt.type().if_as<type::Array>()) {
    if (qt.quals() >= type::Qualifiers::Storage()) {
      out.append(PtrFix(current(), EmitRef(node),
                        context().qual_types(node)[0].type()));
    } else {
      // TODO: Remove assumption that the pointer difference type is int64_t.
      auto index =
          EmitCast(*this, context().typed(node->rhs()),
                   type::PointerDifferenceType(resources().architecture))
              .back()
              .get<int64_t>();
      EmitToBuffer(node->lhs(), out);
      auto incr = current_block()->Append(
          ir::PtrIncrInstruction{.addr   = out.get<ir::addr_t>(0),
                                 .index  = index,
                                 .ptr    = type::Ptr(array_type->data_type()),
                                 .result = current().subroutine->Reserve()});
      out.pop_back();
      out.append(PtrFix(current(), incr, array_type->data_type()));
    }
  } else if (auto const *buf_ptr_type =
                 qt.type().if_as<type::BufferPointer>()) {
    if (qt.quals() >= type::Qualifiers::Storage()) {
      out.append(PtrFix(current(), EmitRef(node),
                        context().qual_types(node)[0].type()));
    } else {
      // TODO: Remove assumption that the pointer difference type is int64_t.
      auto index =
          EmitCast(*this, context().typed(node->rhs()),
                   type::PointerDifferenceType(resources().architecture))
              .back()
              .get<int64_t>();
      auto incr = current_block()->Append(
          ir::PtrIncrInstruction{.addr   = EmitAs<ir::addr_t>(node->lhs()),
                                 .index  = index,
                                 .ptr    = buf_ptr_type,
                                 .result = current().subroutine->Reserve()});
      out.append(PtrFix(current(), incr, buf_ptr_type->pointee()));
    }
  } else {
    EmitIndexOverload(*this, node, out);
  }
}

ir::Reg Compiler::EmitRef(ast::Index const *node) {
  type::Type lhs_type = context().qual_types(node->lhs())[0].type();
  type::Type rhs_type = context().qual_types(node->rhs())[0].type();

  // TODO: Remove assumption that the pointer difference type is int64_t.
  auto index = EmitCast(*this, context().typed(node->rhs()),
                        type::PointerDifferenceType(resources().architecture))
                   .back()
                   .get<int64_t>();

  if (auto const *a = lhs_type.if_as<type::Array>()) {
    auto lval = EmitRef(node->lhs());
    return current_block()->Append(
        ir::PtrIncrInstruction{.addr   = lval,
                               .index  = index,
                               .ptr    = type::Ptr(a->data_type()),
                               .result = current().subroutine->Reserve()});
  } else if (auto *buf_ptr_type = lhs_type.if_as<type::BufferPointer>()) {
    return current_block()->Append(
        ir::PtrIncrInstruction{.addr   = EmitAs<ir::addr_t>(node->lhs()),
                               .index  = index,
                               .ptr    = type::Ptr(buf_ptr_type->pointee()),
                               .result = current().subroutine->Reserve()});
  } else if (auto const *s = lhs_type.if_as<type::Slice>()) {
    auto data = current_block()->Append(ir::LoadInstruction{
        .type   = type::BufPtr(s->data_type()),
        .addr   = current_block()->Append(type::SliceDataInstruction{
            .slice  = EmitAs<ir::addr_t>(node->lhs()),
            .result = current().subroutine->Reserve(),
        }),
        .result = current().subroutine->Reserve(),
    });
    return current_block()->Append(
        ir::PtrIncrInstruction{.addr   = data,
                               .index  = index,
                               .ptr    = type::BufPtr(s->data_type()),
                               .result = current().subroutine->Reserve()});
  }
  UNREACHABLE(lhs_type.to_string());
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyInit(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler
