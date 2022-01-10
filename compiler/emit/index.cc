#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"

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
  out.append(c.builder().PtrFix(result->reg(), result_type));
}

}  // namespace

void Compiler::EmitToBuffer(ast::Index const *node, ir::PartialResultBuffer &out) {
  type::QualType qt = context().qual_types(node->lhs())[0];
  if (auto const *s = qt.type().if_as<type::Slice>()) {
    if (qt.quals() >= type::Quals::Ref()) {
      out.append(builder().PtrFix(EmitRef(node),
                                  context().qual_types(node)[0].type()));
    } else {
      auto data = builder().Load<ir::addr_t>(
          current_block()->Append(type::SliceDataInstruction{
              .slice  = EmitAs<ir::addr_t>(node->lhs()),
              .result = builder().CurrentGroup()->Reserve(),
          }),
          type::BufPtr(s->data_type()));

      auto index = EmitWithCastTo<int64_t>(
          context().qual_types(node->rhs())[0].type(), node->rhs());
      out.append(builder().PtrFix(builder().Index(type::Ptr(s), data, index),
                                  s->data_type()));
    }
  } else if (auto const *array_type = qt.type().if_as<type::Array>()) {
    if (qt.quals() >= type::Quals::Ref()) {
      out.append(builder().PtrFix(EmitRef(node),
                                  context().qual_types(node)[0].type()));
    } else {
      auto index = EmitWithCastTo<int64_t>(
          context().qual_types(node->rhs())[0].type(), node->rhs());
      out.append(builder().PtrFix(
          builder().Index(
              type::Ptr(context().qual_types(node->lhs())[0].type()),
              EmitRef(node->lhs()), index),
          array_type->data_type()));
    }
  } else if (auto const *buf_ptr_type =
                 qt.type().if_as<type::BufferPointer>()) {
    if (qt.quals() >= type::Quals::Ref()) {
      out.append(builder().PtrFix(EmitRef(node),
                                  context().qual_types(node)[0].type()));
    } else {
      auto index = EmitWithCastTo<int64_t>(
          context().qual_types(node->rhs())[0].type(), node->rhs());
      out.append(
          builder().PtrFix(builder().PtrIncr(EmitAs<ir::addr_t>(node->lhs()),
                                             index, buf_ptr_type),
                           buf_ptr_type->pointee()));
    }
  } else {
    EmitIndexOverload(*this, node, out);
  }
}

ir::Reg Compiler::EmitRef(ast::Index const *node) {
  type::Type lhs_type = context().qual_types(node->lhs())[0].type();
  type::Type rhs_type = context().qual_types(node->rhs())[0].type();

  if (lhs_type.is<type::Array>()) {
    auto index = EmitWithCastTo<int64_t>(rhs_type, node->rhs());
    auto lval  = EmitRef(node->lhs());
    return builder().Index(
        type::Ptr(context().qual_types(node->lhs())[0].type()), lval, index);
  } else if (auto *buf_ptr_type = lhs_type.if_as<type::BufferPointer>()) {
    auto index = EmitWithCastTo<int64_t>(rhs_type, node->rhs());
    return builder().PtrIncr(EmitAs<ir::addr_t>(node->lhs()), index,
                             type::Ptr(buf_ptr_type->pointee()));
  } else if (auto const *s = lhs_type.if_as<type::Slice>()) {
    auto data = builder().Load<ir::addr_t>(
        current_block()->Append(type::SliceDataInstruction{
            .slice  = EmitAs<ir::addr_t>(node->lhs()),
            .result = builder().CurrentGroup()->Reserve(),
        }),
        type::BufPtr(s->data_type()));

    auto index = EmitWithCastTo<int64_t>(rhs_type, node->rhs());
    return builder().PtrIncr(data, index, type::BufPtr(s->data_type()));
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
  EmitMoveAssign(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyInit(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler
