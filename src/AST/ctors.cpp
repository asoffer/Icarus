#include "AST.h"

namespace AST {
Expression::Expression() : type(Unknown) {}
Declaration::Declaration() {}
ArrayLiteral::ArrayLiteral() {}
Access::Access() {}
ChainOp::ChainOp() {}
Case::Case() {}
Binop::Binop() {}
Unop::Unop() {}
Terminal::Terminal() {}
ArrayType::ArrayType() {}

Identifier::Identifier(size_t input_line_num, const std::string &token_string)
    : alloc(nullptr), is_function_arg(false), decl(nullptr) {
  token_ = token_string;
  type = Unknown;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
  line_num = input_line_num;
}

FunctionLiteral::FunctionLiteral()
    : fn_scope(new FnScope(nullptr)), llvm_fn(nullptr) {}

TypeLiteral::TypeLiteral() : type_value(nullptr), type_scope(new TypeScope) {}

EnumLiteral::EnumLiteral() : type_value(nullptr) {}

While::While() : while_scope(new WhileScope) {}

// TODO put this somewhere else
void TypeLiteral::build_llvm_internals() {
  assert(type_value);

  for (const auto &decl : declarations) {
    if (decl->type->has_vars) return;
  }

  for (const auto &decl : declarations) {
    if (decl->is_inferred) {
      assert(false); // TODO

    } else {
      auto field = decl->type_expr->evaluate(Scope::Global->context()).as_type;
      assert(field && "field is nullptr");
      assert(type_value && "null type_value");
      type_value->fields.emplace_back(decl->identifier->token(), field);
    }
  }

  size_t num_fields = type_value->fields.size();
  std::vector<llvm::Type *> llvm_fields(num_fields, nullptr);
  for (size_t i = 0; i < num_fields; ++i) {
    llvm_fields[i] = type_value->fields[i].second->llvm_type;
  }

  static_cast<llvm::StructType *>(type_value->llvm_type)
      ->setBody(std::move(llvm_fields), /* isPacked = */ false);
}
} // namespace AST
