#include "AST.h"

namespace AST {
  Expression::Expression() : expr_type_(Unknown) {}
  Declaration::Declaration() {}
  ArrayLiteral::ArrayLiteral() {}
  Access::Access() {}
  ChainOp::ChainOp() {}
  Case::Case() {}
  Binop::Binop() {}
  Unop::Unop() {}
  Terminal::Terminal() {}
  ArrayType::ArrayType() {}

  Identifier::Identifier(size_t set_line_num, const std::string& token_string) : alloc_(nullptr), is_function_arg_(false), decl_(nullptr) {
    token_ = token_string;
    expr_type_ = Unknown;
    precedence_ =
      Language::precedence(Language::Operator::NotAnOperator);
    line_num = set_line_num;
  }


  FunctionLiteral::FunctionLiteral() :
    fn_scope_(new FnScope(nullptr)), llvm_function_(nullptr) {}

  TypeLiteral::TypeLiteral() : type_value_(nullptr), type_scope_(new TypeScope) {}

  // TODO Will TypeScope suffice?
  EnumLiteral::EnumLiteral() : enum_scope_(new TypeScope), type_value_(nullptr) {}

  While::While() : body_scope_(new WhileScope) {}

  // TODO put this somewhere else
  void TypeLiteral::build_llvm_internals() {
    assert(type_value_);

    for (const auto& decl : decls_) {
      if (decl->type()->has_variables()) return;
    }

    for (const auto& decl : decls_) {
      if (decl->type_is_inferred()) {
        assert(false);
        // TODO
      } else {
        auto field = decl->declared_type()->evaluate(Scope::Global->context()).as_type;
        assert(field && "field is nullptr");
        assert(type_value_ && "null type_value_");
        type_value_->fields_.emplace_back(decl->identifier_string(), field);
      }
    }

    size_t num_fields = type_value_->fields_.size();
    std::vector<llvm::Type*> llvm_fields(num_fields, nullptr);
    for (size_t i = 0; i < num_fields; ++i) {
      llvm_fields[i] = type_value_->fields_[i].second->llvm();
    }

    static_cast<llvm::StructType*>(type_value_->llvm_type_)->setBody(
        std::move(llvm_fields), /* isPacked = */ false);
  }

}  // namespace AST
