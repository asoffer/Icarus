#include "AST.h"

namespace AST {
  Expression::Expression() : expr_type_(Unknown) {}

  Identifier::Identifier(size_t line_num, const std::string& token_string) : alloc_(nullptr), is_function_arg_(false) {
    token_ = token_string;
    expr_type_ = Unknown;
    precedence_ =
      Language::precedence(Language::Operator::NotAnOperator);
    line_num_ = line_num;
  }


  FunctionLiteral::FunctionLiteral() :
    fn_scope_(new FnScope(nullptr)), llvm_function_(nullptr) {}

  TypeLiteral::TypeLiteral() :
    type_scope_(Scope::build<TypeScope>()), type_value_(nullptr) {}

  // TODO Will TypeScope suffice?
  EnumLiteral::EnumLiteral() :
    enum_scope_(Scope::build<TypeScope>()), type_value_(nullptr) {}

  While::While() : body_scope_(Scope::build<WhileScope>()) {}

  // TODO put this somewhere else
  void TypeLiteral::build_llvm_internals() {
    for (const auto& decl : decls_) {
      if (decl->type_is_inferred()) {
        // TODO
      } else {
        auto field = decl->declared_type()->evaluate(Scope::Global->context()).as_type;
        assert(field && "field is nullptr");
        std::cout <<*this << std::endl;
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
