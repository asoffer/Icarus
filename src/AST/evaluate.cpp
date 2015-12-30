#include "AST.h"

namespace data {
  extern llvm::Value* const_bool(bool b);
}  // namespace data

namespace AST {
  // TODO
  llvm::Value* Identifier::evaluate()      { return nullptr; }
  llvm::Value* Unop::evaluate()            { return nullptr; }

  llvm::Value* ChainOp::evaluate(){ 
    return data::const_bool(evaluate_as_bool());
  }
 
  bool ChainOp::evaluate_as_bool() {
    for (size_t i = 0; i < ops_.size(); ++i) {
      auto& last = exprs_[i];
      auto& next = exprs_[i + 1];

      if (ops_[i]->token() == "==") {
        if (last->interpret_as_type() != next->interpret_as_type()) {
          return false;
        }

      } else if (ops_[i]->token() == "!=") {
        if (last->interpret_as_type() == next->interpret_as_type()) {
          return false;
        }
      }
    }

    return true;
  }

  llvm::Value* ArrayType::evaluate()       { return nullptr; }
  llvm::Value* ArrayLiteral::evaluate()    { return nullptr; }
  llvm::Value* Terminal::evaluate()        { return nullptr; }
  llvm::Value* FunctionLiteral::evaluate() { return nullptr; }
  llvm::Value* Case::evaluate()            { return nullptr; }
  llvm::Value* Assignment::evaluate()      { return nullptr; }
  llvm::Value* Declaration::evaluate()     { return nullptr; }
  llvm::Value* TypeLiteral::evaluate()     { return nullptr; }
  llvm::Value* EnumLiteral::evaluate()     { return nullptr; }
  llvm::Value* Binop::evaluate()           { return nullptr; }
  llvm::Value* KVPairList::evaluate()      { return nullptr; }
  llvm::Value* Statements::evaluate()      { return nullptr; }
  llvm::Value* Conditional::evaluate()     { return nullptr; }
  llvm::Value* Break::evaluate()           { return nullptr; }
  llvm::Value* While::evaluate()           { return nullptr; }
}  // namespace AST
