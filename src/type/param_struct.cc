#include "type.h"

#include "../ast/ast.h"
#include "../ir/ir.h"
#include "scope.h"

ParamStruct::ParamStruct(const std::string &name,
                         std::vector<AST::Declaration *> params,
                         std::vector<AST::Declaration *> decls)
    : bound_name(name), params(std::move(params)), decls(std::move(decls)) {}

IR::Func *ParamStruct::IRFunc() {
  NOT_YET();
  /*
  if (ir_func) { return ir_func; } // Cache
  for (auto p : params) { p->verify_types(); }

  std::vector<Type *> param_types;
  for (auto p : params) { param_types.push_back(p->type); }

  ir_func = new IR::Func(Func(Tup(param_types), Type_), false);
  ir_func->name = "anon-param-struct-func";

  CURRENT_FUNC(ir_func) {
    IR::Block::Current = ir_func->entry();
    auto found_block = IR::Func::Current->AddBlock("found-rhs");
    auto not_found_block = IR::Func::Current->AddBlock("not-found-rhs");

    auto cached_val = IR::GetFromCache(IR::Val::Type(this));
    auto found = IR::EQ(Type_, cached_val, IR::Val::Type(Err));
    IR::Block::Current->SetConditional(found, not_found_block, found_block);

    found_block->SetReturn(cached_val);
    IR::Block::Current = not_found_block;

    auto vec = IR::InitFieldVec(decls.size());
    for (size_t i = 0; i < decls.size(); ++i) {

      size_t len = decls[i]->identifier->token.size();
      char *id_str = new char[len + 1];
      strcpy(id_str, decls[i]->identifier->token.c_str());
      // id_str owned by PushField after this.

      if (decls[i]->type_expr) {
        decls[i]->type_expr->verify_types();
      }
      if (decls[i]->init_val) {
        decls[i]->init_val->verify_types();
      }

      IR::PushField(vec, id_str,
                    decls[i]->type_expr ? decls[i]->type_expr->EmitIR()
                                        : IR::Val::None(),
                    decls[i]->init_val ? decls[i]->init_val->EmitIR()
                                       : IR::Val::None());
    }

    auto result_type = IR::CreateStruct(vec, IR::Val::HeapAddr(this));

    IR::Block::Current->SetUnconditional(ir_func->exit());
    IR::Block::Current = ir_func->exit();
    IR::Block::Current->SetReturn(result_type);
  }

  return ir_func;
  */
}

void ParamStruct::EmitInit(IR::Val) { UNREACHABLE(); }
void ParamStruct::EmitDestroy(IR::Val) { UNREACHABLE(); }
void ParamStruct::EmitRepr(IR::Val) { UNREACHABLE(); }
IR::Val ParamStruct::EmitInitialValue() const { UNREACHABLE(); }
std::string ParamStruct::to_string() const { return bound_name; }
