#include "type.h"
#include "../ir/ir.h"
#include "../ast/ast.h"

ParamStruct::ParamStruct(
    const std::string &name, const std::vector<AST::Declaration *> params,
    const std::vector<AST::Declaration *> decls)
    : bound_name(name), type_scope(new Scope), params(params), decls(decls),
      ir_func(nullptr) {}

IR::Func *ParamStruct::IRFunc() {
  if (ir_func) { return ir_func; } // Cache
  for (auto p : params) { p->verify_types(); }

  auto saved_func  = IR::Func::Current;
  auto saved_block = IR::Block::Current;

  std::vector<Type *> param_types;
  for (auto p : params) { param_types.push_back(p->type); }

  ir_func = new IR::Func(Func(Tup(param_types), Type_), false);
  ir_func->SetName("anon-param-struct-func");

  IR::Func::Current  = ir_func;
  IR::Block::Current = ir_func->entry();

  auto found_block     = IR::Func::Current->AddBlock("found-rhs");
  auto not_found_block = IR::Func::Current->AddBlock("not-found-rhs");

  auto cached_val = IR::GetFromCache(IR::Value::Type(this));
  auto found = IR::EQ(Type_, cached_val, IR::Value::Type(Err));
  IR::Block::Current->SetConditional(found, not_found_block, found_block);

  found_block->SetReturn(cached_val);
  IR::Block::Current = not_found_block;

  auto vec = IR::InitFieldVec(decls.size());
  for (size_t i = 0; i < decls.size(); ++i) {

    size_t len   = decls[i]->identifier->token.size();
    char *id_str = new char[len + 1];
    strcpy(id_str, decls[i]->identifier->token.c_str());
    // id_str owned by PushField after this.

    if (decls[i]->type_expr) { decls[i]->type_expr->verify_types(); }
    if (decls[i]->init_val) { decls[i]->init_val->verify_types(); }

    IR::PushField(
        vec, id_str,
        decls[i]->type_expr ? decls[i]->type_expr->EmitIR() : IR::Value::None(),
        decls[i]->init_val ? decls[i]->init_val->EmitIR() : IR::Value::None());
  }

  auto result_type = IR::CreateStruct(vec, IR::Value::HeapAddr(this));

  IR::Block::Current->SetUnconditional(ir_func->exit());
  IR::Block::Current = ir_func->exit();
  IR::Block::Current->SetReturn(result_type);

  IR::Func::Current  = saved_func;
  IR::Block::Current = saved_block;

  return ir_func;
}

bool ParamStruct::private_has_vars() { return false; }
size_t ParamStruct::bytes() const { NOT_YET; }
size_t ParamStruct::alignment() const { NOT_YET; }

void ParamStruct::EmitInit(IR::Value id_val) { UNREACHABLE; }
void ParamStruct::EmitDestroy(IR::Value id_val) { UNREACHABLE; }
void ParamStruct::EmitRepr(IR::Value val) { UNREACHABLE; }
IR::Value ParamStruct::EmitInitialValue() const { UNREACHABLE; }
std::string ParamStruct::to_string() const { return bound_name; }
