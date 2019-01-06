#include "ast/enum_literal.h"

#include "ast/hole.h"
#include "ast/identifier.h"
#include "context.h"
#include "ir/cmd.h"
#include "ir/val.h"

namespace ir {
// TODO as a general rule we let ast reach into ir but not the other direction.
// Fix this.
Register CreateEnum(ast::EnumLiteral::Kind kind, ::Module const *mod) {
  switch (kind) {
    case ast::EnumLiteral::Kind::Enum: {
      auto &cmd = MakeCmd(type::Type_, Op::CreateEnum);
      cmd.mod_  = mod;
      return cmd.result;
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto &cmd = MakeCmd(type::Type_, Op::CreateFlags);
      cmd.mod_  = mod;
      return cmd.result;
    } break;
    default: UNREACHABLE();
  }
}

void AddEnumerator(ast::EnumLiteral::Kind kind, Register reg,
                   std::string_view token) {
  switch (kind) {
    case ast::EnumLiteral::Kind::Enum: {
      auto &cmd     = MakeCmd(type::Type_, Op::AddEnumerator);
      cmd.add_enumerator_ = {reg, token};
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto &cmd     = MakeCmd(type::Type_, Op::AddFlag);
      cmd.add_enumerator_ = {reg, token};
    } break;
    default: UNREACHABLE();
  }
}

void SetEnumerator(Register reg, RegisterOr<i32> val) {
  auto &cmd           = MakeCmd(type::Type_, Op::SetEnumerator);
  cmd.set_enumerator_ = {reg, val};
}

TypedRegister<type::Type const *> FinalizeEnum(ast::EnumLiteral::Kind kind,
                                               ir::Register reg) {
  switch (kind) {
    case ast::EnumLiteral::Kind::Enum: {
      auto &cmd = MakeCmd(type::Type_, Op::FinalizeEnum);
      cmd.reg_  = reg;
      return cmd.result;
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto &cmd = MakeCmd(type::Type_, Op::FinalizeFlags);
      cmd.reg_ = reg;
      return cmd.result;
    } break;
    default: UNREACHABLE();
  }
}
}  // namespace ir

namespace ast {
std::string EnumLiteral::to_string(size_t n) const {
  std::stringstream ss;
  switch (kind_) {
    case Kind::Enum: ss << "enum"; break;
    case Kind::Flags: ss << "flags"; break;
  }
  ss << " {\n";
  for (auto &elem : elems_) {
    ss << std::string((n + 1) * 2, ' ') << elem->to_string(n + 1) << "\n";
  }
  ss << std::string(n * 2, ' ') << "}";
  return ss.str();
}

void EnumLiteral::assign_scope(Scope *scope) {
  enum_scope_ = scope->add_child<DeclScope>();
  for (auto &elem : elems_) { elem->assign_scope(enum_scope_.get()); }
}

VerifyResult EnumLiteral::VerifyType(Context *ctx) {
  return ctx->set_type(this, type::Type_);
}

void EnumLiteral::Validate(Context *ctx) {
  for (auto &elem : elems_) {
    if (auto *decl = elem->if_as<Declaration>()) {
      auto *t = decl->init_val->VerifyType(ctx).type_;
      ASSERT(t == type::Int32);
      // TODO determine what is allowed here and how to generate errors.
      elem->as<Declaration>().init_val->Validate(ctx);
    }
  }
}

void EnumLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &elem : elems_) { elem->ExtractJumps(rets); }
}

base::vector<ir::Val> EnumLiteral::EmitIR(Context *ctx) {
  auto reg = ir::CreateEnum(kind_, ctx->mod_);
  for (auto &elem : elems_) {
    if (elem->is<Identifier>()) {
      ir::AddEnumerator(kind_, reg, elem->as<Identifier>().token);
    } else if (elem->is<Declaration>()) {
      auto &decl = elem->as<Declaration>();
      ir::AddEnumerator(kind_, reg, decl.id_);
      if (!decl.init_val->is<Hole>()) {
        ir::SetEnumerator(reg, decl.init_val->EmitIR(ctx)[0].reg_or<i32>());
      }
    }
  }
  return {ir::Val(ir::FinalizeEnum(kind_, reg))};
}

}  // namespace ast
