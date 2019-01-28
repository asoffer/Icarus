#include "type/struct.h"

#include "architecture.h"
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {
using base::check::Is;

void Struct::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                            ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  std::unique_lock lock(mtx_);
  ASSERT(this == from_type);

  if (copy_assign_func_ == nullptr) {
    for (auto &decl : scope_->AllDeclsWithId("copy", ctx)) {
      // Note: there cannot be more than one declaration with the correct type
      // because our shadowing checks would have caught it.
      //
      // TODO check when verifying the declaration that functions named "copy"
      // adhere to a specific interface.
      auto *fn_type = decl.type()->if_as<Function>();
      if (fn_type == nullptr) { continue; }
      if (fn_type->input.front()->as<Pointer>().pointee != this) { continue; }
      copy_assign_func_ =
          std::get<ir::AnyFunc>(decl.get()->EmitIR(ctx)[0].value);
      goto call_it;
    }

    copy_assign_func_ =
        ctx->mod_->AddFunc(type::Func({from_type, type::Ptr(this)}, {}),
                           ast::FnParams<ast::Expression *>(2));

    CURRENT_FUNC(copy_assign_func_.func()) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto val                = ir::Func::Current->Argument(0);
      auto var                = ir::Func::Current->Argument(1);

      for (size_t i = 0; i < fields_.size(); ++i) {
        auto *field_type = from_type->as<type::Struct>().fields_.at(i).type;
        fields_[i].type->EmitCopyAssign(
            fields_[i].type,
            ir::Val::Reg(ir::PtrFix(ir::Field(val, this, i), field_type),
                         field_type),
            ir::Field(var, this, i), ctx);
      }

      ir::ReturnJump();
    }
  }

call_it:
  ir::Arguments call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = copy_assign_func_.func()->type_;
  ir::Call(copy_assign_func_, std::move(call_args));
}

void Struct::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                            ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  std::unique_lock lock(mtx_);
  ASSERT(this == from_type);

  if (move_assign_func_ == nullptr) {
    for (auto &decl : scope_->AllDeclsWithId("copy", ctx)) {
      // Note: there cannot be more than one declaration with the correct type
      // because our shadowing checks would have caught it.
      //
      // TODO check when verifying the declaration that functions named "copy"
      // adhere to a specific interface.
      auto *fn_type = decl.type()->if_as<Function>();
      if (fn_type == nullptr) { continue; }
      if (fn_type->input.front()->as<Pointer>().pointee != this) { continue; }
      move_assign_func_ =
          std::get<ir::AnyFunc>(decl.get()->EmitIR(ctx)[0].value);
      goto call_it;
    }

    move_assign_func_ =
        ctx->mod_->AddFunc(type::Func({from_type, type::Ptr(this)}, {}),
                           ast::FnParams<ast::Expression *>(2));

    CURRENT_FUNC(move_assign_func_.func()) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto val                = ir::Func::Current->Argument(0);
      auto var                = ir::Func::Current->Argument(1);

      for (size_t i = 0; i < fields_.size(); ++i) {
        auto *field_type = from_type->as<type::Struct>().fields_.at(i).type;
        fields_[i].type->EmitCopyAssign(
            fields_[i].type,
            ir::Val::Reg(ir::PtrFix(ir::Field(val, this, i), field_type),
                         field_type),
            ir::Field(var, this, i), ctx);
      }

      ir::ReturnJump();
    }
  }

call_it:
  ir::Arguments call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = move_assign_func_.func()->type_;
  ir::Call(move_assign_func_, std::move(call_args));
}

size_t Struct::offset(size_t field_num, Architecture const &arch) const {
  size_t offset = 0;
  for (size_t i = 0; i < field_num; ++i) {
    offset += arch.bytes(fields_.at(i).type);
    offset = arch.MoveForwardToAlignment(fields_.at(i + 1).type, offset);
  }
  return offset;
}

void Struct::EmitInit(ir::Register id_reg, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(Func({Ptr(this)}, {}),
                                    ast::FnParams<ast::Expression *>(1));

    CURRENT_FUNC(init_func_) {
      ir::BasicBlock::Current = init_func_->entry();

      // TODO init expressions? Do these need to be verfied too?
      for (size_t i = 0; i < fields_.size(); ++i) {
        if (fields_[i].init_val != ir::Val::None()) {
          EmitCopyInit(/* from_type = */ fields_[i].type,
                       /*   to_type = */ fields_[i].type,
                       /*  from_val = */ fields_[i].init_val,
                       /*    to_var = */
                       ir::Field(init_func_->Argument(0), this, i), ctx);
        } else {
          fields_.at(i).type->EmitInit(
              ir::Field(init_func_->Argument(0), this, i), ctx);
        }
      }

      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  ir::Call(ir::AnyFunc{init_func_}, std::move(call_args));
}

size_t Struct::index(std::string const &name) const {
  return field_indices_.at(name);
}

Struct::Field const *Struct::field(std::string const &name) const {
  auto iter = field_indices_.find(name);
  if (iter == field_indices_.end()) { return nullptr; }
  return &fields_[iter->second];
}

void Struct::EmitDestroy(ir::Register reg, Context *ctx) const {
  // TODO where do we check that dtors are only defined in the same module?
  {
    std::unique_lock lock(mtx_);
    if (destroy_func_ == nullptr) {
      for (auto &decl : scope_->AllDeclsWithId("~", ctx)) {
        ASSIGN_OR(continue, auto &fn_type, decl.type()->if_as<Function>());
        // Should have already been part of type-checking.
        ASSERT(fn_type.input.size() == 1u);
        ASSERT(fn_type.input[0], Is<Pointer>());
        auto *ptee    = fn_type.input[0]->as<Pointer>().pointee;
        if (ptee != this) { continue; }
        destroy_func_ = std::get<ir::AnyFunc>(decl.get()->EmitIR(ctx)[0].value);
        goto call_it;
      }

      destroy_func_ =
          ir::AnyFunc{ctx->mod_->AddFunc(type::Func({type::Ptr(this)}, {}),
                                         ast::FnParams<ast::Expression *>(1))};

      CURRENT_FUNC(destroy_func_.func()) {
        ir::BasicBlock::Current = destroy_func_.func()->entry();
        for (size_t i = 0; i < fields_.size(); ++i) {
          fields_[i].type->EmitDestroy(
              ir::Field(destroy_func_.func()->Argument(0), this, i), ctx);
        }
        ir::ReturnJump();
      }
    }
  }
call_it:
  ir::Arguments call_args;
  call_args.append(reg);
  call_args.type_ = destroy_func_.func()->type_;
  ir::Call(destroy_func_, std::move(call_args));
}
void Struct::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

void Struct::set_last_name(std::string_view s) {
  fields_.back().name = std::string(s);
  auto[iter, success] =
      field_indices_.emplace(fields_.back().name, fields_.size() - 1);
  ASSERT(success);
}

void Struct::add_hashtag(ast::Hashtag hashtag) { hashtags_.push_back(hashtag); }

void Struct::add_hashtag_to_last_field(ast::Hashtag hashtag) {
  fields_.back().hashtags_.push_back(hashtag);
}

void Struct::add_field(type::Type const *t) { fields_.emplace_back(t); }

bool Struct::IsDefaultInitializable() const {
  // TODO check that all sub-fields also have this requirement.
  return std::none_of(hashtags_.begin(), hashtags_.end(), [](ast::Hashtag tag) {
    return tag.kind_ == ast::Hashtag::Builtin::NoDefault;
  });
}

bool Struct::IsCopyable() const {
  // TODO check that all sub-fields also have this requirement.
  return std::none_of(hashtags_.begin(), hashtags_.end(), [](ast::Hashtag tag) {
    return tag.kind_ == ast::Hashtag::Builtin::Uncopyable;
  });
}

bool Struct::needs_destroy() const {
  return true;
  //   return std::any_of(fields_.begin(), fields_.end(),
  //                      [](Field const &f) { return f.type->needs_destroy();
  //                      });
}

void Struct::EmitRepr(ir::Val const &val, Context *ctx) const { UNREACHABLE(); }

void Struct::WriteTo(std::string *result) const {
  result->append("struct.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

bool Struct::contains_hashtag(ast::Hashtag needle) const {
  for (auto const &tag : hashtags_) {
    if (tag.kind_ == needle.kind_) { return true; }
  }
  return false;
}

}  // namespace type
