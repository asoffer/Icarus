#include "type/struct.h"

#include "misc/architecture.h"
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/guarded.h"
#include "misc/context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

static std::optional<ir::AnyFunc> SpecialFunction(Struct const *s, char const *symbol,
                                                Context *ctx) {
  auto *ptr_to_s = Ptr(s);
  for (auto &decl : s->scope_->AllDeclsWithId(symbol, ctx)) {
    // Note: there cannot be more than one declaration with the correct type
    // because our shadowing checks would have caught it.
    auto *fn_type = decl.type()->if_as<Function>();
    if (fn_type == nullptr) { continue; }
    if (fn_type->input.front() != ptr_to_s) { continue; }
    return decl.get()->EmitIr(ctx).get<ir::AnyFunc>(0).val_;
  }
  return std::nullopt;
}

template <SpecialFunctionCategory Cat>
static ir::AnyFunc CreateAssign(Struct const *s, Context *ctx) {
  if (auto fn = SpecialFunction(s, Name<Cat>(), ctx)) { return *fn; }
  Pointer const *pt = Ptr(s);
  ir::AnyFunc fn =
      s->mod_->AddFunc(Func({pt, pt}, {}), ast::FnParams<ast::Expression *>(2));
  CURRENT_FUNC(fn.func()) {
    ir::BasicBlock::Current = ir::Func::Current->entry();
    auto val                = ir::Func::Current->Argument(0);
    auto var                = ir::Func::Current->Argument(1);

    for (size_t i = 0; i < s->fields_.size(); ++i) {
      auto *field_type = s->fields_.at(i).type;
      auto from =
          ir::Results{ir::PtrFix(ir::Field(val, s, i).get(), field_type)};
      auto to = ir::Field(var, s, i).get();

      if constexpr (Cat == Copy) {
        field_type->EmitCopyAssign(field_type, from, to, ctx);
      } else if constexpr (Cat == Move) {
        field_type->EmitMoveAssign(field_type, from, to, ctx);
      } else {
        UNREACHABLE();
      }
    }

    ir::ReturnJump();
  }
  return fn;
}

void Struct::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                            ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  copy_assign_func_.init(
      [this, ctx]() { return CreateAssign<Copy>(this, ctx); });
  ir::Copy(this, from.get<ir::Reg>(0), to);
}

void Struct::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                            ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  move_assign_func_.init(
      [this, ctx]() { return CreateAssign<Move>(this, ctx); });
  ir::Move(this, from.get<ir::Reg>(0), to);
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
  init_func_.init([this, ctx]() {
    // TODO special function?

    ir::AnyFunc fn = mod_->AddFunc(Func({Ptr(this)}, {}),
                                   ast::FnParams<ast::Expression *>(1));
    CURRENT_FUNC(fn.func()) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto var                = ir::Func::Current->Argument(0);
      for (size_t i = 0; i < fields_.size(); ++i) {
        auto ir_field     = ir::Field(var, this, i);
        auto const &field = fields_.at(i);
        // if (field.init_val != ir::Val::None()) {
        //   EmitCopyInit(field.type, field.init_val, ir_field, ctx);
        // } else {
          field.type->EmitInit(ir_field.get(), ctx);
        //  }
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Init(this, id_reg);
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
  destroy_func_.init([this, ctx]() {
    if (auto fn = SpecialFunction(this, "~", ctx)) { return *fn; }

    Pointer const *pt = Ptr(this);
    ir::AnyFunc fn =
        mod_->AddFunc(Func({pt}, {}), ast::FnParams<ast::Expression *>(1));
    CURRENT_FUNC(fn.func()) {
      ir::BasicBlock::Current = ir::Func::Current->entry();
      auto var                = ir::Func::Current->Argument(0);

      for (int i = static_cast<int>(fields_.size()) - 1; i >= 0; --i) {
        fields_.at(i).type->EmitDestroy(ir::Field(var, this, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Destroy(this, reg);
}

void Struct::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

void Struct::set_last_name(std::string_view s) {
  fields_.back().name = std::string(s);
  auto[iter, success] =
      field_indices_.emplace(fields_.back().name, fields_.size() - 1);
  static_cast<void>(iter);
  ASSERT(success == true);
}

void Struct::add_hashtag(ast::Hashtag hashtag) { hashtags_.push_back(hashtag); }

void Struct::add_hashtag_to_last_field(ast::Hashtag hashtag) {
  fields_.back().hashtags_.push_back(hashtag);
}

void Struct::add_field(Type const *t) { fields_.emplace_back(t); }

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

bool Struct::IsMovable() const {
  // TODO check that all sub-fields also have this requirement.
  return std::none_of(hashtags_.begin(), hashtags_.end(), [](ast::Hashtag tag) {
    return tag.kind_ == ast::Hashtag::Builtin::Immovable;
  });
}

bool Struct::needs_destroy() const {
  // TODO is this okay? Does it work for generics? Does it need to?
  Context ctx(mod_);
  return SpecialFunction(this, "~", &ctx) ||
         std::any_of(fields_.begin(), fields_.end(),
                     [](Field const &f) { return f.type->needs_destroy(); });
}

void Struct::EmitRepr(ir::Results const &val, Context *ctx) const { UNREACHABLE(); }

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

ir::Results Struct::PrepareArgument(Type const *from, ir::Results const &val,
                                    Context *ctx) const {
  auto arg = ir::Alloca(this);

  if (from->is<Variant>()) {
    EmitMoveAssign(this,
                   ir::Results{ir::VariantValue(this, val.get<ir::Reg>(0))},
                   arg, ctx);
  } else if (this == from) {
    EmitMoveAssign(from, val, arg, ctx);
  } else {
    UNREACHABLE(from);
  }
  return ir::Results{arg};
}

Cmp Struct::Comparator() const { return Cmp::None; }

}  // namespace type
