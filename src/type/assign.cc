#include "type/all.h"

#include "architecture.h"
#include "context.h"
#include "ir/func.h"
#include "module.h"

// TODO destructor for previously held value.
// TODO here and everywhere else: choose a canonical module to add these
// fucntions to.

namespace type {
using base::check::Is;
void Array::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                       Context *ctx) const {
  ASSERT(from_type, Is<Array>());
  auto *from_array_type = &from_type->as<Array>();

  std::unique_lock lock(mtx_);
  auto *&fn = assign_fns_[from_array_type];
  if (fn == nullptr) {
    fn = ctx->mod_->AddFunc(
        Func({from_type, Ptr(this)}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});

    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      auto val                = fn->Argument(0);
      auto var                = fn->Argument(1);

      auto len = IR::ValFrom([&]() -> IR::RegisterOr<i32> {
        if (from_array_type->fixed_length) {
          return static_cast<i32>(from_array_type->len);
        }
        return IR::LoadInt(IR::ArrayLength(std::get<IR::Register>(val.value)));
      }());

      auto *from_ptr_type = type::Ptr(from_type->as<type::Array>().data_type);
      IR::Register from_ptr =
          IR::Index(from_type, std::get<IR::Register>(val.value), 0);
      IR::Register from_end_ptr =
          IR::PtrIncr(from_ptr, len.reg_or<i32>(), from_ptr_type);

      if (!fixed_length) {
        ComputeDestroyWithoutLock(ctx);
        IR::LongArgs call_args;
        call_args.append(var);
        IR::Call(IR::Val::Func(destroy_func_), std::move(call_args));

        // TODO Architecture dependence?
        auto to_bytes = Architecture::InterprettingMachine().ComputeArrayLength(
            len, data_type);
        auto ptr = IR::Malloc(data_type, to_bytes.reg_or<i32>());
        IR::Store(len, IR::ArrayLength(std::get<IR::Register>(var.value)));
        IR::StoreAddr(
            ptr, IR::ArrayData(std::get<IR::Register>(var.value), var.type));
      }

      auto *to_ptr_type = type::Ptr(data_type);
      IR::Register to_ptr =
          IR::Index(type::Ptr(this), std::get<IR::Register>(var.value), 0);

      CreateLoop({IR::Val::Reg(from_ptr, from_ptr_type),
                  IR::Val::Reg(to_ptr, to_ptr_type)},
                 [&](const base::vector<IR::Val> &phis) {
                   return IR::ValFrom(IR::EqAddr(
                       std::get<IR::Register>(phis[0].value), from_end_ptr));
                 },
                 [&](const base::vector<IR::Val> &phis) {
                   EmitCopyInit(from_array_type->data_type, data_type,
                                PtrCallFix(phis[0]), phis[1], ctx);
                   return base::vector<IR::Val>{
                       IR::Val::Reg(
                           IR::PtrIncr(std::get<IR::Register>(phis[0].value), 1,
                                       phis[0].type),
                           phis[0].type),
                       IR::Val::Reg(
                           IR::PtrIncr(std::get<IR::Register>(phis[1].value), 1,
                                       phis[1].type),
                           phis[1].type)};
                 });
      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(from);
  call_args.append(to);
  IR::Call(IR::Val::Func(fn), std::move(call_args));
}

void Pointer::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                         Context *ctx) const {
  ASSERT(this == from_type);
  IR::Store(from, std::get<IR::Register>(to.value));
}

void Scope::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                       Context *ctx) const {
  ASSERT(this == from_type);
  IR::Store(from, std::get<IR::Register>(to.value));
}

void Enum::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                      Context *ctx) const {
  ASSERT(this == from_type);
  IR::Store(from, std::get<IR::Register>(to.value));
}

void Flags::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                       Context *ctx) const {
  ASSERT(this == from_type);
  IR::Store(from, std::get<IR::Register>(to.value));
}

void Variant::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                         Context *ctx) const {
  if (from_type->is<Variant>()) {
    // TODO find the best match for variant types. For instance, we allow
    // assignments like:
    // [3; int] | [4; bool] -> [--; int] | [--; bool]
    auto actual_type =
        IR::LoadType(IR::VariantType(std::get<IR::Register>(from.value)));
    auto landing     = IR::Func::Current->AddBlock();
    for (const Type *v : from_type->as<Variant>().variants_) {
      auto next_block         = IR::Func::Current->AddBlock();
      IR::BasicBlock::Current = IR::EarlyExitOn<false>(
          next_block, IR::ValFrom(IR::EqType(actual_type, v)));
      IR::StoreType(v, IR::VariantType(std::get<IR::Register>(to.value)));
      v->EmitAssign(
          v,
          PtrCallFix(IR::Val::Reg(
              IR::VariantValue(v, std::get<IR::Register>(from.value)), v)),
          IR::Val::Reg(IR::VariantValue(v, std::get<IR::Register>(to.value)),
                       v),
          ctx);
      IR::UncondJump(landing);
      IR::BasicBlock::Current = next_block;
    }
    IR::UncondJump(landing);
    IR::BasicBlock::Current = landing;
  } else {
    IR::StoreType(from_type, IR::VariantType(std::get<IR::Register>(to.value)));
    // TODO Find the best match amongst the variants available.
    const Type *best_match = from_type;
    best_match->EmitAssign(
        from_type, from,
        IR::Val::Reg(
            IR::VariantValue(best_match, std::get<IR::Register>(to.value)),
            best_match),
        ctx);
  }
}

void Struct::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                        Context *ctx) const {
  std::unique_lock lock(mtx_);
  ASSERT(this == from_type);
  if (!assign_func) {
    assign_func = ctx->mod_->AddFunc(
        Func({from_type, Ptr(this)}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});

    CURRENT_FUNC(assign_func) {
      IR::BasicBlock::Current = assign_func->entry();
      auto val = std::get<IR::Register>(assign_func->Argument(0).value);
      auto var = std::get<IR::Register>(assign_func->Argument(1).value);

      for (size_t i = 0; i < fields_.size(); ++i) {
        // TODO is that the right scope?
        fields_[i].type->EmitAssign(
            fields_[i].type,
            PtrCallFix(IR::Val::Reg(
                IR::Field(val, this, i),
                type::Ptr(from_type->as<type::Struct>().fields_.at(i).type))),
            IR::Val::Reg(IR::Field(var, this, i),
                         type::Ptr(this->fields_.at(i).type)),
            ctx);
      }

      IR::ReturnJump();
    }
  }
  ASSERT(assign_func != nullptr);
  IR::LongArgs call_args;
  call_args.append(from);
  call_args.append(to);
  IR::Call(IR::Val::Func(assign_func), std::move(call_args));
}

void Function::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                          Context *ctx) const {
  ASSERT(this == from_type);
  IR::Store(from, std::get<IR::Register>(to.value));
}
void Primitive::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                           Context *ctx) const {
  ASSERT(this == from_type);
  IR::Store(from, std::get<IR::Register>(to.value));
}

void CharBuffer::EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                            Context *ctx) const {
  // TODO Only callable at compile-time?
  NOT_YET();
}
}  // namespace type
