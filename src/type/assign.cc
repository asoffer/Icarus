#include "type/all.h"

#include "architecture.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"

// TODO destructor for previously held value.
// TODO here and everywhere else: choose a canonical module to add these
// fucntions to.

namespace type {
using base::check::Is;
void Array::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                       Context *ctx) const {
  ASSERT(from_type, Is<Array>());
  auto *from_array_type = &from_type->as<Array>();

  std::unique_lock lock(mtx_);
  auto *&fn = assign_fns_[from_array_type];
  if (fn == nullptr) {
    fn = ctx->mod_->AddFunc(
        type::Func({from_type, type::Ptr(this)}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});

    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      auto val                = fn->Argument(0);
      auto var                = fn->Argument(1);

      auto len = [&]() -> IR::RegisterOr<i32> {
        if (from_array_type->fixed_length) {
          return static_cast<i32>(from_array_type->len);
        }
        return IR::LoadInt(IR::ArrayLength(val));
      }();

      auto *from_ptr_type   = type::Ptr(from_type->as<type::Array>().data_type);
      IR::Register from_ptr = IR::Index(from_type, val, 0);
      IR::Register from_end_ptr = IR::PtrIncr(from_ptr, len, from_ptr_type);

      if (!fixed_length) {
        ComputeDestroyWithoutLock(ctx);
        IR::LongArgs call_args;
        call_args.append(var);
        call_args.type_ = destroy_func_->type_;
        IR::Call(IR::AnyFunc{destroy_func_}, std::move(call_args));

        // TODO Architecture dependence?
        auto ptr = IR::Malloc(
            data_type, Architecture::InterprettingMachine().ComputeArrayLength(
                           len, data_type));
        IR::StoreInt(len, IR::ArrayLength(var));
        IR::StoreAddr(ptr, IR::ArrayData(var, type::Ptr(this)));
      }

      auto *to_ptr_type   = type::Ptr(data_type);
      IR::Register to_ptr = IR::Index(type::Ptr(this), var, 0);

      using tup =
          std::tuple<IR::RegisterOr<IR::Addr>, IR::RegisterOr<IR::Addr>>;
      IR::CreateLoop(
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            return IR::EqAddr(std::get<0>(phis).reg_, from_end_ptr);
          },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            ASSERT(std::get<1>(phis).is_reg_);

            IR::Register ptr_fixed_reg =
                from_type->as<type::Array>().data_type->is_big()
                    ? std::get<0>(phis).reg_
                    : IR::Load(std::get<0>(phis).reg_, data_type);
            auto ptr_fixed_type =
                !from_type->as<type::Array>().data_type->is_big()
                    ? from_type->as<type::Array>().data_type
                    : type::Ptr(from_type->as<type::Array>().data_type);

            EmitCopyInit(from_array_type->data_type, data_type,
                         IR::Val::Reg(ptr_fixed_reg, ptr_fixed_type),
                         std::get<1>(phis).reg_, ctx);
            return std::make_tuple(
                IR::RegisterOr<IR::Addr>{
                    IR::PtrIncr(std::get<0>(phis).reg_, 1, from_ptr_type)},
                IR::RegisterOr<IR::Addr>{
                    IR::PtrIncr(std::get<1>(phis).reg_, 1, to_ptr_type)});
          },
          std::tuple<type::Type const *, type::Type const *>{from_ptr_type,
                                                             to_ptr_type},
          tup{IR::RegisterOr<IR::Addr>{from_ptr},
              IR::RegisterOr<IR::Addr>{to_ptr}});
      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = fn->type_;
  IR::Call(IR::AnyFunc{fn}, std::move(call_args));
}

void Pointer::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                         Context *ctx) const {
  ASSERT(this == from_type);
  IR::StoreAddr(from.reg_or<IR::Addr>(), to);
}

void Enum::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                      Context *ctx) const {
  ASSERT(this == from_type);
  IR::StoreEnum(from.reg_or<IR::EnumVal>(), to);
}

void Flags::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                       Context *ctx) const {
  ASSERT(this == from_type);
  IR::StoreFlags(from.reg_or<IR::FlagsVal>(), to);
}

void Variant::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                         Context *ctx) const {
  if (from_type->is<Variant>()) {
    // TODO find the best match for variant types. For instance, we allow
    // assignments like:
    // [3; int] | [4; bool] -> [--; int] | [--; bool]
    auto actual_type =
        IR::LoadType(IR::VariantType(std::get<IR::Register>(from.value)));
    auto landing = IR::Func::Current->AddBlock();
    for (const Type *v : from_type->as<Variant>().variants_) {
      auto next_block = IR::Func::Current->AddBlock();
      IR::BasicBlock::Current =
          IR::EarlyExitOn<false>(next_block, IR::EqType(actual_type, v));
      IR::StoreType(v, IR::VariantType(to));
      v->EmitAssign(
          v,
          IR::Val::Reg(
              IR::PtrFix(
                  IR::VariantValue(v, std::get<IR::Register>(from.value)), v),
              v),
          IR::VariantValue(v, to), ctx);
      IR::UncondJump(landing);
      IR::BasicBlock::Current = next_block;
    }
    IR::UncondJump(landing);
    IR::BasicBlock::Current = landing;
  } else {
    IR::StoreType(from_type, IR::VariantType(to));
    // TODO Find the best match amongst the variants available.
    const Type *best_match = from_type;
    best_match->EmitAssign(from_type, from, IR::VariantValue(best_match, to),
                           ctx);
  }
}

void Struct::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                        Context *ctx) const {
  std::unique_lock lock(mtx_);
  ASSERT(this == from_type);
  if (!assign_func) {
    assign_func = ctx->mod_->AddFunc(
        type::Func({from_type, type::Ptr(this)}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});

    CURRENT_FUNC(assign_func) {
      IR::BasicBlock::Current = assign_func->entry();
      auto val                = assign_func->Argument(0);
      auto var                = assign_func->Argument(1);

      for (size_t i = 0; i < fields_.size(); ++i) {
        auto *field_type = from_type->as<type::Struct>().fields_.at(i).type;
        fields_[i].type->EmitAssign(
            fields_[i].type,
            IR::Val::Reg(IR::PtrFix(IR::Field(val, this, i), field_type),
                         field_type),
            IR::Field(var, this, i), ctx);
      }

      IR::ReturnJump();
    }
  }
  ASSERT(assign_func != nullptr);
  IR::LongArgs call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = assign_func->type_;
  IR::Call(IR::AnyFunc{assign_func}, std::move(call_args));
}

void Function::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                          Context *ctx) const {
  ASSERT(this == from_type);
  IR::StoreFunc(from.reg_or<IR::Func *>(), to);
}
void Primitive::EmitAssign(const Type *from_type, IR::Val from, IR::Register to,
                           Context *ctx) const {
  ASSERT(this == from_type);
  switch (this->type_) {
    case PrimType::Err: UNREACHABLE(this, ": Err");
    case PrimType::Type_:
      IR::StoreType(from.reg_or<type::Type const *>(), to);
      break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Bool: IR::StoreBool(from.reg_or<bool>(), to); break;
    case PrimType::Char: IR::StoreChar(from.reg_or<char>(), to); break;
    case PrimType::Int: IR::StoreInt(from.reg_or<i32>(), to); break;
    case PrimType::Real: IR::StoreReal(from.reg_or<double>(), to); break;
    default: UNREACHABLE();
  }
}

void CharBuffer::EmitAssign(const Type *from_type, IR::Val from,
                            IR::Register to, Context *ctx) const {
  // TODO Only callable at compile-time?
  NOT_YET();
}
}  // namespace type
