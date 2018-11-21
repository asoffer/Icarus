#include "type/all.h"

#include "architecture.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"

// TODO destructor for previously held value.
// TODO here and everywhere else: choose a canonical module to add these
// fucntions to.

namespace type {
using base::check::Is;
void Array::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                       Context *ctx) const {
  ASSERT(from_type, Is<Array>());
  auto *from_array_type = &from_type->as<Array>();

  std::unique_lock lock(mtx_);
  auto *&fn = assign_fns_[from_array_type];
  if (fn == nullptr) {
    fn = ctx->mod_->AddFunc(
        type::Func({from_type, type::Ptr(this)}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      auto val                = fn->Argument(0);
      auto var                = fn->Argument(1);

      auto len = [&]() -> ir::RegisterOr<i32> {
        if (from_array_type->fixed_length) {
          return static_cast<i32>(from_array_type->len);
        }
        return ir::Load<i32>(ir::ArrayLength(val));
      }();

      auto *from_ptr_type = type::Ptr(from_type->as<type::Array>().data_type);
      auto from_ptr       = ir::Index(from_type, val, 0);
      auto from_end_ptr   = ir::PtrIncr(from_ptr, len, from_ptr_type);

      if (!fixed_length) {
        ComputeDestroyWithoutLock(ctx);
        ir::Arguments call_args;
        call_args.append(var);
        call_args.type_ = destroy_func_->type_;
        ir::Call(ir::AnyFunc{destroy_func_}, std::move(call_args));

        // TODO Architecture dependence?
        auto ptr = ir::Malloc(
            data_type, Architecture::InterprettingMachine().ComputeArrayLength(
                           len, data_type));
        ir::Store(len, ir::ArrayLength(var));
        ir::Store(ptr, ir::ArrayData(var, type::Ptr(this)));
      }

      auto *to_ptr_type   = type::Ptr(data_type);
      ir::Register to_ptr = ir::Index(type::Ptr(this), var, 0);

      using tup =
          std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<ir::Addr>>;
      ir::CreateLoop(
          [&](tup const &phis) {
            return ir::Eq(std::get<0>(phis), from_end_ptr);
          },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            ASSERT(std::get<1>(phis).is_reg_);

            ir::Register ptr_fixed_reg =
                from_type->as<type::Array>().data_type->is_big()
                    ? std::get<0>(phis).reg_
                    : ir::Load(std::get<0>(phis).reg_, data_type);
            auto ptr_fixed_type =
                !from_type->as<type::Array>().data_type->is_big()
                    ? from_type->as<type::Array>().data_type
                    : type::Ptr(from_type->as<type::Array>().data_type);

            EmitCopyInit(from_array_type->data_type, data_type,
                         ir::Val::Reg(ptr_fixed_reg, ptr_fixed_type),
                         std::get<1>(phis).reg_, ctx);
            return std::make_tuple(
                ir::PtrIncr(std::get<0>(phis).reg_, 1, from_ptr_type),
                ir::PtrIncr(std::get<1>(phis).reg_, 1, to_ptr_type));
          },
          std::tuple<type::Type const *, type::Type const *>{from_ptr_type,
                                                             to_ptr_type},
          tup{from_ptr, to_ptr});
      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = fn->type_;
  ir::Call(ir::AnyFunc{fn}, std::move(call_args));
}

void Pointer::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                         Context *ctx) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::Addr>(), to);
}

void Enum::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                      Context *ctx) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::EnumVal>(), to);
}

void Flags::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                       Context *ctx) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::FlagsVal>(), to);
}

void Variant::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                         Context *ctx) const {
  if (from_type->is<Variant>()) {
    // TODO find the best match for variant types. For instance, we allow
    // assignments like:
    // [3; int] | [4; bool] -> [--; int] | [--; bool]
    auto actual_type = ir::Load<type::Type const *>(
        ir::VariantType(std::get<ir::Register>(from.value)));
    auto landing = ir::Func::Current->AddBlock();
    for (Type const *v : from_type->as<Variant>().variants_) {
      auto next_block = ir::Func::Current->AddBlock();
      ir::BasicBlock::Current =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      v->EmitAssign(
          v,
          ir::Val::Reg(
              ir::PtrFix(
                  ir::VariantValue(v, std::get<ir::Register>(from.value)), v),
              v),
          ir::VariantValue(v, to), ctx);
      ir::UncondJump(landing);
      ir::BasicBlock::Current = next_block;
    }
    ir::UncondJump(landing);
    ir::BasicBlock::Current = landing;
  } else {
    ir::Store(from_type, ir::VariantType(to));
    // TODO Find the best match amongst the variants available.
    Type const *best_match = from_type;
    best_match->EmitAssign(from_type, from, ir::VariantValue(best_match, to),
                           ctx);
  }
}

void Struct::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                        Context *ctx) const {
  std::unique_lock lock(mtx_);
  ASSERT(this == from_type);
  if (!assign_func) {
    assign_func = ctx->mod_->AddFunc(
        type::Func({from_type, type::Ptr(this)}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});

    CURRENT_FUNC(assign_func) {
      ir::BasicBlock::Current = assign_func->entry();
      auto val                = assign_func->Argument(0);
      auto var                = assign_func->Argument(1);

      for (size_t i = 0; i < fields_.size(); ++i) {
        auto *field_type = from_type->as<type::Struct>().fields_.at(i).type;
        fields_[i].type->EmitAssign(
            fields_[i].type,
            ir::Val::Reg(ir::PtrFix(ir::Field(val, this, i), field_type),
                         field_type),
            ir::Field(var, this, i), ctx);
      }

      ir::ReturnJump();
    }
  }
  ASSERT(assign_func != nullptr);
  ir::Arguments call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = assign_func->type_;
  ir::Call(ir::AnyFunc{assign_func}, std::move(call_args));
}

void Function::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                          Context *ctx) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::Func *>(), to);
}
void Primitive::EmitAssign(Type const *from_type, ir::Val from, ir::Register to,
                           Context *ctx) const {
  ASSERT(this == from_type);
  switch (this->type_) {
    case PrimType::Err: UNREACHABLE(this, ": Err");
    case PrimType::Type_:
      ir::Store(from.reg_or<type::Type const *>(), to);
      break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Bool: ir::Store(from.reg_or<bool>(), to); break;
    case PrimType::Char: ir::Store(from.reg_or<char>(), to); break;
    case PrimType::Int8: ir::Store(from.reg_or<i8>(), to); break;
    case PrimType::Int16: ir::Store(from.reg_or<i16>(), to); break;
    case PrimType::Int32: ir::Store(from.reg_or<i32>(), to); break;
    case PrimType::Int64: ir::Store(from.reg_or<i64>(), to); break;
    case PrimType::Nat8: ir::Store(from.reg_or<u8>(), to); break;
    case PrimType::Nat16: ir::Store(from.reg_or<u16>(), to); break;
    case PrimType::Nat32: ir::Store(from.reg_or<u32>(), to); break;
    case PrimType::Nat64: ir::Store(from.reg_or<u64>(), to); break;
    case PrimType::Float32: ir::Store(from.reg_or<float>(), to); break;
    case PrimType::Float64: ir::Store(from.reg_or<double>(), to); break;
    default: UNREACHABLE();
  }
}

void CharBuffer::EmitAssign(Type const *from_type, ir::Val from,
                            ir::Register to, Context *ctx) const {
  // TODO Only callable at compile-time?
  NOT_YET();
}
}  // namespace type
