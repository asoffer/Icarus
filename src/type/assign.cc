#include "all.h"

#include "../architecture.h"
#include "../context.h"
#include "../ir/func.h"

// TODO destructor for previously held value.

namespace type {
void Array::EmitAssign(const Type *from_type, IR::Val from,
                       IR::Val to) const {
  ASSERT_TYPE(Array, from_type);
  auto *from_array_type = &from_type->as<Array>();

  auto *&fn = assign_fns_[from_array_type];
  if (fn == nullptr) {
    auto assign_func = std::make_unique<IR::Func>(
        Func({from_type, Ptr(this)}, Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});
    fn = assign_func.get();
    IR::Func::All.push_back(std::move(assign_func));

    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      auto val           = fn->Argument(0);
      auto var           = fn->Argument(1);
      IR::Val len        = from_array_type->fixed_length
                        ? IR::Val::Int(static_cast<i32>(from_array_type->len))
                        : IR::Load(IR::ArrayLength(val));
      IR::Val from_ptr     = IR::Index(val, IR::Val::Int(0));
      IR::Val from_end_ptr = IR::PtrIncr(from_ptr, len);

      if (!fixed_length) {
        EmitDestroy(var);
        // TODO Architecture dependence?
        auto to_bytes = Architecture::InterprettingMachine().ComputeArrayLength(
            len, data_type);
        auto ptr = IR::Malloc(data_type, to_bytes);
        IR::Store(len, IR::ArrayLength(var));
        IR::Store(ptr, IR::ArrayData(var));
      }

      IR::Val to_ptr = IR::Index(var, IR::Val::Int(0));

      auto exit_block = IR::Func::Current->AddBlock();
      auto init_block = IR::Block::Current;
      auto loop_phi   = IR::Func::Current->AddBlock();
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto from_phi      = IR::Phi(Ptr(from_array_type->data_type));
      auto to_phi        = IR::Phi(Ptr(data_type));
      auto from_phi_reg  = IR::Func::Current->Command(from_phi).reg();
      auto to_phi_reg    = IR::Func::Current->Command(to_phi).reg();

      IR::Block::Current =
          IR::EarlyExitOn<true>(exit_block, IR::Eq(from_phi_reg, from_end_ptr));
      // Loop body

      EmitCopyInit(from_array_type->data_type, data_type,
                   PtrCallFix(from_phi_reg), to_phi_reg);

      IR::Func::Current->SetArgs(
          from_phi, {IR::Val::Block(init_block), from_ptr,
                     IR::Val::Block(IR::Block::Current),
                     IR::PtrIncr(from_phi_reg, IR::Val::Int(1ul))});
      IR::Func::Current->SetArgs(to_phi,
                                 {IR::Val::Block(init_block), to_ptr,
                                  IR::Val::Block(IR::Block::Current),
                                  IR::PtrIncr(to_phi_reg, IR::Val::Int(1ul))});
      IR::UncondJump(loop_phi);

      IR::Block::Current = exit_block;
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(fn), {from, to}, {});
}

void Tuple::EmitAssign(const Type *, IR::Val, IR::Val) const {
  NOT_YET();
}

void Pointer::EmitAssign(const Type *from_type, IR::Val from,
                               IR::Val to) const {
  ASSERT_EQ(this, from_type);
  IR::Store(from, to);
}

void Range::EmitAssign(const Type *, IR::Val, IR::Val) const { NOT_YET(); }
void Slice::EmitAssign(const Type *, IR::Val, IR::Val) const { NOT_YET(); }
void Scope::EmitAssign(const Type *from_type, IR::Val from, IR::Val to) const {
  ASSERT_EQ(this, from_type);
  IR::Store(from, to);
}

void Enum::EmitAssign(const Type *from_type, IR::Val from, IR::Val to) const {
  ASSERT_EQ(this, from_type);
  IR::Store(from, to);
}

void Variant::EmitAssign(const Type *from_type, IR::Val from,
                         IR::Val to) const {
  if (from_type->is<Variant>()) {
    // TODO find the best match for variant types. For instance, we allow
    // assignments like:
    // [3; int] | [4; bool] -> [--; int] | [--; bool]
    auto actual_type = IR::Load(IR::VariantType(from));
    auto landing     = IR::Func::Current->AddBlock();
    for (const Type *v : from_type->as<Variant>().variants_) {
      auto next_block    = IR::Func::Current->AddBlock();
      IR::Block::Current = IR::EarlyExitOn<false>(
          next_block, IR::Eq(actual_type, IR::Val::Type(v)));
      IR::Store(IR::Val::Type(v), IR::VariantType(to));
      v->EmitAssign(v, PtrCallFix(IR::VariantValue(v, from)),
                    IR::VariantValue(v, to));
      IR::UncondJump(landing);
      IR::Block::Current = next_block;
    }
    IR::UncondJump(landing);
    IR::Block::Current = landing;
  } else {
    IR::Store(IR::Val::Type(from_type), IR::VariantType(to));
    // TODO Find the best match amongst the variants available.
    const Type *best_match = from_type;
    best_match->EmitAssign(from_type, from, IR::VariantValue(best_match, to));
  }
}

void Struct::EmitAssign(const Type *from_type, IR::Val from, IR::Val to) const {
  ASSERT_EQ(this, from_type);
  if (!assign_func) {
    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func({this, Ptr(this)}, type::Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}}));
    assign_func = IR::Func::All.back().get();

    CURRENT_FUNC(assign_func) {
      IR::Block::Current = assign_func->entry();
      auto val           = assign_func->Argument(0);
      auto var           = assign_func->Argument(1);

      for (size_t i = 0; i < fields_.size(); ++i) {
        // TODO is that the right scope?
        fields_[i].type->EmitAssign(
            fields_[i].type, PtrCallFix(IR::Field(val, i)), IR::Field(var, i));
      }

      IR::ReturnJump();
    }
  }
  ASSERT(assign_func, "");
  IR::Call(IR::Val::Func(assign_func), {from, to}, {});
}

void Function::EmitAssign(const Type *from_type, IR::Val from,
                          IR::Val to) const {
  ASSERT_EQ(this, from_type);
  IR::Store(from, to);
}
void Primitive::EmitAssign(const Type *from_type, IR::Val from,
                           IR::Val to) const {
  ASSERT_EQ(this, from_type);
  IR::Store(from, to);
}

} // namespace type
