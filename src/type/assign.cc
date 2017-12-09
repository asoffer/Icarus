#include "type.h"

#include "../architecture.h"
#include "../ir/ir.h"

extern IR::Val PtrCallFix(IR::Val v);

void Type::CallAssignment(Type *from_type, Type *to_type, IR::Val from_val,
                          IR::Val to_var) {

  if (to_type->is<Primitive>() || to_type->is<Pointer>() ||
      to_type->is<Function>() || to_type->is<Enum>()) {
    ASSERT_EQ(from_type, to_type);
    IR::Store(from_val, to_var);

  } else if (to_type->is<Array>()) {
    ASSERT_TYPE(Array, from_type);
    auto *from_array_type = ptr_cast<Array>(from_type);
    auto *to_array_type   = ptr_cast<Array>(to_type);

    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func({Ptr(from_type), Ptr(to_type)}, Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}}));
    auto *assign_func = IR::Func::All.back().get(); // TODO cache this
    assign_func->name = "assign." + Mangle(from_type) + Mangle(to_type);

    CURRENT_FUNC(assign_func) {
      IR::Block::Current = assign_func->entry();
      auto val           = assign_func->Argument(0);
      auto var           = assign_func->Argument(1);
      IR::Val len        = from_array_type->fixed_length
                        ? IR::Val::Uint(from_array_type->len)
                        : IR::Load(IR::ArrayLength(val));
      IR::Val from_ptr     = IR::Index(val, IR::Val::Uint(0));
      IR::Val from_end_ptr = IR::PtrIncr(from_ptr, len);

      if (!to_array_type->fixed_length) {
        to_array_type->EmitDestroy(var);
        // TODO Architecture dependence?
        auto to_bytes = Architecture::InterprettingMachine().ComputeArrayLength(
            len, to_array_type->data_type);
        auto ptr = IR::Malloc(to_array_type->data_type, to_bytes);
        IR::Store(len, IR::ArrayLength(var));
        IR::Store(ptr, IR::ArrayData(var));
      }

      IR::Val to_ptr = IR::Index(var, IR::Val::Uint(0));

      auto init_block = IR::Block::Current;
      auto exit_block = IR::Func::Current->AddBlock();
      auto loop_phi   = IR::Func::Current->AddBlock();
      auto loop_body  = IR::Func::Current->AddBlock();
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto from_phi      = IR::Phi(Ptr(from_array_type->data_type));
      auto to_phi        = IR::Phi(Ptr(to_array_type->data_type));
      auto from_phi_reg  = IR::Func::Current->Command(from_phi).reg();
      auto to_phi_reg    = IR::Func::Current->Command(to_phi).reg();

      IR::CondJump(IR::Eq(from_phi_reg, from_end_ptr), exit_block, loop_body);

      IR::Block::Current = loop_body;
      EmitCopyInit(from_array_type->data_type, to_array_type->data_type,
                   PtrCallFix(from_phi_reg), to_phi_reg);

      IR::UncondJump(loop_phi);

      IR::Func::Current->SetArgs(
          from_phi, {IR::Val::Block(init_block), from_ptr,
                     IR::Val::Block(IR::Block::Current),
                     IR::PtrIncr(from_phi_reg, IR::Val::Uint(1ul))});
      IR::Func::Current->SetArgs(to_phi,
                                 {IR::Val::Block(init_block), to_ptr,
                                  IR::Val::Block(IR::Block::Current),
                                  IR::PtrIncr(to_phi_reg, IR::Val::Uint(1ul))});

      IR::Block::Current = exit_block;
      IR::ReturnJump();
    }
    IR::Call(IR::Val::Func(assign_func), {from_val, to_var});

  } else if (from_type->is<Scope_Type>() && to_type->is<Scope_Type>()) {
    ASSERT_EQ(from_type, to_type);
    IR::Store(from_val, to_var);

  } else if (to_type->is<Variant>()) {
    // TODO this way of determining types only works for primitives.
    auto to_index_ptr = IR::Cast(IR::Val::Type(Ptr(Type_)), to_var);

    // TODO from_type needs to be the right thing.
    if (from_val.type->is<Pointer>() &&
        from_val.type->as<Pointer>().pointee->is<Variant>()) {
      // TODO jump table?

      auto landing = IR::Func::Current->AddBlock();
      auto type    = IR::Load(IR::Cast(IR::Val::Type(Ptr(Type_)), from_val));
      for (Type *v : from_type->as<Variant>().variants_) {
        auto found_block = IR::Func::Current->AddBlock();
        auto next_block  = IR::Func::Current->AddBlock();
        // TODO just testing for equality may not be right. from_type may not
        // actually be in the variant, just castable to something in the
        // variant.
        IR::CondJump(IR::Eq(type, IR::Val::Type(v)), found_block, next_block);

        IR::Block::Current = found_block;
        CallAssignment(Type_, Type_, IR::Val::Type(v), to_index_ptr);
        auto to_data_ptr = IR::Cast(
            IR::Val::Type(Ptr(v)), IR::PtrIncr(to_index_ptr, IR::Val::Uint(1)));
        CallAssignment(from_val.type, from_val.type, from_val, to_data_ptr);

        IR::UncondJump(landing);

        IR::Block::Current = next_block;
      }
      IR::UncondJump(landing);
      IR::Block::Current = landing;
    } else {
      CallAssignment(Type_, Type_, IR::Val::Type(from_val.type), to_index_ptr);
      auto to_data_ptr = IR::Cast(IR::Val::Type(Ptr(from_val.type)),
                                  IR::PtrIncr(to_index_ptr, IR::Val::Uint(1)));
      CallAssignment(from_val.type, from_val.type, from_val, to_data_ptr);
    }
  } else {
    // TODO change name? this is the only assignment?
    from_type->as<Struct>().EmitDefaultAssign(to_var, from_val);
  }
}

void Struct::EmitDefaultAssign(IR::Val to_var, IR::Val from_val) {
  CompleteDefinition();
  if (!assign_func) {
    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func({this, Ptr(this)}, Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"from", nullptr}, {"to", nullptr}}));
    assign_func       = IR::Func::All.back().get();
    assign_func->name = "assign." + Mangle(this);

    CURRENT_FUNC(assign_func) {
      IR::Block::Current = assign_func->entry();
      auto val           = assign_func->Argument(0);
      auto var           = assign_func->Argument(1);

      for (size_t i = 0; i < field_type.size(); ++i) {
        auto the_field_type = field_type AT(i);
        // TODO is that the right scope?
        Type::CallAssignment(the_field_type, the_field_type,
                             PtrCallFix(IR::Field(val, i)), IR::Field(var, i));
      }

      IR::ReturnJump();
    }
  }
  ASSERT(assign_func, "");

  IR::Call(IR::Val::Func(assign_func), {to_var, from_val});
}
