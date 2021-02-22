#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/instructions.h"
#include "ir/instruction/instructions.h"

// TODO: Currently inserting these always at the root. There are a couple of
// reason why this is wrong. First, If this is generated due to a temporary
// subcontext, we probably want to drop it, as we can't verify the constructed
// type is valid in any way. Second, For types like arrays of primitives, we
// only need to generate the code for them once, rather than per module.

namespace compiler {
namespace {
enum Kind{ Move, Copy };


template <Kind K>
void EmitArrayAssignment(Compiler &c, type::Array const *to,
                         type::Array const *from) {
  auto &bldr          = c.builder();
  auto &fn            = *bldr.CurrentGroup();
  bldr.CurrentBlock() = fn.entry();
  auto var            = ir::Reg::Arg(0);
  auto val            = ir::Reg::Arg(1);

  auto to_data_ptr_type   = type::Ptr(to->data_type());
  auto from_data_ptr_type = type::Ptr(from->data_type());

  auto from_ptr = bldr.PtrIncr(val, 0, from_data_ptr_type);
  auto from_end_ptr =
      bldr.PtrIncr(from_ptr, from->length(), from_data_ptr_type);
  auto to_ptr = bldr.PtrIncr(var, 0, to_data_ptr_type);

  auto *loop_body  = bldr.AddBlock();
  auto *land_block = bldr.AddBlock();
  auto *cond_block = bldr.AddBlock();

  bldr.UncondJump(cond_block);

  bldr.CurrentBlock() = cond_block;
  auto *from_phi      = bldr.PhiInst<ir::Addr>();
  auto *to_phi        = bldr.PhiInst<ir::Addr>();
  bldr.CondJump(bldr.Eq(ir::RegOr<ir::Addr>(from_phi->result), from_end_ptr),
                land_block, loop_body);

  bldr.CurrentBlock() = loop_body;
  if constexpr (K == Copy) {
    c.EmitCopyAssign(
        type::Typed<ir::RegOr<ir::Addr>>(to_phi->result, to->data_type()),
        type::Typed<ir::Value>(
            ir::Value(c.builder().PtrFix(from_phi->result, from->data_type())),
            from->data_type()));
  } else if constexpr (K == Move) {
    c.EmitMoveAssign(
        type::Typed<ir::RegOr<ir::Addr>>(to_phi->result, to->data_type()),
        type::Typed<ir::Value>(
            ir::Value(c.builder().PtrFix(from_phi->result, from->data_type())),
            from->data_type()));
  } else {
    UNREACHABLE();
  }

  ir::Reg next_to   = bldr.PtrIncr(to_phi->result, 1, to_data_ptr_type);
  ir::Reg next_from = bldr.PtrIncr(from_phi->result, 1, from_data_ptr_type);
  bldr.UncondJump(cond_block);

  to_phi->add(fn.entry(), to_ptr);
  to_phi->add(bldr.CurrentBlock(), next_to);
  from_phi->add(fn.entry(), from_ptr);
  from_phi->add(bldr.CurrentBlock(), next_from);

  bldr.CurrentBlock() = land_block;
  bldr.ReturnJump();
}

template <Kind K>
void EmitArrayInit(Compiler &c, type::Array const *to,
                   type::Array const *from) {
  auto &bldr          = c.builder();
  auto &fn            = *bldr.CurrentGroup();
  bldr.CurrentBlock() = fn.entry();
  auto var            = ir::Reg::Arg(0);
  auto ret            = bldr.GetRet(0, from->data_type());

  auto from_data_ptr_type = type::Ptr(from->data_type());

  auto from_ptr = bldr.PtrIncr(var, 0, from_data_ptr_type);
  auto from_end_ptr =
      bldr.PtrIncr(from_ptr, from->length(), from_data_ptr_type);
  auto to_ptr = bldr.PtrIncr(ret, 0, from_data_ptr_type);

  auto *loop_body  = bldr.AddBlock();
  auto *land_block = bldr.AddBlock();
  auto *cond_block = bldr.AddBlock();

  bldr.UncondJump(cond_block);

  bldr.CurrentBlock() = cond_block;
  auto *from_phi      = bldr.PhiInst<ir::Addr>();
  auto *to_phi        = bldr.PhiInst<ir::Addr>();
  bldr.CondJump(bldr.Eq(ir::RegOr<ir::Addr>(from_phi->result), from_end_ptr),
                land_block, loop_body);

  bldr.CurrentBlock() = loop_body;
  if constexpr (K == Copy) {
    c.EmitCopyInit(
        type::Typed<ir::Reg>(to_phi->result, to->data_type()),
        type::Typed<ir::Value>(
            ir::Value(c.builder().PtrFix(from_phi->result, from->data_type())),
            from->data_type()));
  } else if constexpr (K == Move) {
    c.EmitMoveInit(
        type::Typed<ir::Reg>(to_phi->result, to->data_type()),
        type::Typed<ir::Value>(
            ir::Value(c.builder().PtrFix(from_phi->result, from->data_type())),
            from->data_type()));
  } else {
    UNREACHABLE();
  }

  ir::Reg next_to   = bldr.PtrIncr(to_phi->result, 1, from_data_ptr_type);
  ir::Reg next_from = bldr.PtrIncr(from_phi->result, 1, from_data_ptr_type);
  bldr.UncondJump(cond_block);

  to_phi->add(fn.entry(), to_ptr);
  to_phi->add(bldr.CurrentBlock(), next_to);
  from_phi->add(fn.entry(), from_ptr);
  from_phi->add(bldr.CurrentBlock(), next_from);

  bldr.CurrentBlock() = land_block;
  bldr.ReturnJump();
}

}  // namespace

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Array> const &r) {
  auto [fn, inserted] = context().root().InsertInit(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      builder().CurrentBlock() = fn->entry();
      builder().OnEachArrayElement(r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
        EmitDefaultInit(type::Typed<ir::Reg>(reg, r.type()->data_type()));
      });
      builder().ReturnJump();
    }
    fn->WriteByteCode<instruction_set_t>();
    // TODO: Remove const_cast.
    const_cast<type::Array *>(r.type())->SetInitializer(fn);
  }

  current_block()->Append(ir::InitInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Array> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  auto [fn, inserted] = context().root().InsertDestroy(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      builder().CurrentBlock() = fn->entry();
      builder().OnEachArrayElement(r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
        EmitDestroy(type::Typed<ir::Reg>(reg, r.type()->data_type()));
      });
      builder().ReturnJump();
    }
    fn->WriteByteCode<instruction_set_t>();
    // TODO: Remove const_cast.
    const_cast<type::Array *>(r.type())->SetDestructor(fn);
  }
  current_block()->Append(ir::DestroyInstruction{.type = r.type(), .reg = *r});
}

void SetArrayInits(Compiler &c, type::Array const *array_type) {
  auto [copy_fn, copy_inserted] =
      c.context().root().InsertCopyInit(array_type, array_type);
  auto [move_fn, move_inserted] =
      c.context().root().InsertMoveInit(array_type, array_type);
  ASSERT(copy_inserted == move_inserted);
  if (copy_inserted) {
    ICARUS_SCOPE(ir::SetCurrent(copy_fn, c.builder())) {
      EmitArrayInit<Copy>(c, array_type, array_type);
    }
    ICARUS_SCOPE(ir::SetCurrent(move_fn, c.builder())) {
      EmitArrayInit<Move>(c, array_type, array_type);
    }

    copy_fn->WriteByteCode<instruction_set_t>();
    move_fn->WriteByteCode<instruction_set_t>();
    // TODO: Remove const_cast.
    const_cast<type::Array *>(array_type)->SetInits(copy_fn, move_fn);
  }
}

void SetArrayAssignments(Compiler &c, type::Array const *array_type) {
  auto [copy_fn, copy_inserted] =
      c.context().root().InsertCopyAssign(array_type, array_type);
  auto [move_fn, move_inserted] =
      c.context().root().InsertMoveAssign(array_type, array_type);
  ASSERT(copy_inserted == move_inserted);
  if (copy_inserted) {
    ICARUS_SCOPE(ir::SetCurrent(copy_fn, c.builder())) {
      EmitArrayAssignment<Copy>(c, array_type, array_type);
    }
    ICARUS_SCOPE(ir::SetCurrent(move_fn, c.builder())) {
      EmitArrayAssignment<Move>(c, array_type, array_type);
    }

    copy_fn->WriteByteCode<instruction_set_t>();
    move_fn->WriteByteCode<instruction_set_t>();
    // TODO: Remove const_cast.
    const_cast<type::Array *>(array_type)->SetAssignments(copy_fn, move_fn);
  }
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Array> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayInits(*this, to.type());
  current_block()->Append(ir::MoveInitInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Array> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayInits(*this, to.type());
  current_block()->Append(ir::CopyInitInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Array> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayAssignments(*this, &to.type()->as<type::Array>());
  builder().Copy(to, type::Typed<ir::Reg>(from->get<ir::Reg>(), from.type()));
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Array> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayAssignments(*this, &to.type()->as<type::Array>());
  builder().Move(to, type::Typed<ir::Reg>(from->get<ir::Reg>(), from.type()));
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Enum> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<type::Enum::underlying_type>>(), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Enum> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<type::Enum::underlying_type>>(), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Enum> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<type::Enum::underlying_type>>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Enum> const &to,
    type::Typed<ir::Value> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Flags> const &r) {
  builder().Store(type::Flags::underlying_type{0}, *r);
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Flags> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<type::Flags::underlying_type>>(), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Flags> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<type::Flags::underlying_type>>(), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Flags> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<type::Flags::underlying_type>>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Flags> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<type::Flags::underlying_type>>(), *to);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Pointer> const &r) {
  builder().Store(ir::Addr::Null(), *r);
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Pointer> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(from.type().is<type::Pointer>() == true);
  ASSERT(from.type().as<type::Pointer>().pointee() == to.type()->pointee());
  builder().Store(from->get<ir::RegOr<ir::Addr>>(), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Pointer> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::Addr>>(), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Pointer> const &to,
    type::Typed<ir::Value> const &from) {
  if (type::Type(to.type()) == from.type()) {
    builder().Store(from->get<ir::RegOr<ir::Addr>>(), *to);
  } else if (from.type() == type::NullPtr) {
    builder().Store(ir::Addr::Null(), *to);
  } else {
    UNREACHABLE(to, ": ", to.type(), " - ", from.type());
  }
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Pointer> const &to,
    type::Typed<ir::Value> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(
    type::Typed<ir::Reg, type::BufferPointer> const &r) {
  EmitDefaultInit(static_cast<type::Typed<ir::Reg, type::Pointer>>(r));
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::BufferPointer> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::Addr>>(), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::BufferPointer> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::Addr>>(), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::BufferPointer> const &to,
    type::Typed<ir::Value> const &from) {
  EmitCopyAssign(
      static_cast<type::Typed<ir::RegOr<ir::Addr>, type::Pointer>>(to), from);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::BufferPointer> const &to,
    type::Typed<ir::Value> const &from) {
  EmitMoveAssign(
      static_cast<type::Typed<ir::RegOr<ir::Addr>, type::Pointer>>(to), from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Primitive> const &r) {
  r.type()->Apply([&]<typename T>() { builder().Store(T{}, *r); });
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Primitive> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  to.type()->Apply([&]<typename T>() {
    builder().Store(from->template get<ir::RegOr<T>>(), *to);
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Primitive> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  to.type()->Apply([&]<typename T>() {
    builder().Store(from->template get<ir::RegOr<T>>(), *to);
  });
}

// TODO: Determine if you want to treat mixed integer assignment as an implicit
// cast or as an overload set.
void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Primitive> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  to.type()->Apply([&]<typename T>() {
    builder().Store(from->template get<ir::RegOr<T>>(), *to);
  });
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Primitive> const &to,
    type::Typed<ir::Value> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Struct> const &r) {
  auto [fn, inserted] = context().root().InsertInit(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();
      auto var                 = ir::Reg::Arg(0);

      for (size_t i = 0; i < r.type()->fields().size(); ++i) {
        auto &field = r.type()->fields()[i];
        if (not field.initial_value.empty()) {
          // TODO: Support other initial value types.
          if (field.type == type::I64) {
            EmitCopyInit(
                builder().FieldRef(var, r.type(), i),
                type::Typed<ir::Value>(field.initial_value, field.type));
          } else {
            NOT_YET();
          }
        } else {
          EmitDefaultInit(
              type::Typed<ir::Reg>(builder().FieldRef(var, r.type(), i)));
        }
      }

      builder().ReturnJump();
    }
    // TODO: Remove this hack.
    const_cast<type::Struct *>(r.type())->init_ = fn;
    fn->WriteByteCode<instruction_set_t>();
  }
  current_block()->Append(ir::InitInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Struct> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  current_block()->Append(ir::DestroyInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Tuple> const &r) {
  auto [fn, inserted] = context().root().InsertInit(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();

      for (size_t i = 0; i < r.type()->size(); ++i) {
        EmitDefaultInit(type::Typed<ir::Reg>(
            builder().FieldRef(ir::Reg::Arg(0), r.type(), i)));
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<instruction_set_t>();
  }
  current_block()->Append(ir::InitInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Tuple> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  auto [fn, inserted] = context().root().InsertDestroy(r.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      builder().CurrentBlock() = builder().CurrentGroup()->entry();

      for (size_t i = 0; i < r.type()->size(); ++i) {
        EmitDestroy(type::Typed<ir::Reg>(
            builder().FieldRef(ir::Reg::Arg(0), r.type(), i)));
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<instruction_set_t>();
  }
  current_block()->Append(ir::DestroyInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Tuple> to,
                            type::Typed<ir::Value> const &from) {
  NOT_YET();
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Tuple> to,
                            type::Typed<ir::Value> const &from) {
  NOT_YET();
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Tuple> const &to,
    type::Typed<ir::Value> const &from) {
  auto [fn, inserted] =
      context().root().InsertMoveAssign(to.type(), from.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      builder().CurrentBlock() = fn->entry();
      auto var                 = ir::Reg::Arg(0);
      auto val                 = ir::Reg::Arg(1);
      for (size_t i = 0; i < to.type()->size(); ++i) {
        EmitMoveAssign(builder().FieldRef(var, to.type(), i),
                       builder().FieldValue(val, to.type(), i));
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<instruction_set_t>();
  }

  builder().Move(to, type::Typed<ir::Reg>(from->get<ir::Reg>(), from.type()));
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Tuple> const &to,
    type::Typed<ir::Value> const &from) {
  auto [fn, inserted] =
      context().root().InsertCopyAssign(to.type(), from.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      builder().CurrentBlock() = fn->entry();
      auto var                 = ir::Reg::Arg(0);
      auto val                 = ir::Reg::Arg(1);
      for (size_t i = 0; i < to.type()->size(); ++i) {
        EmitCopyAssign(builder().FieldRef(var, to.type(), i),
                       builder().FieldValue(val, to.type(), i));
      }

      builder().ReturnJump();
    }
    fn->WriteByteCode<instruction_set_t>();
  }

  builder().Copy(to, type::Typed<ir::Reg>(from->get<ir::Reg>(), from.type()));
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Function> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::Fn>>(), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Function> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::Fn>>(), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Function> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::Fn>>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Function> const &to,
    type::Typed<ir::Value> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Struct> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::MoveInitInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Struct> to,
                            type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::CopyInitInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Struct> const &to,
    type::Typed<ir::Value> const &from) {
  // TODO: Support mixed types and user-defined assignments.
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::CopyInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Struct> const &to,
    type::Typed<ir::Value> const &from) {
  current_block()->Append(ir::MoveInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Slice> to,
                            type::Typed<ir::Value> const &from) {
  builder().Store(builder().Load<ir::Addr>(
                      current_block()->Append(type::SliceDataInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceDataInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  builder().Store(builder().Load<type::Slice::length_t>(
                      current_block()->Append(type::SliceLengthInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Slice> to,
                            type::Typed<ir::Value> const &from) {
  builder().Store(builder().Load<ir::Addr>(
                      current_block()->Append(type::SliceDataInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceDataInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  builder().Store(builder().Load<type::Slice::length_t>(
                      current_block()->Append(type::SliceLengthInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Slice> const &to,
    type::Typed<ir::Value> const &from) {
  builder().Store(builder().Load<ir::Addr>(
                      current_block()->Append(type::SliceDataInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceDataInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  builder().Store(builder().Load<type::Slice::length_t>(
                      current_block()->Append(type::SliceLengthInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  current_block()->load_store_cache().clear();
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Slice> const &to,
    type::Typed<ir::Value> const &from) {
  builder().Store(builder().Load<ir::Addr>(
                      current_block()->Append(type::SliceDataInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceDataInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  builder().Store(builder().Load<type::Slice::length_t>(
                      current_block()->Append(type::SliceLengthInstruction{
                          .slice  = from->get<ir::RegOr<ir::Addr>>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  current_block()->load_store_cache().clear();
}

}  // namespace compiler
