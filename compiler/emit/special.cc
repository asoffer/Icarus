#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/instruction/instructions.h"

// TODO: Currently inserting these always at the root. There are a couple of
// reason why this is wrong. First, If this is generated due to a temporary
// subcontext, we probably want to drop it, as we can't verify the constructed
// type is valid in any way. Second, For types like arrays of primitives, we
// only need to generate the code for them once, rather than per module.

namespace compiler {
namespace {
enum AssignmentKind { Move, Copy };

template <AssignmentKind K>
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
    fn->WriteByteCode<interpretter::instruction_set_t>();
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
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  current_block()->Append(ir::DestroyInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Array> const &to,
    type::Typed<ir::Value> const &from) {
  auto [fn, inserted] =
      context().root().InsertCopyAssign(to.type(), from.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      ASSERT(from.type().is<type::Array>() == true);
      EmitArrayAssignment<Copy>(*this, to.type(),
                                &from.type()->as<type::Array>());
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  builder().Copy(to, type::Typed<ir::Reg>(from->get<ir::Reg>(), from.type()));
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Array> const &to,
    type::Typed<ir::Value> const &from) {
  auto [fn, inserted] =
      context().root().InsertMoveAssign(to.type(), from.type());
  if (inserted) {
    ICARUS_SCOPE(ir::SetCurrent(fn, builder())) {
      ASSERT(from.type().is<type::Array>() == true);
      EmitArrayAssignment<Move>(*this, to.type(),
                                &from.type()->as<type::Array>());
    }
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  builder().Move(to, type::Typed<ir::Reg>(from->get<ir::Reg>(), from.type()));
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Enum> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::EnumVal>>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Enum> const &to,
    type::Typed<ir::Value> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Flags> const &r) {
  builder().Store(ir::FlagsVal{0}, *r);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Flags> const &to,
    type::Typed<ir::Value> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::RegOr<ir::FlagsVal>>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Flags> const &to,
    type::Typed<ir::Value> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Pointer> const &r) {
  builder().Store(ir::Addr::Null(), *r);
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
          if (field.type == type::Int64) {
            EmitCopyInit(
                type::Typed<ir::Value>(field.initial_value, field.type),
                builder().FieldRef(var, r.type(), i));
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
    fn->WriteByteCode<interpretter::instruction_set_t>();
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
    fn->WriteByteCode<interpretter::instruction_set_t>();
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
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }
  current_block()->Append(ir::DestroyInstruction{.type = r.type(), .reg = *r});
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
    fn->WriteByteCode<interpretter::instruction_set_t>();
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
    fn->WriteByteCode<interpretter::instruction_set_t>();
  }

  builder().Copy(to, type::Typed<ir::Reg>(from->get<ir::Reg>(), from.type()));
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

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Struct> const &to,
    type::Typed<ir::Value> const &from) {
  // TODO: Support mixed types and user-defined assignments.
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::CopyInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::Addr>, type::Struct> const &to,
    type::Typed<ir::Value> const &from) {
  current_block()->Append(ir::MoveInstruction{
      .type = to.type(), .from = from->get<ir::Reg>(), .to = *to});
}

}  // namespace compiler
