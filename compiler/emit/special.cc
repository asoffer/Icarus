#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "ir/instruction/instructions.h"
#include "type/cast.h"

// TODO: Currently inserting these always at the root. There are a couple of
// reason why this is wrong. First, If this is generated due to a temporary
// subcontext, we probably want to drop it, as we can't verify the constructed
// type is valid in any way. Second, For types like arrays of primitives, we
// only need to generate the code for them once, rather than per module.

namespace compiler {
namespace {

enum Kind { Move, Copy };

template <Kind K>
void EmitArrayAssignment(Compiler &c, type::Array const *to,
                         type::Array const *from) {
  auto &fn          = *c.current().group;
  c.current_block() = fn.entry();
  auto to_ptr       = ir::Reg::Arg(0);
  auto from_ptr     = ir::Reg::Arg(1);

  auto to_data_ptr_type   = type::Ptr(to->data_type());
  auto from_data_ptr_type = type::Ptr(from->data_type());

  auto from_end_ptr = c.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = from_ptr,
                             .index  = from->length().value(),
                             .ptr    = from_data_ptr_type,
                             .result = c.current().group->Reserve()});

  auto *loop_body  = c.current().group->AppendBlock();
  auto *land_block = c.current().group->AppendBlock();
  auto *cond_block = c.current().group->AppendBlock();

  c.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  c.current_block() = cond_block;
  auto *from_phi    = PhiInst<ir::addr_t>(c.current());
  auto *to_phi      = PhiInst<ir::addr_t>(c.current());
  ir::Reg condition = c.current_block()->Append(
      ir::EqInstruction<ir::addr_t>{.lhs    = from_phi->result,
                                    .rhs    = from_end_ptr,
                                    .result = c.current().group->Reserve()});
  c.current_block()->set_jump(
      ir::JumpCmd::Cond(condition, land_block, loop_body));

  c.current_block() = loop_body;
  ir::PartialResultBuffer buffer;
  buffer.append(PtrFix(c.current(), from_phi->result, from->data_type()));
  type::Typed value_view(buffer[0], from->data_type());
  if constexpr (K == Copy) {
    c.EmitCopyAssign(
        type::Typed<ir::RegOr<ir::addr_t>>(to_phi->result, to->data_type()),
        value_view);
  } else if constexpr (K == Move) {
    c.EmitMoveAssign(
        type::Typed<ir::RegOr<ir::addr_t>>(to_phi->result, to->data_type()),
        value_view);
  } else {
    UNREACHABLE();
  }

  ir::Reg next_to = c.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = to_phi->result,
                             .index  = 1,
                             .ptr    = to_data_ptr_type,
                             .result = c.current().group->Reserve()});
  ir::Reg next_from = c.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = from_phi->result,
                             .index  = 1,
                             .ptr    = from_data_ptr_type,
                             .result = c.current().group->Reserve()});
  c.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  to_phi->add(fn.entry(), to_ptr);
  to_phi->add(c.current_block(), next_to);
  from_phi->add(fn.entry(), from_ptr);
  from_phi->add(c.current_block(), next_from);

  c.current_block() = land_block;
  land_block->set_jump(ir::JumpCmd::Return());
}

template <Kind K>
void EmitArrayInit(Compiler &c, type::Array const *to,
                   type::Array const *from) {
  auto &fn          = *c.current().group;
  c.current_block() = fn.entry();
  auto from_ptr     = ir::Reg::Arg(0);
  auto to_ptr       = ir::Reg::Out(0);

  auto from_data_ptr_type = type::Ptr(from->data_type());
  auto from_end_ptr       = c.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = from_ptr,
                             .index  = from->length().value(),
                             .ptr    = from_data_ptr_type,
                             .result = c.current().group->Reserve()});

  auto *loop_body  = c.current().group->AppendBlock();
  auto *land_block = c.current().group->AppendBlock();
  auto *cond_block = c.current().group->AppendBlock();

  c.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  c.current_block() = cond_block;
  auto *from_phi    = PhiInst<ir::addr_t>(c.current());
  auto *to_phi      = PhiInst<ir::addr_t>(c.current());
  ir::Reg condition = c.current_block()->Append(
      ir::EqInstruction<ir::addr_t>{.lhs    = from_phi->result,
                                    .rhs    = from_end_ptr,
                                    .result = c.current().group->Reserve()});
  c.current_block()->set_jump(
      ir::JumpCmd::Cond(condition, land_block, loop_body));

  c.current_block() = loop_body;
  ir::PartialResultBuffer buffer;
  buffer.append(PtrFix(c.current(), from_phi->result, from->data_type()));
  if constexpr (K == Copy) {
    c.EmitCopyInit(type::Typed<ir::Reg>(to_phi->result, to->data_type()),
                   buffer);

  } else if constexpr (K == Move) {
    c.EmitCopyInit(type::Typed<ir::Reg>(to_phi->result, to->data_type()),
                   buffer);
  } else {
    UNREACHABLE();
  }

  ir::Reg next_to = c.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = to_phi->result,
                             .index  = 1,
                             .ptr    = from_data_ptr_type,
                             .result = c.current().group->Reserve()});
  ir::Reg next_from = c.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = from_phi->result,
                             .index  = 1,
                             .ptr    = from_data_ptr_type,
                             .result = c.current().group->Reserve()});
  c.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  to_phi->add(fn.entry(), to_ptr);
  to_phi->add(c.current_block(), next_to);
  from_phi->add(fn.entry(), from_ptr);
  from_phi->add(c.current_block(), next_from);

  c.current_block() = land_block;
  land_block->set_jump(ir::JumpCmd::Return());
}

}  // namespace

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Array> const &r) {
  auto [fn, inserted] = context().ir().InsertInit(r.type());
  if (inserted) {
    push_current(&*fn);
    absl::Cleanup c = [&] { state().current.pop_back(); };

    current_block() = fn->entry();
    OnEachArrayElement(current(), r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
      EmitDefaultInit(type::Typed<ir::Reg>(reg, r.type()->data_type()));
    });
    current_block()->set_jump(ir::JumpCmd::Return());

    context().ir().WriteByteCode<EmitByteCode>(fn);
    // TODO: Remove const_cast.
    const_cast<type::Array *>(r.type())->SetInitializer(fn);
  }

  current_block()->Append(ir::InitInstruction{.type = r.type(), .reg = *r});
}

void SetArrayInits(Compiler &c, type::Array const *array_type) {
  auto [copy_fn, copy_inserted] =
      c.context().ir().InsertCopyInit(array_type, array_type);
  auto [move_fn, move_inserted] =
      c.context().ir().InsertMoveInit(array_type, array_type);
  ASSERT(copy_inserted == move_inserted);
  if (copy_inserted) {
    c.push_current(&*copy_fn);
    EmitArrayInit<Copy>(c, array_type, array_type);
    c.state().current.pop_back();

    c.push_current(&*move_fn);
    EmitArrayInit<Move>(c, array_type, array_type);
    c.state().current.pop_back();

    c.context().ir().WriteByteCode<EmitByteCode>(copy_fn);
    c.context().ir().WriteByteCode<EmitByteCode>(move_fn);
    // TODO: Remove const_cast.
    const_cast<type::Array *>(array_type)->SetInits(copy_fn, move_fn);
  }
}

void SetArrayAssignments(Compiler &c, type::Array const *array_type) {
  auto [copy_fn, copy_inserted] =
      c.context().ir().InsertCopyAssign(array_type, array_type);
  auto [move_fn, move_inserted] =
      c.context().ir().InsertMoveAssign(array_type, array_type);
  ASSERT(copy_inserted == move_inserted);
  if (copy_inserted) {
    c.push_current(&*copy_fn);
    EmitArrayAssignment<Copy>(c, array_type, array_type);
    c.state().current.pop_back();

    c.push_current(&*move_fn);
    EmitArrayAssignment<Move>(c, array_type, array_type);
    c.state().current.pop_back();

    c.context().ir().WriteByteCode<EmitByteCode>(copy_fn);
    c.context().ir().WriteByteCode<EmitByteCode>(move_fn);
    // TODO: Remove const_cast.
    const_cast<type::Array *>(array_type)->SetAssignments(copy_fn, move_fn);
  }
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Array> to,
                            ir::PartialResultBuffer const &from) {
  SetArrayInits(*this, to.type());
  current_block()->Append(ir::MoveInitInstruction{
      .type = to.type(), .from = from.get<ir::addr_t>(0), .to = *to});
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Array> to,
                            ir::PartialResultBuffer const &from) {
  SetArrayInits(*this, to.type());
  current_block()->Append(ir::CopyInitInstruction{
      .type = to.type(), .from = from.get<ir::addr_t>(0), .to = *to});
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Array> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayAssignments(*this, &to.type()->as<type::Array>());
  current_block()->Append(ir::CopyInstruction{
      .type = to.type(), .from = from->get<ir::addr_t>(), .to = *to});
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Array> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayAssignments(*this, &to.type()->as<type::Array>());
  current_block()->Append(ir::MoveInstruction{
      .type = to.type(), .from = from->get<ir::addr_t>(), .to = *to});
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Enum> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
      .value    = from.get<type::Enum::underlying_type>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Enum> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
      .value    = from.get<type::Enum::underlying_type>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Enum> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
      .value    = from->get<type::Enum::underlying_type>(),
      .location = *to,
  });
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Enum> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Flags> const &r) {
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = type::Flags::underlying_type{0},
      .location = *r,
  });
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Flags> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from.get<type::Flags::underlying_type>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Flags> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from.get<type::Flags::underlying_type>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Flags> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from->get<type::Flags::underlying_type>(),
      .location = *to,
  });
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Flags> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from->get<type::Flags::underlying_type>(),
      .location = *to,
  });
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Pointer> const &r) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = ir::Null(),
      .location = *r,
  });
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Pointer> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Pointer> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Pointer> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  if (type::CanCastImplicitly(from.type(), to.type())) {
    ir::PartialResultBuffer buffer;
    buffer.append(from->get<ir::addr_t>());
    ApplyImplicitCasts(from.type(), type::QualType::NonConstant(to.type()),
                       buffer);
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = buffer.get<ir::addr_t>(0),
        .location = *to,
    });
  } else if (from.type() == type::NullPtr) {
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = ir::Null(),
        .location = *to,
    });
  } else {
    UNREACHABLE(to, ": ", to.type(), " - ", from.type());
  }
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Pointer> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(
    type::Typed<ir::Reg, type::BufferPointer> const &r) {
  EmitDefaultInit(static_cast<type::Typed<ir::Reg, type::Pointer>>(r));
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::BufferPointer> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::BufferPointer> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::BufferPointer> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  EmitCopyAssign(
      static_cast<type::Typed<ir::RegOr<ir::addr_t>, type::Pointer>>(to), from);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::BufferPointer> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  EmitMoveAssign(
      static_cast<type::Typed<ir::RegOr<ir::addr_t>, type::Pointer>>(to), from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Primitive> const &r) {
  r.type()->Apply([&]<typename T>() {
    current_block()->Append(ir::StoreInstruction<T>{
        .value    = T{},
        .location = *r,
    });
  });
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Primitive> to,
                            ir::PartialResultBuffer const &from) {
  to.type()->Apply([&]<typename T>() {
    current_block()->Append(ir::StoreInstruction<T>{
        .value    = from.template get<T>(0),
        .location = *to,
    });
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Primitive> to,
                            ir::PartialResultBuffer const &from) {
  to.type()->Apply([&]<typename T>() {
    current_block()->Append(ir::StoreInstruction<T>{
        .value    = from.template get<T>(0),
        .location = *to,
    });
  });
}

// TODO: Determine if you want to treat mixed integer assignment as an implicit
// cast or as an overload set.
void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Primitive> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ir::PartialResultBuffer buffer;
  buffer.append(*from);
  EmitCast(current(), from.type(), to.type(), buffer);
  to.type()->Apply([&]<typename T>() {
    current_block()->Append(ir::StoreInstruction<T>{
        .value    = buffer.back().get<T>(),
        .location = *to,
    });
  });
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Primitive> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Struct> const &r) {
  auto [fn, inserted] = context().ir().InsertInit(r.type());
  if (inserted) {
    push_current(&*fn);
    absl::Cleanup c = [&] { state().current.pop_back(); };
    current_block() = current().group->entry();
    auto var        = ir::Reg::Arg(0);

    for (size_t i = 0; i < r.type()->fields().size(); ++i) {
      auto &field = r.type()->fields()[i];
      type::Typed<ir::Reg> field_reg(
          current_block()->Append(
              ir::StructIndexInstruction{.addr        = var,
                                         .index       = i,
                                         .struct_type = r.type(),
                                         .result = current().group->Reserve()}),
          r.type()->fields()[i].type);
      if (not field.initial_value.empty()) {
        EmitCopyInit(field_reg, field.initial_value);
      } else {
        EmitDefaultInit(field_reg);
      }
    }

    current_block()->set_jump(ir::JumpCmd::Return());

    // TODO: Remove this hack.
    const_cast<type::Struct *>(r.type())->init_ = fn;
    context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  current_block()->Append(ir::InitInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Function> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = from.get<ir::Fn>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Function> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = from.get<ir::Fn>(0),
      .location = *to,
  });
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Function> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = from->get<ir::Fn>(),
      .location = *to,
  });
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Function> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Struct> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::MoveInitInstruction{
      .type = to.type(), .from = from.get<ir::addr_t>(0), .to = *to});
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Struct> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::CopyInitInstruction{
      .type = to.type(), .from = from.get<ir::addr_t>(0), .to = *to});
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Struct> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  // TODO: Support mixed types and user-defined assignments.
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::CopyInstruction{
      .type = to.type(), .from = from->get<ir::addr_t>(), .to = *to});
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Struct> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  current_block()->Append(ir::MoveInstruction{
      .type = to.type(), .from = from->get<ir::addr_t>(), .to = *to});
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Slice> to,
                            ir::PartialResultBuffer const &from) {
  ir::RegOr<ir::addr_t> data = current_block()->Append(ir::LoadInstruction{
      .type   = type::BufPtr(to.type()->data_type()),
      .addr   = current_block()->Append(type::SliceDataInstruction{
          .slice  = from[0].get<ir::addr_t>(),
          .result = current().group->Reserve(),
      }),
      .result = current().group->Reserve(),
  });
  ir::RegOr<type::Slice::length_t> length =
      current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = from.get<ir::addr_t>(0),
              .result = current().group->Reserve(),
          }),
          .result = current().group->Reserve(),
      });

  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = data,
      .location = current_block()->Append(type::SliceDataInstruction{
          .slice  = *to,
          .result = current().group->Reserve(),
      }),
  });
  current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
      .value    = length,
      .location = current_block()->Append(type::SliceLengthInstruction{
          .slice  = *to,
          .result = current().group->Reserve(),
      }),
  });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Slice> to,
                            ir::PartialResultBuffer const &from) {
  EmitMoveInit(to, from);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Slice> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::BufPtr(to.type()->data_type()),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = from->get<ir::addr_t>(),
              .result = current().group->Reserve(),
          }),
          .result = current().group->Reserve(),
      }),
      .location = current_block()->Append(type::SliceDataInstruction{
          .slice  = *to,
          .result = current().group->Reserve(),
      }),
  });

  current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = *to,
              .result = current().group->Reserve(),
          }),
          .result = current().group->Reserve(),
      }),
      .location = current_block()->Append(type::SliceLengthInstruction{
          .slice  = *to,
          .result = current().group->Reserve(),
      }),
  });
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Slice> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::BufPtr(to.type()->data_type()),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = from->get<ir::addr_t>(),
              .result = current().group->Reserve(),
          }),
          .result = current().group->Reserve(),
      }),
      .location = current_block()->Append(type::SliceDataInstruction{
          .slice  = *to,
          .result = current().group->Reserve(),
      }),
  });

  current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = from->get<ir::addr_t>(),
              .result = current().group->Reserve(),
          }),
          .result = current().group->Reserve(),
      }),
      .location = current_block()->Append(type::SliceLengthInstruction{
          .slice  = *to,
          .result = current().group->Reserve(),
      }),
  });
}

}  // namespace compiler
