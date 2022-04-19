#include "compiler/emit/copy_move_assignment.h"

#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/instructions.h"
#include "type/cast.h"

// TODO: Currently inserting these always at the root. There are a couple of
// reason why this is wrong. First, If this is generated due to a temporary
// subcontext, we probably want to drop it, as we can't verify the constructed
// type is valid in any way. Second, For types like arrays of primitives, we
// only need to generate the code for them once, rather than per module.

namespace compiler {
namespace {

void EmitArrayAssignment(auto emitter, type::Array const *to,
                         type::Array const *from) {
  auto &fn                = *emitter.current().subroutine;
  emitter.current_block() = fn.entry();
  auto to_ptr             = ir::Reg::Parameter(0);
  auto from_ptr           = ir::Reg::Parameter(1);

  auto to_data_ptr_type   = type::Ptr(to->data_type());
  auto from_data_ptr_type = type::Ptr(from->data_type());

  auto from_end_ptr = emitter.current_block()->Append(ir::PtrIncrInstruction{
      .addr   = from_ptr,
      .index  = from->length().value(),
      .ptr    = from_data_ptr_type,
      .result = emitter.current().subroutine->Reserve()});

  auto *loop_body  = emitter.current().subroutine->AppendBlock();
  auto *land_block = emitter.current().subroutine->AppendBlock();
  auto *cond_block = emitter.current().subroutine->AppendBlock();

  emitter.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  emitter.current_block() = cond_block;
  auto *from_phi          = PhiInst<ir::addr_t>(emitter.current());
  auto *to_phi            = PhiInst<ir::addr_t>(emitter.current());
  ir::Reg condition =
      emitter.current_block()->Append(ir::EqInstruction<ir::addr_t>{
          .lhs    = from_phi->result,
          .rhs    = from_end_ptr,
          .result = emitter.current().subroutine->Reserve()});
  emitter.current_block()->set_jump(
      ir::JumpCmd::Cond(condition, land_block, loop_body));

  emitter.current_block() = loop_body;
  ir::PartialResultBuffer buffer;
  buffer.append(PtrFix(emitter.current(), from_phi->result, from->data_type()));
  type::Typed value_view(buffer[0], from->data_type());
  emitter(to->data_type(), to_phi->result, value_view);
  ir::Reg next_to   = emitter.current_block()->Append(ir::PtrIncrInstruction{
      .addr   = to_phi->result,
      .index  = 1,
      .ptr    = to_data_ptr_type,
      .result = emitter.current().subroutine->Reserve()});
  ir::Reg next_from = emitter.current_block()->Append(ir::PtrIncrInstruction{
      .addr   = from_phi->result,
      .index  = 1,
      .ptr    = from_data_ptr_type,
      .result = emitter.current().subroutine->Reserve()});
  emitter.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  to_phi->add(fn.entry(), to_ptr);
  to_phi->add(emitter.current_block(), next_to);
  from_phi->add(fn.entry(), from_ptr);
  from_phi->add(emitter.current_block(), next_from);

  emitter.current_block() = land_block;
  land_block->set_jump(ir::JumpCmd::Return());
}

}  // namespace

void SetArrayAssignments(CompilationDataReference ref,
                         type::Array const *array_type) {
  auto [copy_fn, copy_inserted] = ref.context().ir().InsertCopyAssign(
      array_type, array_type, [&](ir::Subroutine &s) {
        ref.push_current(&s);
        EmitArrayAssignment(CopyAssignmentEmitter(ref), array_type, array_type);
        ref.state().current.pop_back();
      });
  auto [move_fn, move_inserted] = ref.context().ir().InsertMoveAssign(
      array_type, array_type, [&](ir::Subroutine &s) {
        ref.push_current(&s);
        EmitArrayAssignment(MoveAssignmentEmitter(ref), array_type, array_type);
        ref.state().current.pop_back();
      });
  ASSERT(copy_inserted == move_inserted);
  if (move_inserted) {
    // TODO: Remove const_cast.
    const_cast<type::Array *>(array_type)->SetAssignments(copy_fn, move_fn);
  }
}

void CopyAssignmentEmitter::EmitAssignment(
    type::Array const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  SetArrayAssignments(*this, &t->as<type::Array>());
  current_block()->Append(ir::CopyInstruction{
      .type = t, .from = from->get<ir::addr_t>(), .to = to});
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Array const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  SetArrayAssignments(*this, &t->as<type::Array>());
  current_block()->Append(ir::MoveInstruction{
      .type = t, .from = from->get<ir::addr_t>(), .to = to});
}

void CopyAssignmentEmitter::EmitAssignment(
    type::Enum const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
      .value    = from->get<type::Enum::underlying_type>(),
      .location = to,
  });
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Enum const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
      .value    = from->get<type::Enum::underlying_type>(),
      .location = to,
  });
}

void CopyAssignmentEmitter::EmitAssignment(
    type::Flags const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from->get<type::Flags::underlying_type>(),
      .location = to,
  });
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Flags const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from->get<type::Flags::underlying_type>(),
      .location = to,
  });
}

void CopyAssignmentEmitter::EmitAssignment(
    type::Pointer const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  if (type::CanCastImplicitly(from.type(), t)) {
    ir::PartialResultBuffer buffer;
    buffer.append(from->get<ir::addr_t>());
    ApplyImplicitCasts(*this, from.type(), type::QualType::NonConstant(t),
                       buffer);
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = buffer.get<ir::addr_t>(0),
        .location = to,
    });
  } else if (from.type() == type::NullPtr) {
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = ir::Null(),
        .location = to,
    });
  } else {
    UNREACHABLE(to, ": ", t, " - ", from.type());
  }
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Pointer const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  if (type::CanCastImplicitly(from.type(), t)) {
    ir::PartialResultBuffer buffer;
    buffer.append(from->get<ir::addr_t>());
    ApplyImplicitCasts(*this, from.type(), type::QualType::NonConstant(t),
                       buffer);
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = buffer.get<ir::addr_t>(0),
        .location = to,
    });
  } else if (from.type() == type::NullPtr) {
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = ir::Null(),
        .location = to,
    });
  } else {
    UNREACHABLE(to, ": ", t, " - ", from.type());
  }
}

void CopyAssignmentEmitter::EmitAssignment(
    type::BufferPointer const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  if (type::CanCastImplicitly(from.type(), t)) {
    ir::PartialResultBuffer buffer;
    buffer.append(from->get<ir::addr_t>());
    ApplyImplicitCasts(*this, from.type(), type::QualType::NonConstant(t),
                       buffer);
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = buffer.get<ir::addr_t>(0),
        .location = to,
    });
  } else if (from.type() == type::NullPtr) {
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = ir::Null(),
        .location = to,
    });
  } else {
    UNREACHABLE(to, ": ", t, " - ", from.type());
  }
}

void MoveAssignmentEmitter::EmitAssignment(
    type::BufferPointer const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  if (type::CanCastImplicitly(from.type(), t)) {
    ir::PartialResultBuffer buffer;
    buffer.append(from->get<ir::addr_t>());
    ApplyImplicitCasts(*this, from.type(), type::QualType::NonConstant(t),
                       buffer);
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = buffer.get<ir::addr_t>(0),
        .location = to,
    });
  } else if (from.type() == type::NullPtr) {
    current_block()->Append(ir::StoreInstruction<ir::addr_t>{
        .value    = ir::Null(),
        .location = to,
    });
  } else {
    UNREACHABLE(to, ": ", t, " - ", from.type());
  }
}

// TODO: Determine if you want to treat mixed integer assignment as an implicit
// cast or as an overload set.
void CopyAssignmentEmitter::EmitAssignment(
    type::Primitive const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ir::PartialResultBuffer buffer;
  buffer.append(*from);
  EmitCast(*this, from.type(), t, buffer);
  t->Apply([&]<typename T>() {
    if constexpr (base::meta<T> == base::meta<ir::Integer>) {
      current_block()->Append(ir::CompileTime<ir::Action::CopyAssign, T>{
          .from = from->get<ir::addr_t>(), .to = to});
    } else {
      current_block()->Append(ir::StoreInstruction<T>{
          .value = buffer.back().get<T>(), .location = to});
    }
  });
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Primitive const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ir::PartialResultBuffer buffer;
  buffer.append(*from);
  EmitCast(*this, from.type(), t, buffer);
  t->Apply([&]<typename T>() {
    if constexpr (base::meta<T> == base::meta<ir::Integer>) {
      current_block()->Append(ir::CompileTime<ir::Action::MoveAssign, T>{
          .from = from->get<ir::addr_t>(), .to = to});
    } else {
      current_block()->Append(ir::StoreInstruction<T>{
          .value = buffer.back().get<T>(), .location = to});
    }
  });
}

void CopyAssignmentEmitter::EmitAssignment(
    type::Function const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = from->get<ir::Fn>(),
      .location = to,
  });
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Function const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(t) == from.type());
  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = from->get<ir::Fn>(),
      .location = to,
  });
}

void CopyAssignmentEmitter::EmitAssignment(
    type::Struct const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  // TODO: Support mixed types and user-defined assignments.
  ASSERT(type::Type(t) == from.type());
  current_block()->Append(ir::CopyInstruction{
      .type = t, .from = from->get<ir::addr_t>(), .to = to});
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Struct const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  current_block()->Append(ir::MoveInstruction{
      .type = t, .from = from->get<ir::addr_t>(), .to = to});
}

void CopyAssignmentEmitter::EmitAssignment(
    type::Slice const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::BufPtr(t->data_type()),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = from->get<ir::addr_t>(),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }),
      .location = current_block()->Append(type::SliceDataInstruction{
          .slice  = to,
          .result = current().subroutine->Reserve(),
      }),
  });

  current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = from->get<ir::addr_t>(),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }),
      .location = current_block()->Append(type::SliceLengthInstruction{
          .slice  = to,
          .result = current().subroutine->Reserve(),
      }),
  });
}

void MoveAssignmentEmitter::EmitAssignment(
    type::Slice const *t, ir::RegOr<ir::addr_t> to,
    type::Typed<ir::PartialResultRef> const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::BufPtr(t->data_type()),
          .addr   = current_block()->Append(type::SliceDataInstruction{
              .slice  = from->get<ir::addr_t>(),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }),
      .location = current_block()->Append(type::SliceDataInstruction{
          .slice  = to,
          .result = current().subroutine->Reserve(),
      }),
  });

  current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
      .value    = current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = from->get<ir::addr_t>(),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      }),
      .location = current_block()->Append(type::SliceLengthInstruction{
          .slice  = to,
          .result = current().subroutine->Reserve(),
      }),
  });
}

}  // namespace compiler
