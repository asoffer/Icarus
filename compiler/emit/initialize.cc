#include "compiler/emit/initialize.h"

#include "compiler/emit/common.h"
#include "compiler/instructions.h"

namespace compiler {
namespace {

struct DefaultValueVisitor {
  using signature = void(ir::PartialResultBuffer &);

  void operator()(auto const *t, ir::PartialResultBuffer &buffer) { NOT_YET(t->to_string()); }

  void operator()(type::Function const *t, ir::PartialResultBuffer &buffer) {
    UNREACHABLE();
  }

  void operator()(type::Flags const *f, ir::PartialResultBuffer &buffer) {
    f->UnderlyingType().visit(*this, buffer);
  }

  void operator()(type::Pointer const *, ir::PartialResultBuffer &buffer) {
    buffer.append(ir::Null());
  }

  void operator()(type::BufferPointer const *,
                  ir::PartialResultBuffer &buffer) {
    buffer.append(ir::Null());
  }

  void operator()(type::Primitive const *p, ir::PartialResultBuffer &buffer) {
    switch (p->kind()) {
      case type::Primitive::Kind::NullPtr: buffer.append(ir::Null()); return;
      case type::Primitive::Kind::Bool: buffer.append(false); return;
      case type::Primitive::Kind::Char: buffer.append(ir::Char()); return;
      case type::Primitive::Kind::Integer: buffer.append(ir::Integer()); return;
      case type::Primitive::Kind::I8: buffer.append(int8_t{0}); return;
      case type::Primitive::Kind::I16: buffer.append(int16_t{0}); return;
      case type::Primitive::Kind::I32: buffer.append(int32_t{0}); return;
      case type::Primitive::Kind::I64: buffer.append(int64_t{0}); return;
      case type::Primitive::Kind::U8: buffer.append(uint8_t{0}); return;
      case type::Primitive::Kind::U16: buffer.append(uint16_t{0}); return;
      case type::Primitive::Kind::U32: buffer.append(uint32_t{0}); return;
      case type::Primitive::Kind::U64: buffer.append(uint64_t{0}); return;
      case type::Primitive::Kind::F32: buffer.append(float{0}); return;
      case type::Primitive::Kind::F64: buffer.append(double{0}); return;
      case type::Primitive::Kind::Byte: buffer.append(std::byte{}); return;
      default: UNREACHABLE();
    }
  }

  void operator()(type::Slice const *, ir::PartialResultBuffer &buffer) {
    buffer.append(ir::Slice(ir::Null(), 0));
  }

  void operator()(type::Struct const *s, ir::PartialResultBuffer &buffer) {
    for (auto const &field : s->fields()) {
      if (field.initial_value.empty()) {
        field.type.visit(*this, buffer);
      } else {
        buffer.append(field.initial_value[0]);
      }
    }
  }
};

enum Kind { Move, Copy };
template <Kind K>
void EmitArrayInit(CompilationDataReference ref, type::Array const *to,
                   type::Array const *from) {
  auto &fn            = *ref.current().subroutine;
  ref.current_block() = fn.entry();
  auto from_ptr       = ir::Reg::Parameter(0);
  auto to_ptr         = ir::Reg::Output(0);

  auto from_data_ptr_type = type::Ptr(from->data_type());
  auto from_end_ptr       = ref.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = from_ptr,
                             .index  = from->length().value(),
                             .ptr    = from_data_ptr_type,
                             .result = ref.current().subroutine->Reserve()});

  auto *loop_body  = ref.current().subroutine->AppendBlock();
  auto *land_block = ref.current().subroutine->AppendBlock();
  auto *cond_block = ref.current().subroutine->AppendBlock();

  ref.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  ref.current_block() = cond_block;
  auto *from_phi      = PhiInst<ir::addr_t>(ref.current());
  auto *to_phi        = PhiInst<ir::addr_t>(ref.current());
  ir::Reg condition = ref.current_block()->Append(ir::EqInstruction<ir::addr_t>{
      .lhs    = from_phi->result,
      .rhs    = from_end_ptr,
      .result = ref.current().subroutine->Reserve()});
  ref.current_block()->set_jump(
      ir::JumpCmd::Cond(condition, land_block, loop_body));

  ref.current_block() = loop_body;
  ir::PartialResultBuffer buffer;
  buffer.append(PtrFix(ref.current(), from_phi->result, from->data_type()));
  if constexpr (K == Copy) {
    CopyInitializationEmitter emitter(ref);
    emitter(to->data_type(), to_phi->result, buffer);
  } else if constexpr (K == Move) {
    MoveInitializationEmitter emitter(ref);
    emitter(to->data_type(), to_phi->result, buffer);
  } else {
    UNREACHABLE();
  }

  ir::Reg next_to = ref.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = to_phi->result,
                             .index  = 1,
                             .ptr    = from_data_ptr_type,
                             .result = ref.current().subroutine->Reserve()});
  ir::Reg next_from = ref.current_block()->Append(
      ir::PtrIncrInstruction{.addr   = from_phi->result,
                             .index  = 1,
                             .ptr    = from_data_ptr_type,
                             .result = ref.current().subroutine->Reserve()});
  ref.current_block()->set_jump(ir::JumpCmd::Uncond(cond_block));

  to_phi->add(fn.entry(), to_ptr);
  to_phi->add(ref.current_block(), next_to);
  from_phi->add(fn.entry(), from_ptr);
  from_phi->add(ref.current_block(), next_from);

  ref.current_block() = land_block;
  land_block->set_jump(ir::JumpCmd::Return());
}

void SetArrayInits(CompilationDataReference ref,
                   type::Array const *array_type) {
  static absl::node_hash_map<type::Array const *, std::once_flag> flags;
  std::call_once(flags[array_type], [&] {
    auto [copy_fn, copy_inserted] =
        ref.context().ir().InsertCopyInit(array_type, array_type);
    auto [move_fn, move_inserted] =
        ref.context().ir().InsertMoveInit(array_type, array_type);
    ASSERT(copy_inserted == move_inserted);
    if (copy_inserted) {
      ref.push_current(&*copy_fn);
      EmitArrayInit<Copy>(ref, array_type, array_type);
      ref.state().current.pop_back();

      ref.push_current(&*move_fn);
      EmitArrayInit<Move>(ref, array_type, array_type);
      ref.state().current.pop_back();

      ref.context().ir().WriteByteCode<EmitByteCode>(copy_fn);
      ref.context().ir().WriteByteCode<EmitByteCode>(move_fn);
      // TODO: Remove const_cast.
      const_cast<type::Array *>(array_type)->SetInits(copy_fn, move_fn);
    }
  });
}

}  // namespace

void DefaultInitializationEmitter::EmitInitialize(type::Array const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  auto [fn, inserted] = context().ir().InsertInit(t);
  if (inserted) {
    push_current(&*fn);
    absl::Cleanup c = [&] { state().current.pop_back(); };

    current_block() = fn->entry();
    current_block() = OnEachArrayElement(
        current(), t, ir::Reg::Parameter(0), [=](ir::BasicBlock *entry, ir::Reg reg) {
          current_block() = entry;
          EmitInitialize(t->data_type(), reg);
          return current_block();
        });
    current_block()->set_jump(ir::JumpCmd::Return());

    context().ir().WriteByteCode<EmitByteCode>(fn);
    // TODO: Remove const_cast.
    const_cast<type::Array *>(t)->SetInitializer(fn);
  }

  current_block()->Append(ir::InitInstruction{.type = t, .reg = addr.reg()});
}

void DefaultInitializationEmitter::EmitInitialize(type::Enum const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  UNREACHABLE();
}

void DefaultInitializationEmitter::EmitInitialize(type::Flags const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = type::Flags::underlying_type{0},
      .location = addr,
  });
}

void DefaultInitializationEmitter::EmitInitialize(type::Function const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  UNREACHABLE();
}

void DefaultInitializationEmitter::EmitInitialize(type::Pointer const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = ir::Null(),
      .location = addr,
  });
}

void DefaultInitializationEmitter::EmitInitialize(type::BufferPointer const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = ir::Null(),
      .location = addr,
  });
}

void DefaultInitializationEmitter::EmitInitialize(type::Primitive const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  t->Apply([&]<typename T>() {
    if constexpr (base::meta<T> == base::meta<ir::Integer>) {
      NOT_YET();
    } else {
      current_block()->Append(ir::StoreInstruction<T>{.location = addr});
    }
  });
}

void DefaultInitializationEmitter::EmitInitialize(type::Slice const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  UNREACHABLE();
}

void DefaultInitializationEmitter::EmitInitialize(type::Struct const *t,
                                                  ir::RegOr<ir::addr_t> addr) {
  auto [fn, inserted] = context().ir().InsertInit(t);
  if (inserted) {
    push_current(&*fn);
    absl::Cleanup c = [&] { state().current.pop_back(); };
    current_block() = current().subroutine->entry();
    auto var        = ir::Reg::Parameter(0);

    for (size_t i = 0; i < t->fields().size(); ++i) {
      auto &field = t->fields()[i];
      type::Typed<ir::Reg> field_reg(
          current_block()->Append(ir::StructIndexInstruction{
              .addr        = var,
              .index       = i,
              .struct_type = t,
              .result      = current().subroutine->Reserve()}),
          t->fields()[i].type);
      if (not field.initial_value.empty()) {
        CopyInitializationEmitter emitter(*this);
        emitter(field_reg.type(), *field_reg, field.initial_value);
      } else {
        DefaultInitializationEmitter emitter(*this);
        emitter(field_reg.type(), *field_reg);
      }
    }

    current_block()->set_jump(ir::JumpCmd::Return());

    // TODO: Remove this hack.
    const_cast<type::Struct *>(t)->init_ = fn;
    context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  current_block()->Append(ir::InitInstruction{.type = t, .reg = addr.reg()});
}

void MoveInitializationEmitter::EmitInitialize(
    type::Array const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  SetArrayInits(*this, t);
  current_block()->Append(ir::MoveInitInstruction{
      .type = t, .from = from.get<ir::addr_t>(0), .to = addr});
}

void CopyInitializationEmitter::EmitInitialize(
    type::Array const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  SetArrayInits(*this, t);
  current_block()->Append(ir::CopyInitInstruction{
      .type = t, .from = from.get<ir::addr_t>(0), .to = addr});
}

void MoveInitializationEmitter::EmitInitialize(
    type::Enum const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
      .value    = from.get<type::Enum::underlying_type>(0),
      .location = addr,
  });
}

void CopyInitializationEmitter::EmitInitialize(
    type::Enum const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Enum::underlying_type>{
      .value    = from.get<type::Enum::underlying_type>(0),
      .location = addr,
  });
}

void MoveInitializationEmitter::EmitInitialize(
    type::Flags const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from.get<type::Flags::underlying_type>(0),
      .location = addr,
  });
}

void CopyInitializationEmitter::EmitInitialize(
    type::Flags const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
      .value    = from.get<type::Flags::underlying_type>(0),
      .location = addr,
  });
}

void MoveInitializationEmitter::EmitInitialize(
    type::Pointer const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = addr,
  });
}

void CopyInitializationEmitter::EmitInitialize(
    type::Pointer const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = addr,
  });
}

void MoveInitializationEmitter::EmitInitialize(
    type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = addr,
  });
}

void CopyInitializationEmitter::EmitInitialize(
    type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = from.get<ir::addr_t>(0),
      .location = addr,
  });
}

void MoveInitializationEmitter::EmitInitialize(
    type::Primitive const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  t->Apply([&]<typename T>() {
    if constexpr (base::meta<T> == base::meta<ir::Integer>) {
      current_block()->Append(ir::CompileTime<ir::Action::MoveInit, T>{
          .from = from.get<ir::addr_t>(0), .to = addr});
    } else {
      current_block()->Append(ir::StoreInstruction<T>{
          .value    = from.template get<T>(0),
          .location = addr,
      });
    }
  });
}

void CopyInitializationEmitter::EmitInitialize(
    type::Primitive const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  t->Apply([&]<typename T>() {
    if constexpr (base::meta<T> == base::meta<ir::Integer>) {
      current_block()->Append(ir::CompileTime<ir::Action::CopyInit, T>{
          .from = from.get<ir::addr_t>(0), .to = addr});
    } else {
      current_block()->Append(ir::StoreInstruction<T>{
          .value    = from.template get<T>(0),
          .location = addr,
      });
    }
  });
}

void MoveInitializationEmitter::EmitInitialize(
    type::Function const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = from.get<ir::Fn>(0),
      .location = addr,
  });
}

void CopyInitializationEmitter::EmitInitialize(
    type::Function const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::StoreInstruction<ir::Fn>{
      .value    = from.get<ir::Fn>(0),
      .location = addr,
  });
}

void MoveInitializationEmitter::EmitInitialize(
    type::Struct const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::MoveInitInstruction{
      .type = t, .from = from.get<ir::addr_t>(0), .to = addr});
}

void CopyInitializationEmitter::EmitInitialize(
    type::Struct const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::CopyInitInstruction{
      .type = t, .from = from.get<ir::addr_t>(0), .to = addr});
}

void MoveInitializationEmitter::EmitInitialize(
    type::Slice const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  ir::RegOr<ir::addr_t> data = current_block()->Append(ir::LoadInstruction{
      .type   = type::BufPtr(t->data_type()),
      .addr   = current_block()->Append(type::SliceDataInstruction{
          .slice  = from[0].get<ir::addr_t>(),
          .result = current().subroutine->Reserve(),
      }),
      .result = current().subroutine->Reserve(),
  });
  ir::RegOr<type::Slice::length_t> length =
      current_block()->Append(ir::LoadInstruction{
          .type   = type::Slice::LengthType(),
          .addr   = current_block()->Append(type::SliceLengthInstruction{
              .slice  = from.get<ir::addr_t>(0),
              .result = current().subroutine->Reserve(),
          }),
          .result = current().subroutine->Reserve(),
      });

  current_block()->Append(ir::StoreInstruction<ir::addr_t>{
      .value    = data,
      .location = current_block()->Append(type::SliceDataInstruction{
          .slice  = addr,
          .result = current().subroutine->Reserve(),
      }),
  });
  current_block()->Append(ir::StoreInstruction<type::Slice::length_t>{
      .value    = length,
      .location = current_block()->Append(type::SliceLengthInstruction{
          .slice  = addr,
          .result = current().subroutine->Reserve(),
      }),
  });
}

void CopyInitializationEmitter::EmitInitialize(
    type::Slice const *t, ir::RegOr<ir::addr_t> addr,
    ir::PartialResultBuffer const &from) {
  MoveInitializationEmitter emitter(*this);
  emitter(t, addr, from);
}

void WriteDefaultValueFor(type::Type t, ir::PartialResultBuffer &out) {
  DefaultValueVisitor v;
  t.visit(v, out);
}

}  // namespace compiler
