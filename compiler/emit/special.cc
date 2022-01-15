#include "ast/ast.h"
#include "compiler/compiler.h"
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

// Usually it is sufficient to determine all the inputs to a phi instruction
// upfront, but sometimes it is useful to construct a phi instruction without
// having set its inputs.
//
// TODO: Right now we are relying on the fact that Inst stores values on the
// heap, but this may not always be the case.
template <typename T>
ir::PhiInstruction<T> *PhiInst(IrBuilder &builder) {
  ir::PhiInstruction<T> inst;
  inst.result = builder.CurrentGroup()->Reserve();
  builder.CurrentBlock()->Append(std::move(inst));
  return builder.CurrentBlock()
      ->instructions()
      .back()
      .template if_as<ir::PhiInstruction<T>>();
}

template <typename F>
void OnEachArrayElement(IrBuilder &builder, type::Array const *t,
                        ir::Reg array_reg, F fn) {
  auto *data_ptr_type = type::Ptr(t->data_type());

  auto ptr     = builder.PtrIncr(array_reg, 0, type::Ptr(data_ptr_type));
  auto end_ptr = builder.PtrIncr(ptr, t->length().value(), data_ptr_type);

  auto *start_block = builder.CurrentBlock();
  auto *loop_body   = builder.CurrentGroup()->AppendBlock();
  auto *land_block  = builder.CurrentGroup()->AppendBlock();
  auto *cond_block  = builder.CurrentGroup()->AppendBlock();

  builder.UncondJump(cond_block);

  builder.CurrentBlock() = cond_block;
  auto *phi              = PhiInst<ir::addr_t>(builder);

  ir::Reg condition =
      builder.CurrentBlock()->Append(ir::EqInstruction<ir::addr_t>{
          .lhs    = phi->result,
          .rhs    = end_ptr,
          .result = builder.CurrentGroup()->Reserve()});
  builder.CondJump(condition, land_block, loop_body);

  builder.CurrentBlock() = loop_body;
  fn(phi->result);
  ir::Reg next = builder.PtrIncr(phi->result, 1, data_ptr_type);
  builder.UncondJump(cond_block);

  phi->add(start_block, ptr);
  phi->add(builder.CurrentBlock(), next);

  builder.CurrentBlock() = land_block;
}

enum Kind { Move, Copy };

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
      bldr.PtrIncr(from_ptr, from->length().value(), from_data_ptr_type);
  auto to_ptr = bldr.PtrIncr(var, 0, to_data_ptr_type);

  auto *loop_body  = bldr.CurrentGroup()->AppendBlock();
  auto *land_block = bldr.CurrentGroup()->AppendBlock();
  auto *cond_block = bldr.CurrentGroup()->AppendBlock();

  bldr.UncondJump(cond_block);

  bldr.CurrentBlock() = cond_block;
  auto *from_phi      = PhiInst<ir::addr_t>(bldr);
  auto *to_phi        = PhiInst<ir::addr_t>(bldr);
  ir::Reg condition   = bldr.CurrentBlock()->Append(
      ir::EqInstruction<ir::addr_t>{.lhs    = from_phi->result,
                                    .rhs    = from_end_ptr,
                                    .result = bldr.CurrentGroup()->Reserve()});
  bldr.CondJump(condition, land_block, loop_body);

  bldr.CurrentBlock() = loop_body;
  ir::PartialResultBuffer buffer;
  buffer.append(c.builder().PtrFix(from_phi->result, from->data_type()));
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
  auto ret            = ir::Reg::Out(0);

  auto from_data_ptr_type = type::Ptr(from->data_type());

  auto from_ptr = bldr.PtrIncr(var, 0, from_data_ptr_type);
  auto from_end_ptr =
      bldr.PtrIncr(from_ptr, from->length().value(), from_data_ptr_type);
  auto to_ptr = bldr.PtrIncr(ret, 0, from_data_ptr_type);

  auto *loop_body  = bldr.CurrentGroup()->AppendBlock();
  auto *land_block = bldr.CurrentGroup()->AppendBlock();
  auto *cond_block = bldr.CurrentGroup()->AppendBlock();

  bldr.UncondJump(cond_block);

  bldr.CurrentBlock() = cond_block;
  auto *from_phi      = PhiInst<ir::addr_t>(bldr);
  auto *to_phi        = PhiInst<ir::addr_t>(bldr);
  ir::Reg condition   = bldr.CurrentBlock()->Append(
      ir::EqInstruction<ir::addr_t>{.lhs    = from_phi->result,
                                    .rhs    = from_end_ptr,
                                    .result = bldr.CurrentGroup()->Reserve()});
  bldr.CondJump(condition, land_block, loop_body);

  bldr.CurrentBlock() = loop_body;
  ir::PartialResultBuffer buffer;
  buffer.append(c.builder().PtrFix(from_phi->result, from->data_type()));
  if constexpr (K == Copy) {
    c.EmitCopyInit(type::Typed<ir::Reg>(to_phi->result, to->data_type()),
                   buffer);

  } else if constexpr (K == Move) {
    c.EmitCopyInit(type::Typed<ir::Reg>(to_phi->result, to->data_type()),
                   buffer);
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
  auto [fn, inserted] = context().ir().InsertInit(r.type());
  if (inserted) {
    set_builder(&*fn);
    absl::Cleanup c = [&] { state().builders.pop_back(); };

    builder().CurrentBlock() = fn->entry();
    OnEachArrayElement(builder(), r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
      EmitDefaultInit(type::Typed<ir::Reg>(reg, r.type()->data_type()));
    });
    builder().ReturnJump();
    context().ir().WriteByteCode<EmitByteCode>(fn);
    // TODO: Remove const_cast.
    const_cast<type::Array *>(r.type())->SetInitializer(fn);
  }

  current_block()->Append(ir::InitInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Array> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  auto [fn, inserted] = context().ir().InsertDestroy(r.type());
  if (inserted) {
    set_builder(&*fn);
    absl::Cleanup c = [&] { state().builders.pop_back(); };

    builder().CurrentBlock() = fn->entry();
    OnEachArrayElement(builder(), r.type(), ir::Reg::Arg(0), [=](ir::Reg reg) {
      EmitDestroy(type::Typed<ir::Reg>(reg, r.type()->data_type()));
    });
    builder().ReturnJump();
    context().ir().WriteByteCode<EmitByteCode>(fn);
    // TODO: Remove const_cast.
    const_cast<type::Array *>(r.type())->SetDestructor(fn);
  }
  current_block()->Append(ir::DestroyInstruction{.type = r.type(), .reg = *r});
}

void SetArrayInits(Compiler &c, type::Array const *array_type) {
  auto [copy_fn, copy_inserted] =
      c.context().ir().InsertCopyInit(array_type, array_type);
  auto [move_fn, move_inserted] =
      c.context().ir().InsertMoveInit(array_type, array_type);
  ASSERT(copy_inserted == move_inserted);
  if (copy_inserted) {
    c.set_builder(&*copy_fn);
    EmitArrayInit<Copy>(c, array_type, array_type);
    c.state().builders.pop_back();

    c.set_builder(&*move_fn);
    EmitArrayInit<Move>(c, array_type, array_type);
    c.state().builders.pop_back();

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
    c.set_builder(&*copy_fn);
    EmitArrayAssignment<Copy>(c, array_type, array_type);
    c.state().builders.pop_back();

    c.set_builder(&*move_fn);
    EmitArrayAssignment<Move>(c, array_type, array_type);
    c.state().builders.pop_back();

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
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Array> to,
                            ir::PartialResultBuffer const &from) {
  SetArrayInits(*this, to.type());
  current_block()->Append(ir::CopyInitInstruction{
      .type = to.type(), .from = from.get<ir::addr_t>(0), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Array> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayAssignments(*this, &to.type()->as<type::Array>());
  builder().Copy(
      to, type::Typed<ir::Reg>(from->get<ir::addr_t>().reg(), from.type()));
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Array> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  SetArrayAssignments(*this, &to.type()->as<type::Array>());
  builder().Move(
      to, type::Typed<ir::Reg>(from->get<ir::addr_t>().reg(), from.type()));
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Enum> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<type::Enum::underlying_type>(0), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Enum> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<type::Enum::underlying_type>(0), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Enum> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<type::Enum::underlying_type>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Enum> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  EmitCopyAssign(to, from);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Flags> const &r) {
  builder().Store(type::Flags::underlying_type{0}, *r);
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Flags> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<type::Flags::underlying_type>(0), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Flags> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<type::Flags::underlying_type>(0), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Flags> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<type::Flags::underlying_type>(), *to);
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Flags> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<type::Flags::underlying_type>(), *to);
}

void Compiler::EmitDefaultInit(type::Typed<ir::Reg, type::Pointer> const &r) {
  builder().Store(ir::Null(), *r);
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Pointer> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<ir::addr_t>(0), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Pointer> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<ir::addr_t>(0), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Pointer> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  if (type::CanCastImplicitly(from.type(), to.type())) {
    ir::PartialResultBuffer buffer;
    buffer.append(from->get<ir::addr_t>());
    ApplyImplicitCasts(from.type(), type::QualType::NonConstant(to.type()),
                       buffer);
    builder().Store(buffer.get<ir::addr_t>(0), *to);
  } else if (from.type() == type::NullPtr) {
    builder().Store(ir::Null(), *to);
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
  builder().Store(from.get<ir::addr_t>(0), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::BufferPointer> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<ir::addr_t>(0), *to);
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
  r.type()->Apply([&]<typename T>() { builder().Store(T{}, *r); });
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Primitive> to,
                            ir::PartialResultBuffer const &from) {
  to.type()->Apply(
      [&]<typename T>() { builder().Store(from.template get<T>(0), *to); });
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Primitive> to,
                            ir::PartialResultBuffer const &from) {
  to.type()->Apply(
      [&]<typename T>() { builder().Store(from.template get<T>(0), *to); });
}

// TODO: Determine if you want to treat mixed integer assignment as an implicit
// cast or as an overload set.
void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Primitive> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  to.type()->Apply([&]<typename T>() {
    builder().Store(builder().CastTo<T>(from.type(), *from), *to);
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
    set_builder(&*fn);
    absl::Cleanup c = [&] { state().builders.pop_back(); };
    builder().CurrentBlock() = builder().CurrentGroup()->entry();
    auto var                 = ir::Reg::Arg(0);

    for (size_t i = 0; i < r.type()->fields().size(); ++i) {
      auto &field = r.type()->fields()[i];
      if (not field.initial_value.empty()) {
        EmitCopyInit(builder().FieldRef(var, r.type(), i), field.initial_value);
      } else {
        EmitDefaultInit(
            type::Typed<ir::Reg>(builder().FieldRef(var, r.type(), i)));
      }
    }

    builder().ReturnJump();

    // TODO: Remove this hack.
    const_cast<type::Struct *>(r.type())->init_ = fn;
    context().ir().WriteByteCode<EmitByteCode>(fn);
  }
  current_block()->Append(ir::InitInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitDestroy(type::Typed<ir::Reg, type::Struct> const &r) {
  if (not r.type()->HasDestructor()) { return; }
  current_block()->Append(ir::DestroyInstruction{.type = r.type(), .reg = *r});
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Function> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<ir::Fn>(0), *to);
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Function> to,
                            ir::PartialResultBuffer const &from) {
  builder().Store(from.get<ir::Fn>(0), *to);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Function> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  ASSERT(type::Type(to.type()) == from.type());
  builder().Store(from->get<ir::Fn>(), *to);
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
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Struct> to,
                            ir::PartialResultBuffer const &from) {
  current_block()->Append(ir::CopyInitInstruction{
      .type = to.type(), .from = from.get<ir::addr_t>(0), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Struct> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  // TODO: Support mixed types and user-defined assignments.
  ASSERT(type::Type(to.type()) == from.type());
  current_block()->Append(ir::CopyInstruction{
      .type = to.type(), .from = from->get<ir::addr_t>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Struct> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  current_block()->Append(ir::MoveInstruction{
      .type = to.type(), .from = from->get<ir::addr_t>(), .to = *to});
  current_block()->load_store_cache().clear();
}

void Compiler::EmitMoveInit(type::Typed<ir::Reg, type::Slice> to,
                            ir::PartialResultBuffer const &from) {
  ir::RegOr<ir::addr_t> data = builder().Load<ir::addr_t>(
      current_block()->Append(type::SliceDataInstruction{
          .slice  = from.get<ir::addr_t>(0),
          .result = builder().CurrentGroup()->Reserve(),
      }),
      type::BufPtr(to.type()->data_type()));
  ir::RegOr<type::Slice::length_t> length =
      builder().Load<type::Slice::length_t>(
          current_block()->Append(type::SliceLengthInstruction{
              .slice  = from.get<ir::addr_t>(0),
              .result = builder().CurrentGroup()->Reserve(),
          }));

  builder().Store(data, current_block()->Append(type::SliceDataInstruction{
                            .slice  = *to,
                            .result = builder().CurrentGroup()->Reserve(),
                        }));
  builder().Store(length, current_block()->Append(type::SliceLengthInstruction{
                              .slice  = *to,
                              .result = builder().CurrentGroup()->Reserve(),
                          }));
  current_block()->load_store_cache().clear();
}

void Compiler::EmitCopyInit(type::Typed<ir::Reg, type::Slice> to,
                            ir::PartialResultBuffer const &from) {
  EmitMoveInit(to, from);
}

void Compiler::EmitCopyAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Slice> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  builder().Store(builder().Load<ir::addr_t>(
                      current_block()->Append(type::SliceDataInstruction{
                          .slice  = from->get<ir::addr_t>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      }),
                      type::BufPtr(to.type()->data_type())),
                  current_block()->Append(type::SliceDataInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  builder().Store(builder().Load<type::Slice::length_t>(
                      current_block()->Append(type::SliceLengthInstruction{
                          .slice  = from->get<ir::addr_t>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  current_block()->load_store_cache().clear();
}

void Compiler::EmitMoveAssign(
    type::Typed<ir::RegOr<ir::addr_t>, type::Slice> const &to,
    type::Typed<ir::PartialResultRef> const &from) {
  builder().Store(builder().Load<ir::addr_t>(
                      current_block()->Append(type::SliceDataInstruction{
                          .slice  = from->get<ir::addr_t>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      }),
                      type::BufPtr(to.type()->data_type())),
                  current_block()->Append(type::SliceDataInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  builder().Store(builder().Load<type::Slice::length_t>(
                      current_block()->Append(type::SliceLengthInstruction{
                          .slice  = from->get<ir::addr_t>(),
                          .result = builder().CurrentGroup()->Reserve(),
                      })),
                  current_block()->Append(type::SliceLengthInstruction{
                      .slice  = *to,
                      .result = builder().CurrentGroup()->Reserve(),
                  }));
  current_block()->load_store_cache().clear();
}

}  // namespace compiler
