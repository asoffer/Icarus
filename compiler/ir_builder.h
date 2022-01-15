#ifndef ICARUS_COMPILER_IR_BUILDER_H
#define ICARUS_COMPILER_IR_BUILDER_H

#include <vector>

#include "absl/types/span.h"
#include "base/debug.h"
#include "base/meta.h"
#include "base/scope.h"
#include "base/untyped_buffer.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/instruction/arithmetic.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/core.h"
#include "ir/instruction/instructions.h"
#include "ir/out_params.h"
#include "ir/scope_state.h"
#include "ir/value/addr.h"
#include "ir/value/char.h"
#include "ir/value/module_id.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/interface/interface.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/scope.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

// TODO: Remove as much as possible from IrBuilder. I'm not exactly sure what
// I want to do with this, but we should really limit it only to core
// instructions and/or instructions that can only be optimized in the streaming
// approach by carrying around extra state (like loads/store-caching).

struct IrBuilder {
  explicit IrBuilder(ir::internal::BlockGroupBase* group,
                     ast::Scope const* scope);

  ir::BasicBlock* EmitDestructionPath(ast::Scope const* from,
                                      ast::Scope const* to);

  ir::internal::BlockGroupBase*& CurrentGroup() { return group_; }
  ir::BasicBlock*& CurrentBlock() { return block_; }

  // INSTRUCTIONS

  ir::Reg PtrFix(ir::RegOr<ir::addr_t> addr, type::Type desired_type) {
    // TODO must this be a register if it's loaded?
    if (desired_type.get()->is_big()) { return addr.reg(); }
    ir::PartialResultBuffer buffer;
    Load(addr, desired_type, buffer);
    return buffer.get<ir::Reg>(0);
  }

  template <typename T>
  ir::RegOr<T> Load(ir::RegOr<ir::addr_t> addr, type::Type t = GetType<T>()) {
    ASSERT(addr != ir::Null());
    auto& blk = *CurrentBlock();

    auto [slot, inserted] = blk.load_store_cache().slot<T>(addr);
    if (not inserted) { return slot; }

    ir::LoadInstruction inst{.type = t, .addr = addr};
    auto result = inst.result = CurrentGroup()->Reserve();

    slot = result;

    blk.Append(std::move(inst));
    return result;
  }

  void Load(ir::RegOr<ir::addr_t> r, type::Type t, ir::PartialResultBuffer& out) {
    LOG("Load", "Calling Load(%s, %s)", r, t.to_string());
    ASSERT(r != ir::Null());
    if (t.is<type::Function>()) {
      out.append(Load<ir::Fn>(r, t));
    } else if (t.is<type::Pointer>()) {
      out.append(Load<ir::addr_t>(r, t));
    } else if (t.is<type::Enum>()) {
      out.append(Load<type::Enum::underlying_type>(r, t));
    } else if (t.is<type::Flags>()) {
      out.append(Load<type::Flags::underlying_type>(r, t));
    } else {
      t.as<type::Primitive>().Apply(
          [&]<typename T>() { return out.append(Load<T>(r, t)); });
    }
  }

  template <typename T>
  void Store(T r, ir::RegOr<ir::addr_t> addr) {
    if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
      auto& blk = *CurrentBlock();
      blk.load_store_cache().clear<typename T::type>();
      blk.Append(
          ir::StoreInstruction<typename T::type>{.value = r, .location = addr});
    } else {
      Store(ir::RegOr<T>(r), addr);
    }
  }

  // Emits a function-call instruction, calling `fn` of type `f` with the given
  // `arguments` and output parameters. If output parameters are not present,
  // the function must return nothing.
  void Call(ir::RegOr<ir::Fn> const& fn, type::Function const* f,
            ir::PartialResultBuffer args, ir::OutParams outs);

  // Jump instructions must be the last instruction in a basic block. They
  // handle control-flow, indicating which basic block control should be
  // transferred to next.
  //
  // `UncondJump`:   Transfers control to `block`.
  // `CondJump`:     Transfers control to one of two blocks depending on a
  //                 run-time boolean value.
  // `ReturnJump`:   Transfers control back to the calling function.
  void UncondJump(ir::BasicBlock* block);
  void CondJump(ir::RegOr<bool> cond, ir::BasicBlock* true_block,
                ir::BasicBlock* false_block);
  void ReturnJump();
  void BlockJump(ir::Block b, ir::BasicBlock* after);

  // Special members function instructions. Calling these typically calls
  // builtin functions (or, in the case of primitive types, do nothing).
  void Move(type::Typed<ir::RegOr<ir::addr_t>> to, type::Typed<ir::Reg> from);
  void Copy(type::Typed<ir::RegOr<ir::addr_t>> to, type::Typed<ir::Reg> from);

  // Data structure access commands. For structs, `Fields` takes an
  // address of the data structure and returns the address of the particular
  // field requested. For variants, `VariantType` computes the location where
  // the type is stored and `VariantValue` accesses the location where the
  // value is stored.
  type::Typed<ir::Reg> FieldRef(ir::RegOr<ir::addr_t> r, type::Struct const* t,
                                int64_t n);

  type::Type FieldValue(ir::RegOr<ir::addr_t> r, type::Struct const* t, int64_t n,
                        ir::PartialResultBuffer& out) {
    auto typed_reg = FieldRef(r, t, n);
    out.append(PtrFix(*typed_reg, typed_reg.type()));
    return typed_reg.type();
  }
  ir::Reg PtrIncr(ir::RegOr<ir::addr_t> ptr, ir::RegOr<int64_t> inc,
                  type::Pointer const* t);

  ir::Reg Alloca(type::Type t);

  void DebugIr() {
    CurrentBlock()->Append(ir::DebugIrInstruction{.fn = CurrentGroup()});
  }

  ir::BasicBlock* landing(ast::Scope const* s) const;

  template <typename T>
  static type::Type GetType() {
    if constexpr (base::meta<T> == base::meta<bool>) {
      return type::Bool;
    } else if constexpr (base::meta<T> == base::meta<ir::Integer>) {
      return type::Integer;
    } else if constexpr (base::meta<T> == base::meta<ir::Char>) {
      return type::Char;
    } else if constexpr (base::meta<T> == base::meta<ir::memory_t>) {
      return type::Byte;
    } else if constexpr (base::meta<T> == base::meta<int8_t>) {
      return type::I8;
    } else if constexpr (base::meta<T> == base::meta<int16_t>) {
      return type::I16;
    } else if constexpr (base::meta<T> == base::meta<int32_t>) {
      return type::I32;
    } else if constexpr (base::meta<T> == base::meta<int64_t>) {
      return type::I64;
    } else if constexpr (base::meta<T> == base::meta<uint8_t>) {
      return type::U8;
    } else if constexpr (base::meta<T> == base::meta<uint16_t>) {
      return type::U16;
    } else if constexpr (base::meta<T> == base::meta<uint32_t>) {
      return type::U32;
    } else if constexpr (base::meta<T> == base::meta<uint64_t>) {
      return type::U64;
    } else if constexpr (base::meta<T> == base::meta<float>) {
      return type::F32;
    } else if constexpr (base::meta<T> == base::meta<double>) {
      return type::F64;
    } else if constexpr (base::meta<T> == base::meta<type::Type>) {
      return type::Type_;
    } else if constexpr (base::meta<T> == base::meta<ir::Scope>) {
      return type::Scp({});
    } else if constexpr (base::meta<T> == base::meta<ir::ModuleId>) {
      return type::Module;
    } else if constexpr (base::meta<T> == base::meta<interface::Interface>) {
      return type::Interface;
    } else if constexpr (std::is_pointer_v<T>) {
      return type::Ptr(GetType<std::decay_t<decltype(*std::declval<T>())>>());
    } else {
      UNREACHABLE(typeid(T).name());
    }
  }

 private:
  ir::BasicBlock* block_;
  ir::internal::BlockGroupBase* group_;

  // TODO: Early exists from a scope should only destroy a prefix of the
  // variables. We don't handle that concern yet.
  //
  // Each destruction path ending at the destruction of the local variables in a
  // scope continues executing at a particular basic block. This a map
  // associates each such scope to its corresponding landing block.
  absl::flat_hash_map<ast::Scope const*, ir::BasicBlock*> landings_;
  // Given a destruction path starting at a scope `start` and proceeding to
  // destroy all local variables in all scopes up through and including a scope
  // `end`, this map associates the pair `{start, end}` with the block to which
  // you should jump to execute these destructions.
  absl::flat_hash_map<std::pair<ast::Scope const*, ast::Scope const*>,
                      ir::BasicBlock*>
      destruction_blocks_;
};

ir::Reg RegisterReferencing(IrBuilder& builder, type::Type t,
                            ir::PartialResultRef const& value);

}  // namespace compiler

#endif  // ICARUS_COMPILER_IR_BUILDER_H
