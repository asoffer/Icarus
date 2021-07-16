#ifndef ICARUS_IR_BUILDER_H
#define ICARUS_IR_BUILDER_H

#include <vector>

#include "absl/types/span.h"
#include "ast/overload_set.h"
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
#include "ir/local_block_interpretation.h"
#include "ir/out_params.h"
#include "ir/value/addr.h"
#include "ir/value/block.h"
#include "ir/value/char.h"
#include "ir/value/module_id.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "ir/value/string.h"
#include "ir/value/value.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/interface/interface.h"
#include "type/jump.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace ir {

// TODO: Remove as much as possible from Builder. I'm not exactly sure what
// I want to do with this, but we should really limit it only to core
// instructions and/or instructions that can only be optimized in the streaming
// approach by carrying around extra state (like loads/store-caching).

struct Builder {
  BasicBlock* AddBlock();
  BasicBlock* AddBlock(std::string header);
  BasicBlock* AddBlock(BasicBlock const& to_copy);

  ir::OutParams OutParams(
      absl::Span<type::Type const> types,
      absl::Span<type::Typed<RegOr<addr_t>> const> to = {});

  template <typename KeyType, typename ValueType>
  absl::flat_hash_map<KeyType, BasicBlock*> AddBlocks(
      absl::flat_hash_map<KeyType, ValueType> const& table) {
    absl::flat_hash_map<KeyType, BasicBlock*> result;
    for (auto const& [key, val] : table) { result.emplace(key, AddBlock()); }
    return result;
  }

  absl::flat_hash_map<ast::Expression const*, BasicBlock*> AddBlocks(
      ast::OverloadSet const& os) {
    absl::flat_hash_map<ast::Expression const*, BasicBlock*> result;
    for (auto const* overload : os.members()) {
      result.emplace(overload, AddBlock());
    }
    return result;
  }

  Reg Reserve() { return CurrentGroup()->Reserve(); }

  internal::BlockGroupBase*& CurrentGroup() { return current_.group_; }
  BasicBlock*& CurrentBlock() { return current_.block_; }

  template <typename T>
  struct reduced_type {
    using type = T;
  };
  template <typename T>
  struct reduced_type<RegOr<T>> {
    using type = T;
  };

  template <typename T>
  using reduced_type_t = typename reduced_type<T>::type;

  // INSTRUCTIONS

  template <typename Lhs, typename Rhs>
  RegOr<bool> Lt(Lhs const& lhs, Rhs const& rhs) {
    using type = reduced_type_t<Lhs>;
    if constexpr (base::meta<Lhs>.template is_a<RegOr>() and
                  base::meta<Rhs>.template is_a<RegOr>()) {
      if (not lhs.is_reg() and not rhs.is_reg()) {
        return LtInstruction<type>::Apply(lhs.value(), rhs.value());
      }

      return CurrentBlock()->Append(LtInstruction<reduced_type_t<Lhs>>{
          .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
    } else {
      return Lt(RegOr<type>(lhs), RegOr<type>(rhs));
    }
  }

  template <typename Lhs, typename Rhs>
  RegOr<bool> Le(Lhs const& lhs, Rhs const& rhs) {
    using type = reduced_type_t<Lhs>;
    if constexpr (base::meta<Lhs>.template is_a<RegOr>() and
                  base::meta<Rhs>.template is_a<RegOr>()) {
      if (not lhs.is_reg() and not rhs.is_reg()) {
        return LeInstruction<type>::Apply(lhs.value(), rhs.value());
      }

      return CurrentBlock()->Append(LeInstruction<type>{
          .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
    } else {
      return Le(RegOr<type>(lhs), RegOr<type>(rhs));
    }
  }

  // Comparison
  template <typename Lhs, typename Rhs>
  RegOr<bool> Eq(Lhs const& lhs, Rhs const& rhs) {
    using type = reduced_type_t<Lhs>;
    if constexpr (base::meta<type> == base::meta<bool>) {
      return EqBool(lhs, rhs);
    } else if constexpr (base::meta<Lhs>.template is_a<RegOr>() and
                         base::meta<Rhs>.template is_a<RegOr>()) {
      if (not lhs.is_reg() and not rhs.is_reg()) {
        return EqInstruction<type>::Apply(lhs.value(), rhs.value());
      }
      return CurrentBlock()->Append(EqInstruction<type>{
          .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
    } else {
      return Eq(RegOr<type>(lhs), RegOr<type>(rhs));
    }
  }

  template <typename Lhs, typename Rhs>
  RegOr<bool> Ne(Lhs const& lhs, Rhs const& rhs) {
    using type = reduced_type_t<Lhs>;
    if constexpr (std::is_same_v<type, bool>) {
      return NeBool(lhs, rhs);
    } else if constexpr (base::meta<Lhs>.template is_a<RegOr>() and
                         base::meta<Rhs>.template is_a<RegOr>()) {
      if (not lhs.is_reg() and not rhs.is_reg()) {
        return NeInstruction<type>::Apply(lhs.value(), rhs.value());
      }
      return CurrentBlock()->Append(NeInstruction<type>{
          .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
    } else {
      return Ne(RegOr<type>(lhs), RegOr<type>(rhs));
    }
  }

  template <typename T>
  RegOr<T> Neg(RegOr<T> const& val) {
    if (not val.is_reg()) { return NegInstruction<T>::Apply(val.value()); }
    return CurrentBlock()->Append(NegInstruction<reduced_type_t<T>>{
        .operand = val, .result = CurrentGroup()->Reserve()});
  }

  RegOr<bool> Not(RegOr<bool> const& val) {
    if (not val.is_reg()) { return NotInstruction::Apply(val.value()); }
    return CurrentBlock()->Append(
        NotInstruction{.operand = val, .result = CurrentGroup()->Reserve()});
  }

  template <typename ToType>
  RegOr<ToType> CastTo(type::Type t, PartialResultRef const& buffer) {
    if (t == GetType<ToType>()) { return buffer.get<ToType>(); }
    if (auto const* p = t.if_as<type::Primitive>()) {
      return p->Apply([&]<typename T>()->RegOr<ToType> {
        return Cast<T, ToType>(buffer.get<T>());
      });
    } else if (t.is<type::Enum>()) {
      if constexpr (base::meta<ToType> ==
                    base::meta<type::Enum::underlying_type>) {
        return buffer.get<type::Enum::underlying_type>();
      } else {
        return Cast<type::Enum::underlying_type, ToType>(
            buffer.get<type::Enum::underlying_type>());
      }
    } else if (t.is<type::Flags>()) {
      if constexpr (base::meta<ToType> ==
                    base::meta<type::Flags::underlying_type>) {
        return buffer.get<type::Flags::underlying_type>();
      } else {
        return Cast<type::Flags::underlying_type, ToType>(
            buffer.get<type::Flags::underlying_type>());
      }
    } else {
      UNREACHABLE(base::meta<ToType>, ", ", t);
    }
  }

  template <typename FromType, typename ToType>
  RegOr<ToType> Cast(RegOr<FromType> r) {
    if constexpr (base::meta<ToType> == base::meta<FromType>) {
      return r;
    } else if constexpr ((base::meta<FromType> == base::meta<Integer> and
                          std::is_integral_v<ToType>) or
                         base::meta<FromType>.template converts_to<ToType>()) {
      if (r.is_reg()) {
        return CurrentBlock()->Append(CastInstruction<ToType(FromType)>{
            .value = r.reg(), .result = CurrentGroup()->Reserve()});
      } else {
        if constexpr (base::meta<FromType> == base::meta<Integer>) {
          return static_cast<ToType>(r.value().value());
        } else {
          return static_cast<ToType>(r.value());
        }
      }
    } else {
      UNREACHABLE(base::meta<FromType>, " cannot be cast to ",
                  base::meta<ToType>);
    }
  }

  // Phi instruction. Takes a span of basic blocks and a span of (registers or)
  // values. As a precondition, the number of blocks must be equal to the number
  // of values. This instruction evaluates to the value `values[i]` if the
  // previous block was `blocks[i]`.
  //
  // In the first overload, the resulting value is assigned to `r`. In the
  // second overload, a register is constructed to represent the value.
  template <typename T>
  void Phi(Reg r, std::vector<BasicBlock const*> blocks,
           std::vector<RegOr<T>> values) {
    ASSERT(blocks.size() == values.size());
    PhiInstruction<T> inst(std::move(blocks), std::move(values));
    inst.result = r;
  }

  template <typename T>
  RegOr<T> Phi(std::vector<BasicBlock const*> blocks,
               std::vector<RegOr<T>> values) {
    if (values.size() == 1u) { return values[0]; }
    PhiInstruction<T> inst(std::move(blocks), std::move(values));
    auto result = inst.result = CurrentGroup()->Reserve();
    CurrentBlock()->Append(std::move(inst));
    return result;
  }

  // Usually it is sufficient to determine all the inputs to a phi instruction
  // upfront, but sometimes it is useful to construct a phi instruction without
  // having set its inputs.
  //
  // TODO: Right now we are relying on the fact that Inst stores values on the
  // heap, but this may not always be the case.
  template <typename T>
  PhiInstruction<T>* PhiInst() {
    PhiInstruction<T> inst;
    inst.result = CurrentGroup()->Reserve();
    CurrentBlock()->Append(std::move(inst));
    return CurrentBlock()
        ->instructions()
        .back()
        .template if_as<PhiInstruction<T>>();
  }

  template <typename F>
  void OnEachArrayElement(type::Array const* t, Reg array_reg, F fn) {
    auto* data_ptr_type = type::Ptr(t->data_type());

    auto ptr = PtrIncr(array_reg, 0, type::Ptr(data_ptr_type));
    auto end_ptr = PtrIncr(ptr, t->length().value(), data_ptr_type);

    auto* start_block = CurrentBlock();
    auto* loop_body   = AddBlock();
    auto* land_block  = AddBlock();
    auto* cond_block  = AddBlock();

    UncondJump(cond_block);

    CurrentBlock() = cond_block;
    auto* phi      = PhiInst<addr_t>();
    CondJump(Eq(RegOr<addr_t>(phi->result), end_ptr), land_block, loop_body);

    CurrentBlock() = loop_body;
    fn(phi->result);
    Reg next = PtrIncr(phi->result, 1, data_ptr_type);
    UncondJump(cond_block);

    phi->add(start_block, ptr);
    phi->add(CurrentBlock(), next);

    CurrentBlock() = land_block;
  }

  void Comment(std::string s) {
    CurrentBlock()->Append(CommentInstruction{.comment = std::move(s)});
  }

  Reg PtrFix(Reg r, type::Type desired_type) {
    // TODO must this be a register if it's loaded?
    if (desired_type.get()->is_big()) { return r; }
    ir::PartialResultBuffer buffer;
    Load(r, desired_type, buffer);
    return buffer.get<Reg>(0);
  }

  template <typename T>
  RegOr<T> Load(RegOr<addr_t> addr, type::Type t = GetType<T>()) {
    auto& blk = *CurrentBlock();

    // TODO Just take a Reg. RegOr<addr_t> is overkill and not possible because
    // constants don't have addresses.
    Value& cache_results = blk.load_store_cache().slot<T>(addr);
    if (not cache_results.empty()) {
      // TODO may not be Reg. could be anything of the right type.
      return cache_results.get<RegOr<T>>();
    }

    LoadInstruction inst{.type = t, .addr = addr};
    auto result = inst.result = CurrentGroup()->Reserve();

    cache_results = Value(result);

    blk.Append(std::move(inst));
    return result;
  }

  void Load(RegOr<addr_t> r, type::Type t, PartialResultBuffer& out) {
    LOG("Load", "Calling Load(%s, %s)", r, t.to_string());
    if (t.is<type::Function>()) {
      out.append(Load<Fn>(r, t));
    } else if (t.is<type::Pointer>()) {
      out.append(Load<addr_t>(r, t));
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
  void Store(T r, RegOr<addr_t> addr) {
    if constexpr (base::meta<T>.template is_a<RegOr>()) {
      auto& blk = *CurrentBlock();
      blk.load_store_cache().clear<typename T::type>();
      blk.Append(
          StoreInstruction<typename T::type>{.value = r, .location = addr});
    } else {
      Store(RegOr<T>(r), addr);
    }
  }

  Reg Index(type::Pointer const* t, RegOr<addr_t> addr, RegOr<int64_t> offset) {
    type::Type pointee = t->pointee();
    type::Type data_type;
    if (auto const* a = pointee.if_as<type::Array>()) {
      data_type = a->data_type();
    } else if (auto const* s = pointee.if_as<type::Slice>()) {
      data_type = s->data_type();
    } else {
      UNREACHABLE(t->to_string());
    }

    return PtrIncr(addr, offset, type::Ptr(data_type));
  }

  // Emits a function-call instruction, calling `fn` of type `f` with the given
  // `arguments` and output parameters. If output parameters are not present,
  // the function must return nothing.
  void Call(RegOr<Fn> const& fn, type::Function const* f,
            std::vector<Value> args, ir::OutParams outs);

  // Jump instructions must be the last instruction in a basic block. They
  // handle control-flow, indicating which basic block control should be
  // transferred to next.
  //
  // `UncondJump`:   Transfers control to `block`.
  // `CondJump`:     Transfers control to one of two blocks depending on a
  //                 run-time boolean value.
  // `ReturnJump`:   Transfers control back to the calling function.
  // `ChooseJump`:   Transfers control to the appropriate block-handler. Note
  //                 that this is highly specific to the current scope-definine
  //                 language constructs which are likely to change.
  // `JumpExitJump`: Transfers control to the calling jump/function, specifying
  //                 the block it came from.
  void UncondJump(BasicBlock* block);
  void CondJump(RegOr<bool> cond, BasicBlock* true_block,
                BasicBlock* false_block);
  void ReturnJump();
  // TODO: Probably better to have a data structure for this.
  void ChooseJump(
      std::vector<std::string_view> names, std::vector<BasicBlock*> blocks,
      std::vector<core::Arguments<std::pair<Value, type::QualType>>> args);
  void JumpExitJump(std::string name, BasicBlock* choose_block);

  template <bool B>
  BasicBlock* EarlyExitOn(BasicBlock* exit_block, RegOr<bool> cond) {
    auto* continue_block = AddBlock();
    if constexpr (B) {
      CondJump(cond, exit_block, continue_block);
    } else {
      CondJump(cond, continue_block, exit_block);
    }
    return continue_block;
  }

  // Special members function instructions. Calling these typically calls
  // builtin functions (or, in the case of primitive types, do nothing).
  void Move(type::Typed<RegOr<addr_t>> to, type::Typed<Reg> from);
  void Copy(type::Typed<RegOr<addr_t>> to, type::Typed<Reg> from);

  // Data structure access commands. For structs, `Fields` takes an
  // address of the data structure and returns the address of the particular
  // field requested. For variants, `VariantType` computes the location where
  // the type is stored and `VariantValue` accesses the location where the
  // value is stored.
  type::Typed<Reg> FieldRef(RegOr<addr_t> r, type::Struct const* t, int64_t n);

  type::Typed<Value> FieldValue(RegOr<addr_t> r, type::Struct const* t,
                                int64_t n) {
    auto typed_reg = FieldRef(r, t, n);
    return type::Typed<Value>(Value(PtrFix(*typed_reg, typed_reg.type())),
                              typed_reg.type());
  }
  Reg PtrIncr(RegOr<addr_t> ptr, RegOr<int64_t> inc, type::Pointer const* t);

  // Low-level size/alignment commands
  Reg Align(RegOr<type::Type> r);
  Reg Bytes(RegOr<type::Type> r);

  Reg Alloca(type::Type t);
  Reg TmpAlloca(type::Type t);

  void MakeBlock(Block block, std::vector<RegOr<Fn>> befores,
                 std::vector<RegOr<Jump>> afters);
  void MakeScope(Scope scope, std::vector<RegOr<Jump>> inits,
                 std::vector<RegOr<Fn>> dones,
                 absl::flat_hash_map<std::string_view, Block> blocks);

  void DebugIr() { CurrentBlock()->Append(DebugIrInstruction{}); }

  // Apply the callable to each temporary in reverse order, and clear the list
  // of temporaries.
  template <typename Fn>
  void FinishTemporariesWith(Fn&& fn) {
    for (auto iter = current_.temporaries_to_destroy_.rbegin();
         iter != current_.temporaries_to_destroy_.rend(); ++iter) {
      fn(*iter);
    }
    current_.temporaries_to_destroy_.clear();
  }

  enum class BlockTerminationState {
    kMoreStatements,  // Not at the end of the block yet
    kNoTerminator,    // Block complete; no `return` or `<<`
    kReturn,          // Block completed with `return`
    kGoto,            // Block completed with `goto`
    kLabeledYield,    // Block completed with `#.my_label << `
    kYield,           // Block completed with `<<`
  };
  constexpr BlockTerminationState block_termination_state() const {
    return current_.block_termination_state_;
  }
  constexpr BlockTerminationState& block_termination_state() {
    return current_.block_termination_state_;
  }

  Reg addr(ast::Declaration::Id const* id) const { return addr_.at(id); }
  void set_addr(ast::Declaration::Id const* id, Reg addr) {
    addr_.emplace(id, addr);
  }

 private:
  template <typename T>
  static type::Type GetType() {
    if constexpr (base::meta<T> == base::meta<bool>) {
      return type::Bool;
    } else if constexpr (base::meta<T> == base::meta<Integer>) {
      return type::Integer;
    } else if constexpr (base::meta<T> == base::meta<Char>) {
      return type::Char;
    } else if constexpr (base::meta<T> == base::meta<memory_t>) {
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
    } else if constexpr (base::meta<T> == base::meta<Block>) {
      return type::Block;
    } else if constexpr (base::meta<T> == base::meta<type::Type>) {
      return type::Type_;
    } else if constexpr (base::meta<T> == base::meta<Scope>) {
      return type::Scope;
    } else if constexpr (base::meta<T> == base::meta<ModuleId>) {
      return type::Module;
    } else if constexpr (base::meta<T> == base::meta<interface::Interface>) {
      return type::Interface;
    } else if constexpr (std::is_pointer_v<T>) {
      return type::Ptr(GetType<std::decay_t<decltype(*std::declval<T>())>>());
    } else {
      UNREACHABLE(typeid(T).name());
    }
  }

  RegOr<bool> EqBool(RegOr<bool> const& lhs, RegOr<bool> const& rhs) {
    if (not lhs.is_reg()) { return lhs.value() ? rhs : Not(rhs); }
    if (not rhs.is_reg()) { return rhs.value() ? lhs : Not(lhs); }
    return CurrentBlock()->Append(EqInstruction<bool>{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  RegOr<bool> NeBool(RegOr<bool> const& lhs, RegOr<bool> const& rhs) {
    if (not lhs.is_reg()) { return lhs.value() ? Not(rhs) : rhs; }
    if (not rhs.is_reg()) { return rhs.value() ? Not(lhs) : lhs; }
    return CurrentBlock()->Append(NeInstruction<bool>{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  friend struct SetCurrent;
  friend struct SetTemporaries;

  struct State {
    internal::BlockGroupBase* group_ = nullptr;
    BasicBlock* block_;

    // Temporaries need to be destroyed at the end of each statement.
    // This is a pointer to a buffer where temporary allocations can register
    // themselves for deletion.
    std::vector<type::Typed<Reg>> temporaries_to_destroy_;
    BlockTerminationState block_termination_state_ =
        BlockTerminationState::kMoreStatements;
  } current_;

  // Stores addresses of local identifiers
  absl::flat_hash_map<ast::Declaration::Id const *, Reg> addr_;
};

struct SetCurrent : public base::UseWithScope {
  explicit SetCurrent(internal::BlockGroupBase& fn, Builder& builder);
  explicit SetCurrent(NativeFn fn, Builder& builder)
      : SetCurrent(*fn, builder) {}
  ~SetCurrent();

 private:
  Builder& builder_;
  internal::BlockGroupBase* old_group_;
  BasicBlock* old_block_;
  Builder::BlockTerminationState old_termination_state_;
};

struct SetTemporaries : public base::UseWithScope {
  SetTemporaries(Builder& bldr) : bldr_(bldr) {
    old_temporaries_ = std::exchange(bldr_.current_.temporaries_to_destroy_,
                                     std::vector<type::Typed<Reg>>{});
  }
  ~SetTemporaries() {}

 private:
  std::vector<type::Typed<Reg>> old_temporaries_;
  Builder& bldr_;
};

}  // namespace ir

#endif  // ICARUS_IR_BUILDER_H
