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
#include "ir/value/char.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "type/enum.h"
#include "type/jump.h"
#include "type/slice.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace ir {

// TODO: Remove as much as possible from ir::Builder. I'm not exactly sure what
// I want to do with this, but we should really limit it only to core
// instructions and/or instructions that can only be optimized in the streaming
// approach by carrying around extra state (like loads/store-caching).

struct Builder {
  BasicBlock* AddBlock();
  BasicBlock* AddBlock(std::string header);
  BasicBlock* AddBlock(BasicBlock const& to_copy);

  ir::OutParams OutParams(absl::Span<type::Type const> types);
  ir::OutParams OutParamsCopyInit(
      absl::Span<type::Type const> types,
      absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to);
  ir::OutParams OutParamsMoveInit(
      absl::Span<type::Type const> types,
      absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to);
  ir::OutParams OutParamsAssign(
      absl::Span<type::Type const> types,
      absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to);

  template <typename KeyType, typename ValueType>
  absl::flat_hash_map<KeyType, ir::BasicBlock*> AddBlocks(
      absl::flat_hash_map<KeyType, ValueType> const& table) {
    absl::flat_hash_map<KeyType, ir::BasicBlock*> result;
    for (auto const& [key, val] : table) { result.emplace(key, AddBlock()); }
    return result;
  }

  absl::flat_hash_map<ast::Expression const*, ir::BasicBlock*> AddBlocks(
      ast::OverloadSet const& os) {
    absl::flat_hash_map<ast::Expression const*, ir::BasicBlock*> result;
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
    if constexpr (base::meta<Lhs>.template is_a<ir::RegOr>() and
                  base::meta<Rhs>.template is_a<ir::RegOr>()) {
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
  RegOr<bool> Gt(Lhs const& lhs, Rhs const& rhs) {
    return Lt(rhs, lhs);
  }

  template <typename Lhs, typename Rhs>
  RegOr<bool> Le(Lhs const& lhs, Rhs const& rhs) {
    using type = reduced_type_t<Lhs>;
    if constexpr (base::meta<Lhs>.template is_a<ir::RegOr>() and
                  base::meta<Rhs>.template is_a<ir::RegOr>()) {
      if (not lhs.is_reg() and not rhs.is_reg()) {
        return LeInstruction<type>::Apply(lhs.value(), rhs.value());
      }

      return CurrentBlock()->Append(LeInstruction<type>{
          .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
    } else {
      return Le(RegOr<type>(lhs), RegOr<type>(rhs));
    }
  }

  template <typename Lhs, typename Rhs>
  RegOr<bool> Ge(Lhs const& lhs, Rhs const& rhs) {
    return Le(rhs, lhs);
  }

  // Comparison
  template <typename Lhs, typename Rhs>
  RegOr<bool> Eq(Lhs const& lhs, Rhs const& rhs) {
    using type = reduced_type_t<Lhs>;
    if constexpr (base::meta<type> == base::meta<bool>) {
      return EqBool(lhs, rhs);
    } else if constexpr (base::meta<Lhs>.template is_a<ir::RegOr>() and
                         base::meta<Rhs>.template is_a<ir::RegOr>()) {
      if (not lhs.is_reg() and not rhs.is_reg()) {
        return EqInstruction<type>::Apply(lhs.value(), rhs.value());
      }
      return CurrentBlock()->Append(EqInstruction<type>{
          .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
    } else {
      return Eq(RegOr<type>(lhs), RegOr<type>(rhs));
    }
  }

  RegOr<bool> Eq(type::Type common_type, ir::Value const& lhs_val,
                 ir::Value const& rhs_val) {
    return type::ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
                            uint8_t, uint16_t, uint32_t, uint64_t, float,
                            double>(common_type, [&]<typename T>() {
      return Eq(lhs_val.get<RegOr<T>>(), rhs_val.get<RegOr<T>>());
    });
  }

  template <typename Lhs, typename Rhs>
  RegOr<bool> Ne(Lhs const& lhs, Rhs const& rhs) {
    using type = reduced_type_t<Lhs>;
    if constexpr (std::is_same_v<type, bool>) {
      return NeBool(lhs, rhs);
    } else if constexpr (base::meta<Lhs>.template is_a<ir::RegOr>() and
                         base::meta<Rhs>.template is_a<ir::RegOr>()) {
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
  RegOr<ToType> CastTo(type::Typed<ir::Value> v) {
    if (v.type() == type::I8) {
      return Cast<int8_t, ToType>(v->get<RegOr<int8_t>>());
    } else if (v.type() == type::U8) {
      return Cast<uint8_t, ToType>(v->get<RegOr<uint8_t>>());
    } else if (v.type() == type::I16) {
      return Cast<int16_t, ToType>(v->get<RegOr<int16_t>>());
    } else if (v.type() == type::U16) {
      return Cast<uint16_t, ToType>(v->get<RegOr<uint16_t>>());
    } else if (v.type() == type::I32) {
      return Cast<int32_t, ToType>(v->get<RegOr<int32_t>>());
    } else if (v.type() == type::U32) {
      return Cast<uint32_t, ToType>(v->get<RegOr<uint32_t>>());
    } else if (v.type() == type::I64) {
      return Cast<int64_t, ToType>(v->get<RegOr<int64_t>>());
    } else if (v.type() == type::U64) {
      return Cast<uint64_t, ToType>(v->get<RegOr<uint64_t>>());
    } else if (v.type() == type::F32) {
      return Cast<float, ToType>(v->get<RegOr<float>>());
    } else if (v.type() == type::F64) {
      return Cast<double, ToType>(v->get<RegOr<double>>());
    } else if (v.type() == type::Char) {
      return Cast<ir::Char, ToType>(v->get<RegOr<ir::Char>>());
    } else if (v.type().is<type::Enum>()) {
      if constexpr (base::meta<ToType> ==
                    base::meta<type::Enum::underlying_type>) {
        return v->get<ir::RegOr<type::Enum::underlying_type>>();
      } else {
        return Cast<type::Enum::underlying_type, ToType>(
            v->get<type::Enum::underlying_type>());
      }
    } else if (v.type().is<type::Flags>()) {
      if constexpr (base::meta<ToType> ==
                    base::meta<type::Flags::underlying_type>) {
        return v->get<ir::RegOr<type::Flags::underlying_type>>();
      } else {
        return Cast<type::Flags::underlying_type, ToType>(
            v->get<type::Flags::underlying_type>());
      }
    } else {
      UNREACHABLE(base::meta<ToType>, ", ", v.type());
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
    auto end_ptr =
        PtrIncr(ptr, static_cast<int32_t>(t->length()), data_ptr_type);

    auto* start_block = CurrentBlock();
    auto* loop_body   = AddBlock();
    auto* land_block  = AddBlock();
    auto* cond_block  = AddBlock();

    UncondJump(cond_block);

    CurrentBlock() = cond_block;
    auto* phi      = PhiInst<Addr>();
    CondJump(Eq(RegOr<Addr>(phi->result), end_ptr), land_block, loop_body);

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
    return desired_type.get()->is_big() ? r : Load(r, desired_type).get<Reg>();
  }

  template <typename T>
  RegOr<T> Load(RegOr<Addr> addr) {
    auto& blk = *CurrentBlock();

    // TODO Just take a Reg. RegOr<Addr> is overkill and not possible because
    // constants don't have addresses.
    Value& cache_results = blk.load_store_cache().slot<T>(addr);
    if (not cache_results.empty()) {
      // TODO may not be Reg. could be anything of the right type.
      return cache_results.get<RegOr<T>>();
    }

    LoadInstruction inst{.num_bytes = core::Bytes::Get<T>().value(),
                         .addr      = addr};
    auto result = inst.result = CurrentGroup()->Reserve();

    cache_results = Value(result);

    blk.Append(std::move(inst));
    return result;
  }

  Value Load(RegOr<Addr> r, type::Type t) {
    using base::stringify;
    LOG("Load", "Calling Load(%s, %s)", r, t.to_string());
    if (t.is<type::Function>()) { return Value(Load<Fn>(r)); }
    return type::ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
                            uint8_t, uint16_t, uint32_t, uint64_t, float,
                            double, type::Type, Addr, Fn>(
        t, [&]<typename T>() { return Value(Load<T>(r)); });
  }

  template <typename T>
  void Store(T r, RegOr<Addr> addr) {
    if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
      auto& blk = *CurrentBlock();
      blk.load_store_cache().clear<typename T::type>();
      blk.Append(
          StoreInstruction<typename T::type>{.value = r, .location = addr});
    } else {
      Store(RegOr<T>(r), addr);
    }
  }

  Reg GetRet(uint16_t n, type::Type t) {
    GetReturnInstruction inst{.index = n};
    auto result = inst.result = CurrentGroup()->Reserve();
    CurrentBlock()->Append(std::move(inst));
    return result;
  }

  Reg Index(type::Pointer const* t, RegOr<Addr> addr, RegOr<int64_t> offset) {
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
  void Move(type::Typed<RegOr<Addr>> to, type::Typed<Reg> from);
  void Copy(type::Typed<RegOr<Addr>> to, type::Typed<Reg> from);

  // Data structure access commands. For structs and tuples, `Fields` takes an
  // address of the data structure and returns the address of the particular
  // field requested. For variants, `VariantType` computes the location where
  // the type is stored and `VariantValue` accesses the location where the
  // value is stored.
  type::Typed<Reg> FieldRef(RegOr<Addr> r, type::Struct const* t, int64_t n);
  type::Typed<Reg> FieldRef(RegOr<Addr> r, type::Tuple const* t, int64_t n);

  type::Typed<Value> FieldValue(RegOr<Addr> r, type::Struct const* t,
                                int64_t n) {
    auto typed_reg = FieldRef(r, t, n);
    return type::Typed<Value>(Value(PtrFix(*typed_reg, typed_reg.type())),
                              typed_reg.type());
  }
  type::Typed<Value> FieldValue(RegOr<Addr> r, type::Tuple const* t,
                                int64_t n) {
    auto typed_reg = FieldRef(r, t, n);
    return type::Typed<Value>(Value(PtrFix(*typed_reg, typed_reg.type())),
                              typed_reg.type());
  }

  Reg PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc, type::Pointer const* t);

  // Low-level size/alignment commands
  Reg Align(RegOr<type::Type> r);
  Reg Bytes(RegOr<type::Type> r);

  Reg Alloca(type::Type t);
  Reg TmpAlloca(type::Type t);

  Reg MakeBlock(Block block, std::vector<RegOr<Fn>> befores,
                std::vector<RegOr<Jump>> afters);
  Reg MakeScope(ir::Scope scope, std::vector<RegOr<Jump>> inits,
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

  template <typename T>
  void SetRet(uint16_t n, T val) {
    if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
      SetReturnInstruction<typename T::type> inst{.index = n, .value = val};
      CurrentBlock()->Append(std::move(inst));
    } else {
      SetRet(n, RegOr<T>(val));
    }
  }

  void SetRet(uint16_t n, type::Typed<Value> const& r) {
    ASSERT(r.type().get()->is_big() == false);
    type::Apply(r.type(), [&]<typename T>() { SetRet(n, r->get<RegOr<T>>()); });
  }

 private:
  template <typename FromType, typename ToType>
  RegOr<ToType> Cast(RegOr<FromType> r) {
    if constexpr (base::meta<ToType> == base::meta<FromType>) {
      return r;
    } else if constexpr (base::meta<FromType>.template converts_to<ToType>()) {
      if (r.is_reg()) {
        return CurrentBlock()->Append(CastInstruction<ToType, FromType>{
            .value = r.reg(), .result = CurrentGroup()->Reserve()});
      } else {
        return static_cast<ToType>(r.value());
      }
    } else {
      UNREACHABLE(base::meta<FromType>, " cannot be cast to ",
                  base::meta<ToType>);
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
};

struct SetCurrent : public base::UseWithScope {
  explicit SetCurrent(internal::BlockGroupBase& fn, Builder& builder);
  explicit SetCurrent(NativeFn fn, Builder& builder)
      : SetCurrent(*fn.get(), builder) {}
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
