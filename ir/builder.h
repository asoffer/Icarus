#ifndef ICARUS_IR_BUILDER_H
#define ICARUS_IR_BUILDER_H

#include <vector>

#include "absl/types/span.h"
#include "ast/overload_set.h"
#include "base/debug.h"
#include "base/meta.h"
#include "base/scope.h"
#include "base/tag.h"
#include "base/untyped_buffer.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/instruction/core.h"
#include "ir/instruction/instructions.h"
#include "ir/local_block_interpretation.h"
#include "ir/out_params.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "type/enum.h"
#include "type/jump.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace ir {

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
  template <typename Tag>
  struct reduced_type<base::Tagged<Tag, Reg>> {
    using type = Tag;
  };

  template <typename T>
  using reduced_type_t = typename reduced_type<T>::type;

  // INSTRUCTIONS

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Add(Lhs const& lhs, Rhs const& rhs) {
    return CurrentBlock()->Append(AddInstruction<reduced_type_t<Lhs>>{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Sub(Lhs const& lhs, Rhs const& rhs) {
    return CurrentBlock()->Append(SubInstruction<reduced_type_t<Lhs>>{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Mul(Lhs const& lhs, Rhs const& rhs) {
    return CurrentBlock()->Append(MulInstruction<reduced_type_t<Lhs>>{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Div(Lhs const& lhs, Rhs const& rhs) {
    return CurrentBlock()->Append(DivInstruction<reduced_type_t<Lhs>>{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Mod(Lhs const& lhs, Rhs const& rhs) {
    return CurrentBlock()->Append(ModInstruction<reduced_type_t<Lhs>>{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

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

  // Flags operators
  RegOr<FlagsVal> XorFlags(RegOr<FlagsVal> const& lhs,
                           RegOr<FlagsVal> const& rhs) {
    if (not lhs.is_reg() and not rhs.is_reg()) {
      return XorFlagsInstruction::Apply(lhs.value(), rhs.value());
    }
    return CurrentBlock()->Append(XorFlagsInstruction{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  RegOr<FlagsVal> AndFlags(RegOr<FlagsVal> const& lhs,
                           RegOr<FlagsVal> const& rhs) {
    if (not lhs.is_reg() and not rhs.is_reg()) {
      return AndFlagsInstruction::Apply(lhs.value(), rhs.value());
    }
    return CurrentBlock()->Append(AndFlagsInstruction{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
  }

  RegOr<FlagsVal> OrFlags(RegOr<FlagsVal> const& lhs,
                          RegOr<FlagsVal> const& rhs) {
    if (not lhs.is_reg() and not rhs.is_reg()) {
      return OrFlagsInstruction::Apply(lhs.value(), rhs.value());
    }
    return CurrentBlock()->Append(OrFlagsInstruction{
        .lhs = lhs, .rhs = rhs, .result = CurrentGroup()->Reserve()});
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
    return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                            uint16_t, uint32_t, uint64_t, float, double,
                            ir::EnumVal, ir::FlagsVal>(
        common_type, [&]<typename T>() {
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

  // Note: Even though this must return a more specific type (BufferPointer
  // instead of Type), we use Type to ensure that if this gets routed into an
  // ir::Value, it will be tagged correctly.
  RegOr<type::Type> BufPtr(RegOr<type::Type> const& val) {
    if (not val.is_reg()) {
      return type::Type(BufPtrInstruction::Apply(val.value()));
    }
    return CurrentBlock()->Append(
        BufPtrInstruction{.operand = val, .result = CurrentGroup()->Reserve()});
  }

  // Note: Even though this must return a more specific type (Pointer instead of
  // Type), we use Type to ensure that if this gets routed into an ir::Value, it
  // will be tagged correctly.
  RegOr<type::Type> Ptr(RegOr<type::Type> const& val) {
    if (not val.is_reg()) {
      return type::Type(PtrInstruction::Apply(val.value()));
    }
    return CurrentBlock()->Append(
        PtrInstruction{.operand = val, .result = CurrentGroup()->Reserve()});
  }

  RegOr<bool> Not(RegOr<bool> const& val) {
    if (not val.is_reg()) { return NotInstruction::Apply(val.value()); }
    return CurrentBlock()->Append(
        NotInstruction{.operand = val, .result = CurrentGroup()->Reserve()});
  }

  RegOr<type::Type> Tup(std::vector<RegOr<type::Type>> types) {
    // TODO constant-folding
    return CurrentBlock()->Append(TupleInstruction{
        .values = std::move(types), .result = CurrentGroup()->Reserve()});
  }

  template <typename ToType>
  RegOr<ToType> CastTo(type::Typed<ir::Value> v) {
    if constexpr (base::meta<ToType> != base::meta<ir::EnumVal> and
                  base::meta<ToType> != base::meta<ir::FlagsVal>) {
      if (v.type() == type::Get<ToType>()) { return v->get<RegOr<ToType>>(); }
    }

    if (v.type() == type::Int8) {
      return Cast<int8_t, ToType>(v->get<RegOr<int8_t>>());
    } else if (v.type() == type::Nat8) {
      return Cast<uint8_t, ToType>(v->get<RegOr<uint8_t>>());
    } else if (v.type() == type::Int16) {
      return Cast<int16_t, ToType>(v->get<RegOr<int16_t>>());
    } else if (v.type() == type::Nat16) {
      return Cast<uint16_t, ToType>(v->get<RegOr<uint16_t>>());
    } else if (v.type() == type::Int32) {
      return Cast<int32_t, ToType>(v->get<RegOr<int32_t>>());
    } else if (v.type() == type::Nat32) {
      return Cast<uint32_t, ToType>(v->get<RegOr<uint32_t>>());
    } else if (v.type() == type::Int64) {
      return Cast<int64_t, ToType>(v->get<RegOr<int64_t>>());
    } else if (v.type() == type::Nat64) {
      return Cast<uint64_t, ToType>(v->get<RegOr<uint64_t>>());
    } else if (v.type() == type::Float32) {
      return Cast<float, ToType>(v->get<RegOr<float>>());
    } else if (v.type() == type::Float64) {
      return Cast<double, ToType>(v->get<RegOr<double>>());
    } else if (v.type().is<type::Enum>()) {
      auto result = CurrentGroup()->Reserve();
      CurrentBlock()->Append(UnwrapEnumInstruction{
          .value = v->get<ir::RegOr<ir::EnumVal>>(), .result = result});
      if constexpr (base::meta<ToType> ==
                    base::meta<ir::EnumVal::underlying_type>) {
        return result;
      } else {
        return Cast<ir::EnumVal::underlying_type, ToType>(result);
      }
    } else if (v.type().is<type::Flags>()) {
      auto result = CurrentGroup()->Reserve();
      CurrentBlock()->Append(UnwrapFlagsInstruction{
          .value = v->get<ir::RegOr<ir::FlagsVal>>(), .result = result});
      if constexpr (base::meta<ToType> ==
                    base::meta<ir::FlagsVal::underlying_type>) {
        return result;
      } else {
        return Cast<ir::FlagsVal::underlying_type, ToType>(result);
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
    return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                            uint16_t, uint32_t, uint64_t, float, double,
                            type::Type, EnumVal, FlagsVal, Addr, String, Fn>(
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

  Reg Index(type::Pointer const* t, Reg array_ptr, RegOr<int64_t> offset) {
    return PtrIncr(array_ptr, offset,
                   type::Ptr(t->pointee().as<type::Array>().data_type()));
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
  // `UncondJump`: Transfers control to `block`.
  // `CondJump`:   Transfers control to one of two blocks depending on a
  //               run-time boolean value.
  // `ReturnJump`: Transfers control back to the calling function.
  //
  // `ChooseJump`: Transfers control to the appropriate block-handler. Note that
  //               this is highly specific to the current scope-definine
  //               language constructs which are likely to change.
  void UncondJump(BasicBlock* block);
  void CondJump(RegOr<bool> cond, BasicBlock* true_block,
                BasicBlock* false_block);
  void ReturnJump();
  // TODO: Probably better to have a data structure for this.
  void ChooseJump(std::vector<std::string_view> names,
                  std::vector<BasicBlock*> blocks,
                  std::vector<core::Arguments<type::Typed<Value>>> args);

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
  RegOr<uint64_t> ByteViewLength(RegOr<ir::String> val);
  RegOr<Addr> ByteViewData(RegOr<ir::String> val);

  // Type construction commands

  // Note: Even though this must return a more specific type (Function instead
  // of Type), we use Type to ensure that if this gets routed into an ir::Value,
  // it will be tagged correctly.
  RegOr<type::Type> Arrow(std::vector<RegOr<type::Type>> const& ins,
                          std::vector<RegOr<type::Type>> const& outs);

  RegOr<type::Type> Array(RegOr<ArrayInstruction::length_t> len,
                          RegOr<type::Type> data_type);

  Reg OpaqueType(module::BasicModule const* mod);

  Reg Enum(type::Enum* e, std::vector<std::string_view> names,
           absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values);
  Reg Flags(type::Flags* f, std::vector<std::string_view> names,
            absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values);

  type::Typed<Reg> LoadSymbol(String name, type::Type type);

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
    } else if constexpr (base::IsTaggedV<T>) {
      static_assert(base::meta<typename T::base_type> == base::meta<Reg>);
      SetRet(n, RegOr<typename T::tag_type>(val));
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
    if (r.is_reg()) {
      return CurrentBlock()->Append(CastInstruction<ToType, FromType>{
          .value = r.reg(), .result = CurrentGroup()->Reserve()});
    } else {
      return static_cast<ToType>(r.value());
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
