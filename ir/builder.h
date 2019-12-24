#ifndef ICARUS_IR_BUILDER_H
#define ICARUS_IR_BUILDER_H

#include <vector>

#include "absl/types/span.h"
#include "base/debug.h"
#include "base/scope.h"
#include "base/tag.h"
#include "base/untyped_buffer.h"
#include "ir/addr.h"
#include "ir/basic_block.h"
#include "ir/block_group.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jump.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/cmd/util.h"
#include "ir/instructions.h"
#include "ir/local_block_interpretation.h"
#include "ir/reg.h"
#include "ir/struct_field.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace ir {
struct Builder;

namespace internal {

template <typename SizeType, typename T>
void Serialize(base::untyped_buffer*, absl::Span<RegOr<T> const>);

template <typename CmdType, typename T>
auto MakeBinaryCmd(RegOr<T>, RegOr<T>, Builder*);

template <typename CmdType>
RegOr<typename CmdType::type> MakeVariadicCmd(
    absl::Span<RegOr<typename CmdType::type> const> vals, Builder*);

template <typename T>
auto PrepareCmdArg(T&& arg);

}  // namespace internal

struct Builder {
  BasicBlock* AddBlock();
  BasicBlock* AddBlock(BasicBlock const& to_copy);

  template <typename KeyType, typename ValueType>
  absl::flat_hash_map<KeyType, ir::BasicBlock*> AddBlocks(
      absl::flat_hash_map<KeyType, ValueType> const& table) {
    absl::flat_hash_map<KeyType, ir::BasicBlock*> result;
    for (auto const& [key, val] : table) { result.emplace(key, AddBlock()); }
    return result;
  }

  internal::BlockGroup*& CurrentGroup() { return current_.group_; }
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
    auto inst = std::make_unique<AddInstruction<reduced_type_t<Lhs>>>(lhs, rhs);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Sub(Lhs const& lhs, Rhs const& rhs) {
    auto inst = std::make_unique<SubInstruction<reduced_type_t<Lhs>>>(lhs, rhs);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Mul(Lhs const& lhs, Rhs const& rhs) {
    auto inst = std::make_unique<MulInstruction<reduced_type_t<Lhs>>>(lhs, rhs);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Div(Lhs const& lhs, Rhs const& rhs) {
    auto inst = std::make_unique<DivInstruction<reduced_type_t<Lhs>>>(lhs, rhs);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  template <typename Lhs, typename Rhs>
  RegOr<reduced_type_t<Lhs>> Mod(Lhs const& lhs, Rhs const& rhs) {
    auto inst = std::make_unique<ModInstruction<reduced_type_t<Lhs>>>(lhs, rhs);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  template <typename Lhs, typename Rhs>
  auto Lt(Lhs const& lhs, Rhs const& rhs) {
    auto inst = std::make_unique<LtInstruction<reduced_type_t<Lhs>>>(lhs, rhs);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  template <typename Lhs, typename Rhs>
  auto Gt(Lhs const& lhs, Rhs const& rhs) {
    return Lt(rhs, lhs);
  }

  template <typename Lhs, typename Rhs>
  auto Le(Lhs const& lhs, Rhs const& rhs) {
    auto inst = std::make_unique<LeInstruction<reduced_type_t<Lhs>>>(lhs, rhs);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  template <typename Lhs, typename Rhs>
  auto Ge(Lhs const& lhs, Rhs const& rhs) {
    return Le(rhs, lhs);
  }

#define ICARUS_IR_DEFINE_CMD(name)                                             \
  template <typename Lhs, typename Rhs>                                        \
  auto name(Lhs&& lhs, Rhs&& rhs) {                                            \
    return internal::MakeBinaryCmd<name##Cmd>(                                 \
        internal::PrepareCmdArg(std::forward<Lhs>(lhs)),                       \
        internal::PrepareCmdArg(std::forward<Rhs>(rhs)), this);                \
  }

  // Flags operators
  ICARUS_IR_DEFINE_CMD(XorFlags);
  ICARUS_IR_DEFINE_CMD(AndFlags);
  ICARUS_IR_DEFINE_CMD(OrFlags);
#undef ICARUS_IR_DEFINE_CMD

  template <typename Lhs, typename Rhs>
  auto Eq(Lhs&& lhs, Rhs&& rhs) {
    using lhs_t = std::decay_t<Lhs>;
    using rhs_t = std::decay_t<Rhs>;
    if constexpr (std::is_same_v<lhs_t, bool>) {
      return lhs ? rhs : Not(rhs);
    } else if constexpr (std::is_same_v<lhs_t, RegOr<bool>>) {
      if (lhs.is_reg()) {
        return internal::MakeBinaryCmd<EqCmd>(
            internal::PrepareCmdArg(std::forward<Lhs>(lhs)),
            internal::PrepareCmdArg(std::forward<Rhs>(rhs)), this);
      } else {
        return lhs.value() ? rhs : Not(rhs);
      }
    } else if constexpr (std::is_same_v<rhs_t, bool>) {
      return rhs ? lhs : Not(lhs);
    } else if constexpr (std::is_same_v<rhs_t, RegOr<bool>>) {
      if (rhs.is_reg()) {
        return internal::MakeBinaryCmd<EqCmd>(
            internal::PrepareCmdArg(std::forward<Lhs>(lhs)),
            internal::PrepareCmdArg(std::forward<Rhs>(rhs)), this);
      } else {
        return rhs.value() ? lhs : Not(lhs);
      }
    } else {
      return internal::MakeBinaryCmd<EqCmd>(
          internal::PrepareCmdArg(std::forward<Lhs>(lhs)),
          internal::PrepareCmdArg(std::forward<Rhs>(rhs)), this);
    }
  }

  template <typename Lhs, typename Rhs>
  auto Ne(Lhs&& lhs, Rhs&& rhs) {
    using lhs_t = std::decay_t<Lhs>;
    using rhs_t = std::decay_t<Rhs>;
    if constexpr (std::is_same_v<lhs_t, bool>) {
      return lhs ? Not(rhs) : rhs;
    } else if constexpr (std::is_same_v<lhs_t, RegOr<bool>>) {
      if (lhs.is_reg()) {
        return internal::MakeBinaryCmd<EqCmd>(
            internal::PrepareCmdArg(std::forward<Lhs>(lhs)),
            internal::PrepareCmdArg(std::forward<Rhs>(rhs)), this);
      } else {
        return lhs.value() ? Not(rhs) : rhs;
      }
    } else if constexpr (std::is_same_v<rhs_t, bool>) {
      return rhs ? Not(lhs) : lhs;
    } else if constexpr (std::is_same_v<rhs_t, RegOr<bool>>) {
      if (rhs.is_reg()) {
        return internal::MakeBinaryCmd<EqCmd>(
            internal::PrepareCmdArg(std::forward<Lhs>(lhs)),
            internal::PrepareCmdArg(std::forward<Rhs>(rhs)), this);
      } else {
        return rhs.value() ? Not(lhs) : lhs;
      }
    } else {
      return internal::MakeBinaryCmd<EqCmd>(
          internal::PrepareCmdArg(std::forward<Lhs>(lhs)),
          internal::PrepareCmdArg(std::forward<Rhs>(rhs)), this);
    }
  }

  template <typename T>
  RegOr<T> Neg(RegOr<T> const& val) {
    using InstrT = NegInstruction<T>;
    if (not val.is_reg()) { return InstrT::Apply(val.value()); }
    auto inst   = std::make_unique<NegInstruction<reduced_type_t<T>>>(val);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    CurrentBlock()->instructions_.push_back(std::move(inst));
    return result;
  }

  RegOr<type::BufferPointer const*> BufPtr(
      RegOr<type::Type const*> const& val) {
    using InstrT = BufPtrInstruction;
    if (not val.is_reg()) { return InstrT::Apply(val.value()); }
    auto inst   = std::make_unique<InstrT>(val);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    return result;
  }

  RegOr<type::Pointer const*> Ptr(RegOr<type::Type const*> const& val) {
    using InstrT = PtrInstruction;
    if (not val.is_reg()) { return InstrT::Apply(val.value()); }
    auto inst   = std::make_unique<InstrT>(val);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    return result;
  }

  RegOr<bool> Not(RegOr<bool> const& val) {
    using InstrT = NotInstruction;
    if (not val.is_reg()) { return InstrT::Apply(val.value()); }
    auto inst    = std::make_unique<InstrT>(val);
    auto result = inst->result = CurrentGroup()->Reserve(nullptr);
    inst->Serialize(&CurrentBlock()->cmd_buffer_);
    return result;
  }

  RegOr<type::Type const*> Var(
      absl::Span<RegOr<type::Type const*> const> types) {
    return internal::MakeVariadicCmd<VariantCmd>(types, this);
  }

  RegOr<type::Type const*> Tup(
      absl::Span<RegOr<type::Type const*> const> types) {
    return internal::MakeVariadicCmd<TupleCmd>(types, this);
  }

  // Phi instruction. Takes a span of basic blocks and a span of (registers or)
  // values. As a precondition, the number of blocks must be equal to the number
  // of values. This instruction evaluates to the value `values[i]` if the
  // previous block was `blocks[i]`.
  //
  // In the first overload, the resulting value is assigned to `r`. In the
  // second overload, a register is constructed to represent the value.
  template <typename T>
  void Phi(Reg r, absl::Span<BasicBlock const* const> blocks,
           absl::Span<RegOr<T> const> values) {
    ASSERT(blocks.size() == values.size());

    auto& buf = CurrentBlock()->cmd_buffer_;
    buf.append(PhiCmd::index);
    buf.append(PrimitiveIndex<T>());
    buf.append<uint16_t>(values.size());
    for (auto block : blocks) { buf.append(block); }
    internal::Serialize<uint16_t>(&buf, values);

    buf.append(r);
  }

  template <typename T>
  RegOr<T> Phi(absl::Span<BasicBlock const* const> blocks,
               absl::Span<RegOr<T> const> values) {
    if (values.size() == 1u) { return values[0]; }
    auto r = MakeResult<T>();
    Phi(r, blocks, values);
    return r;
  }

  // Emits a function-call instruction, calling `fn` of type `f` with the given
  // `arguments` and output parameters. If output parameters are not present,
  // the function must return nothing.
  void Call(RegOr<AnyFunc> const& fn, type::Function const* f,
            absl::Span<Results const> arguments, OutParams = {});

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
  void ChooseJump(absl::Span<std::string_view const> names,
                  absl::Span<BasicBlock* const> blocks);

  // Special members function instructions. Calling these typically calls
  // builtin functions (or, in the case of primitive types, do nothing).
  void Init(type::Type const* t, Reg r);
  void Destroy(type::Type const* t, Reg r);
  void Move(type::Type const* t, Reg from, RegOr<Addr> to);
  void Copy(type::Type const* t, Reg from, RegOr<Addr> to);

  // Data structure access commands. For structs and tuples, `Fields` takes an
  // address of the data structure and returns the address of the particular
  // field requested. For variants, `VariantType` computes the location where
  // the type is stored and `VariantValue` accesses the location where the
  // value is stored.
  //
  // TODO: Long-term, variant will probably not be implemented this way.
  // Ideally, something like `*int64 | *int32` will only use 8 bytes because
  // we'll be able to see that the pointers are aligned and we have spare bits.
  // This means variant isn't the lowest level API, but rather some mechanism
  // by which you can overlay types.
  type::Typed<Reg> Field(RegOr<Addr> r, type::Struct const* t, int64_t n);
  type::Typed<Reg> Field(RegOr<Addr> r, type::Tuple const* t, int64_t n);
  Reg VariantType(RegOr<Addr> const& r);
  Reg VariantValue(type::Variant const* v, RegOr<Addr> const& r);

  base::Tagged<Addr, Reg> PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                                  type::Pointer const* t);

  // Type construction commands
  RegOr<type::Function const*> Arrow(
      absl::Span<RegOr<type::Type const*> const> ins,
      absl::Span<RegOr<type::Type const*> const> outs);

  RegOr<type::Type const*> Array(RegOr<ArrayCmd::length_t> len,
                                 RegOr<type::Type const*> data_type);

  Reg OpaqueType(module::BasicModule const* mod);

  Reg Struct(ast::Scope const* scope, absl::Span<StructField const> fields);

  // Print commands
  template <typename T>
  void Print(T r) {
    auto& buf = CurrentBlock()->cmd_buffer_;
    if constexpr (IsRegOr<T>::value) {
      auto inst = std::make_unique<PrintInstruction<typename T::type>>(r);
      inst->Serialize(&CurrentBlock()->cmd_buffer_);
      CurrentBlock()->instructions_.push_back(std::move(inst));
    } else if constexpr (std::is_same_v<T, char const*>) {
      Print(RegOr<std::string_view>(r));
    } else {
      Print(RegOr<T>(r));
    }
  }

  template <typename T,
            typename std::enable_if_t<std::is_same_v<T, EnumVal> or
                                      std::is_same_v<T, FlagsVal>>* = nullptr>
  void Print(RegOr<T> r, type::Type const* t) {
    auto& buf = CurrentBlock()->cmd_buffer_;
    buf.append(PrintCmd::index);
    buf.append(PrintCmd::MakeControlBits<T>(r.is_reg()));
    r.apply([&](auto v) { buf.append(v); });
    buf.append(t);
  }

  // Low-level size/alignment commands
  base::Tagged<core::Alignment, Reg> Align(RegOr<type::Type const*> r);
  base::Tagged<core::Bytes, Reg> Bytes(RegOr<type::Type const*> r);

  base::Tagged<Addr, Reg> Alloca(type::Type const* t);
  base::Tagged<Addr, Reg> TmpAlloca(type::Type const* t);

#if defined(ICARUS_DEBUG)
  void DebugIr() {
    CurrentBlock()->instructions_.push_back(
        std::make_unique<DebugIrInstruction>());
    CurrentBlock()->cmd_buffer_.append(DebugIrCmd::index);
  }
#endif  // ICARUS_DEBUG

  LocalBlockInterpretation MakeLocalBlockInterpretation(ast::ScopeNode const*);

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

  constexpr bool more_stmts_allowed() const {
    return current_.more_stmts_allowed_;
  }
  constexpr void allow_more_stmts() { current_.more_stmts_allowed_ = true; }
  constexpr void disallow_more_stmts() { current_.more_stmts_allowed_ = false; }

  ICARUS_PRIVATE
  friend struct SetCurrent;
  friend struct SetTemporaries;

  struct State {
    internal::BlockGroup* group_ = nullptr;
    BasicBlock* block_;

    // Temporaries need to be destroyed at the end of each statement.
    // This is a pointer to a buffer where temporary allocations can register
    // themselves for deletion.
    std::vector<type::Typed<Reg>> temporaries_to_destroy_;
    bool more_stmts_allowed_ = true;
  } current_;
};

Builder& GetBuilder();

struct SetCurrent : public base::UseWithScope {
  SetCurrent(internal::BlockGroup* fn, Builder* builder = nullptr);
  ~SetCurrent();

 private:
  Builder* builder_;
  internal::BlockGroup* old_group_;
  BasicBlock* old_block_;
};

struct SetTemporaries : public base::UseWithScope {
  SetTemporaries(Builder& bldr) : bldr_(bldr) {
    old_temporaries_ = std::exchange(bldr_.current_.temporaries_to_destroy_,
                                     std::vector<type::Typed<Reg>>{});
    old_more_stmts_allowed_ =
        std::exchange(bldr_.current_.more_stmts_allowed_, true);
  }
  ~SetTemporaries() {}

 private:
  std::vector<type::Typed<Reg>> old_temporaries_;
  bool old_more_stmts_allowed_;
  Builder& bldr_;
};

template <typename ToType, typename FromType>
RegOr<ToType> Cast(RegOr<FromType> r) {
  if (r.is_reg()) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append(CastCmd::index);
    blk.cmd_buffer_.append(PrimitiveIndex<ToType>());
    blk.cmd_buffer_.append(PrimitiveIndex<FromType>());
    blk.cmd_buffer_.append(r.reg());
    Reg result = MakeResult<ToType>();
    blk.cmd_buffer_.append(result);
    return result;
  } else {
    return ToType(r.value());
  }
}

template <typename ToType>
RegOr<ToType> CastTo(type::Type const* from_type, ir::Results const& r) {
  if (from_type == type::Get<ToType>()) { return r.get<ToType>(0); }
  if (from_type == type::Int8) {
    return Cast<ToType, int8_t>(r.get<int8_t>(0));
  } else if (from_type == type::Nat8) {
    return Cast<ToType, uint8_t>(r.get<uint8_t>(0));
  } else if (from_type == type::Int16) {
    return Cast<ToType, int16_t>(r.get<int16_t>(0));
  } else if (from_type == type::Nat16) {
    return Cast<ToType, uint16_t>(r.get<uint16_t>(0));
  } else if (from_type == type::Int32) {
    return Cast<ToType, int32_t>(r.get<int32_t>(0));
  } else if (from_type == type::Nat32) {
    return Cast<ToType, uint32_t>(r.get<uint32_t>(0));
  } else if (from_type == type::Int64) {
    return Cast<ToType, int64_t>(r.get<int64_t>(0));
  } else if (from_type == type::Nat64) {
    return Cast<ToType, uint64_t>(r.get<uint64_t>(0));
  } else if (from_type == type::Float32) {
    return Cast<ToType, float>(r.get<float>(0));
  } else if (from_type == type::Float64) {
    return Cast<ToType, double>(r.get<double>(0));
  } else {
    UNREACHABLE();
  }
}

template <typename T>
base::Tagged<T, Reg> Load(RegOr<Addr> addr) {
  auto& blk   = *GetBuilder().CurrentBlock();
  auto inst   = std::make_unique<LoadInstruction<T>>(addr);
  auto result = inst->result = GetBuilder().CurrentGroup()->Reserve(nullptr);
  inst->Serialize(&blk.cmd_buffer_);
  blk.instructions_.push_back(std::move(inst));
  return result;
}

inline Reg Load(RegOr<Addr> r, type::Type const* t) {
  using base::stringify;
  DEBUG_LOG("Load")("Calling Load(", stringify(r), ", ", t->to_string(), ")");
  if (t->is<type::Function>()) { return Load<AnyFunc>(r); }
  return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                          uint16_t, uint32_t, uint64_t, float, double,
                          type::Type const*, EnumVal, FlagsVal, Addr,
                          std::string_view, AnyFunc>(t, [&](auto tag) -> Reg {
    using T = typename decltype(tag)::type;
    return Load<T>(r);
  });
}

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const* type);

template <typename T>
Reg MakeReg(T t) {
  static_assert(not std::is_same_v<T, Reg>);
  if constexpr (ir::IsRegOr<T>::value) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append(RegisterCmd::index);
    blk.cmd_buffer_.append(
        RegisterCmd::MakeControlBits<typename T::type>(t.is_reg()));
    t.apply([&](auto v) { blk.cmd_buffer_.append(v); });
    Reg result = MakeResult<typename T::type>();
    blk.cmd_buffer_.append(result);
    return result;

  } else {
    return MakeReg(RegOr<T>{t});
  }
}

template <typename T>
void SetRet(uint16_t n, T val) {
  if constexpr (ir::IsRegOr<T>::value) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append(ReturnCmd::index);
    blk.cmd_buffer_.append(
        ReturnCmd::MakeControlBits<typename T::type>(val.is_reg(), false));
    blk.cmd_buffer_.append(n);
    val.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  } else if constexpr (base::IsTaggedV<T>) {
    static_assert(std::is_same_v<typename T::base_type, Reg>);
    SetRet(n, RegOr<typename T::tag_type>(val));
  } else {
    SetRet(n, RegOr<T>(val));
  }
}

inline base::Tagged<Addr, Reg> GetRet(uint16_t n, type::Type const* t) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append(ReturnCmd::index);
  blk.cmd_buffer_.append(ReturnCmd::MakeControlBits<int>(false, true));
  blk.cmd_buffer_.append(n);
  Reg r = MakeResult(t);
  blk.cmd_buffer_.append(r);
  return r;
}

inline void SetRet(uint16_t n, type::Typed<Results> const& r) {
  // if (r.type()->is<type::GenericStruct>()) {
  //   SetRet(n, r->get<AnyFunc>(0));
  // }
  if (r.type()->is<type::Jump>()) {
    // TODO currently this has to be implemented outside type::Apply because
    // that's in type.h which is wrong because it forces weird instantiation
    // order issues (type/type.h can't depend on type/jump.h).
    SetRet(n, r->get<AnyFunc>(0));
  } else {
    ASSERT(r.type()->is_big() == false) << r.type()->to_string();
    type::Apply(r.type(), [&](auto tag) {
      using T = typename decltype(tag)::type;
      SetRet(n, r->get<T>(0));
    });
  }
}

// TODO "Handler" doesn't really make sense in the name for these.
Reg BlockHandler(ir::BlockDef* block_def,
                 absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<Jump const*> const> afters);

Reg ScopeHandler(
    ir::ScopeDef* scope_def, absl::Span<RegOr<Jump const*> const> inits,
    absl::Span<RegOr<AnyFunc> const> dones,
    absl::flat_hash_map<std::string_view, BlockDef*> const& blocks);

template <typename T>
void Store(T r, RegOr<Addr> addr) {
  if constexpr (IsRegOr<T>::value) {
    auto& blk = *GetBuilder().CurrentBlock();
    auto inst = std::make_unique<StoreInstruction<typename T::type>>(r, addr);
    inst->Serialize(&blk.cmd_buffer_);
    blk.instructions_.push_back(std::move(inst));
  } else {
    Store(RegOr<T>(r), addr);
  }
}

Reg Enum(module::BasicModule* mod, absl::Span<std::string_view const> names,
         absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const&
             specified_values);

Reg Flags(module::BasicModule* mod, absl::Span<std::string_view const> names,
          absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const&
              specified_values);

// ----------------------------------------------------------------------------
// Implementation details only below
// ----------------------------------------------------------------------------

namespace internal {
template <typename SizeType, typename T, typename Fn>
void WriteBits(base::untyped_buffer* buf, absl::Span<T const> span,
               Fn&& predicate) {
  ASSERT(span.size() < std::numeric_limits<SizeType>::max());
  buf->append<SizeType>(span.size());

  uint8_t reg_mask = 0;
  for (size_t i = 0; i < span.size(); ++i) {
    if (predicate(span[i])) { reg_mask |= (1 << (7 - (i % 8))); }
    if (i % 8 == 7) {
      buf->append(reg_mask);
      reg_mask = 0;
    }
  }
  if (span.size() % 8 != 0) { buf->append(reg_mask); }
}

template <typename SizeType, typename T>
void Serialize(base::untyped_buffer* buf, absl::Span<RegOr<T> const> span) {
  WriteBits<SizeType, RegOr<T>>(buf, span,
                                [](RegOr<T> const& r) { return r.is_reg(); });

  absl::c_for_each(
      span, [&](RegOr<T> x) { x.apply([&](auto v) { buf->append(v); }); });
}

template <typename T>
auto PrepareCmdArg(T&& arg) {
  using type = std::decay_t<T>;
  if constexpr (base::IsTaggedV<type>) {
    static_assert(std::is_same_v<typename type::base_type, Reg>);
    return RegOr<typename type::tag_type>(std::forward<T>(arg));
  } else if constexpr (IsRegOrV<type>) {
    return std::forward<T>(arg);
  } else {
    return RegOr<type>(std::forward<T>(arg));
  }
}

template <typename CmdType, typename T>
auto MakeBinaryCmd(RegOr<T> lhs, RegOr<T> rhs, Builder* bldr) {
  using fn_type     = typename CmdType::fn_type;
  using result_type = decltype(fn_type{}(lhs.value(), rhs.value()));
  if constexpr (CmdType::template IsSupported<T>()) {
    if (not lhs.is_reg() and not rhs.is_reg()) {
      return RegOr<result_type>{fn_type{}(lhs.value(), rhs.value())};
    }
  }

  auto& buf = bldr->CurrentBlock()->cmd_buffer_;
  buf.append(CmdType::index);
  buf.append(CmdType::template MakeControlBits<T>(lhs.is_reg(), rhs.is_reg()));

  lhs.apply([&](auto v) { buf.append(v); });
  rhs.apply([&](auto v) { buf.append(v); });

  Reg result = MakeResult<T>();
  buf.append(result);
  return RegOr<result_type>{result};
}

template <typename CmdType>
RegOr<typename CmdType::type> MakeVariadicCmd(
    absl::Span<RegOr<typename CmdType::type> const> vals, Builder* bldr) {
  auto& buf = bldr->CurrentBlock()->cmd_buffer_;
  using T   = typename CmdType::type;
  {
    std::vector<T> vs;
    vs.reserve(vals.size());
    if (absl::c_all_of(vals, [&](RegOr<T> t) {
          if (t.is_reg()) { return false; }
          vs.push_back(t.value());
          return true;
        })) {
      return CmdType::fn_ptr(vs);
    }
  }

  buf.append(CmdType::index);
  Serialize<uint16_t>(&buf, vals);

  Reg result = MakeResult<T>();
  buf.append(result);
  return RegOr<T>{result};
}

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_BUILDER_H
