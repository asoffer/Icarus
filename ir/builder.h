#ifndef ICARUS_IR_BUILDER_H
#define ICARUS_IR_BUILDER_H

#include "base/debug.h"
#include "base/scope.h"
#include "base/tag.h"
#include "ir/addr.h"
#include "ir/basic_block.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jumps.h"
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
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "type/typed_value.h"
#include "type/util.h"

namespace ir {
namespace internal {
struct BlockGroup;
}  // namespace internal

struct Builder {
  BasicBlock* AddBlock();

  internal::BlockGroup*& CurrentGroup() { return current_.group_; }
  BasicBlock*& CurrentBlock() { return current_.block_; }

  base::Tagged<Addr, Reg> Alloca(type::Type const* t);
  base::Tagged<Addr, Reg> TmpAlloca(type::Type const* t);

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
  friend struct SetCurrentFunc;
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

struct SetCurrentFunc : public base::UseWithScope {
  SetCurrentFunc(internal::BlockGroup* fn);
  ~SetCurrentFunc();

 private:
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

void Call(RegOr<AnyFunc> const &fn, type::Function const *f,
          absl::Span<Results const> arguments);
void Call(RegOr<AnyFunc> const &fn, type::Function const *f,
          absl::Span<Results const> arguments, OutParams);

template <typename ToType, typename FromType>
RegOr<ToType> Cast(RegOr<FromType> r) {
  if (r.is_reg()) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append_index<CastCmd>();
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

inline void UncondJump(BasicBlock const* block) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<JumpCmd>();
  blk.cmd_buffer_.append(JumpCmd::Kind::kUncond);
  blk.cmd_buffer_.append(block);
}

inline void ReturnJump() {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<JumpCmd>();
  blk.cmd_buffer_.append(JumpCmd::Kind::kRet);
  // This extra block index is so that when inlined, we don't have to worry
  // about iterator invalidation, as a return becomes an unconditional jump
  // needing extra space.
  blk.cmd_buffer_.append(ReturnBlock());
}

inline void CondJump(RegOr<bool> cond, BasicBlock const* true_block,
                     BasicBlock const* false_block) {
  auto& blk = *GetBuilder().CurrentBlock();
  if (cond.is_reg()) {
    blk.cmd_buffer_.append_index<JumpCmd>();
    blk.cmd_buffer_.append(JumpCmd::Kind::kCond);
    blk.cmd_buffer_.append(cond.reg());
    blk.cmd_buffer_.append(false_block);
    blk.cmd_buffer_.append(true_block);
  } else {
    UncondJump(cond.value() ? true_block : false_block);
  }
}

inline void ChooseJump(absl::Span<std::string_view const> names,
                       absl::Span<BasicBlock* const> blocks) {
  ASSERT(names.size() == blocks.size());
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<JumpCmd>();
  blk.cmd_buffer_.append(JumpCmd::Kind::kChoose);
  blk.cmd_buffer_.append<uint16_t>(names.size());
  for (std::string_view name : names) { blk.cmd_buffer_.append(name); }
  for (BasicBlock* block : blocks) { blk.cmd_buffer_.append(block); }
}

template <typename T>
base::Tagged<T, Reg> Load(RegOr<Addr> addr) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<LoadCmd>();
  blk.cmd_buffer_.append(LoadCmd::MakeControlBits<T>(addr.is_reg()));
  addr.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  base::Tagged<T, Reg> result = MakeResult<T>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("load")(blk.cmd_buffer_.to_string());
  return result;
}

inline Reg Load(RegOr<Addr> r, type::Type const* t) {
  if (t->is<type::Function>()) { return Load<AnyFunc>(r); }
  return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                          uint16_t, uint32_t, uint64_t, float, double,
                          type::Type const*, EnumVal, FlagsVal, Addr,
                          std::string_view, AnyFunc>(t, [&](auto tag) -> Reg {
    using T = typename decltype(tag)::type;
    return Load<T>(r);
  });
}

base::Tagged<core::Alignment, Reg> Align(RegOr<type::Type const*> r);
base::Tagged<core::Bytes, Reg> Bytes(RegOr<type::Type const*> r);

void Init(type::Type const* t, Reg r);
void Destroy(type::Type const* t, Reg r);
void Move(type::Type const* t, Reg from, RegOr<Addr> to);
void Copy(type::Type const* t, Reg from, RegOr<Addr> to);

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const* type);

base::Tagged<Addr, Reg> PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                                type::Pointer const* t);
type::Typed<Reg> Field(RegOr<Addr> r, type::Struct const* t, int64_t n);
type::Typed<Reg> Field(RegOr<Addr> r, type::Tuple const* t, int64_t n);

Reg VariantType(RegOr<Addr> const& r);
Reg VariantValue(type::Variant const* v, RegOr<Addr> const& r);

#if defined(ICARUS_DEBUG)
inline void DebugIr() {}
#endif  // ICARUS_DEBUG

namespace internal {
template <typename SizeType, typename T, typename Fn>
void WriteBits(CmdBuffer* buf, absl::Span<T const> span, Fn&& predicate) {
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
void Serialize(CmdBuffer* buf, absl::Span<RegOr<T> const> span) {
  WriteBits<SizeType, RegOr<T>>(buf, span,
                                [](RegOr<T> const& r) { return r.is_reg(); });

  absl::c_for_each(
      span, [&](RegOr<T> x) { x.apply([&](auto v) { buf->append(v); }); });
}
}  // namespace internal

template <typename T>
RegOr<T> Phi(Reg r, absl::Span<BasicBlock const *const> blocks,
             absl::Span<RegOr<T> const> values) {
  ASSERT(blocks.size() == values.size());
  if (values.size() == 1u) { return values[0]; }

  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<PhiCmd>();
  blk.cmd_buffer_.append(PrimitiveIndex<T>());
  blk.cmd_buffer_.append<uint16_t>(values.size());
  for (auto block : blocks) { blk.cmd_buffer_.append(block); }
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, values);

  Reg result = MakeResult<T>();
  blk.cmd_buffer_.append(result);
  return result;
}

template <typename T>
RegOr<T> Phi(absl::Span<BasicBlock const *const> blocks,
             absl::Span<RegOr<T> const> values) {
  return Phi(MakeResult<T>(), blocks, values);
}

inline Results Phi(type::Type const *type,
                   absl::flat_hash_map<BasicBlock *, Results> const &values) {
  if (values.size() == 1) { return values.begin()->second; }
  return type::Apply(type, [&](auto tag) {
    using T = typename decltype(tag)::type;
    std::vector<RegOr<T>> vals;
    vals.reserve(values.size());
    std::vector<BasicBlock const *> blocks;
    blocks.reserve(values.size());
    for (auto const &[key, val] : values) {
      blocks.push_back(key);
      vals.push_back(val.template get<T>(0));
    }
    return Results{Phi<T>(blocks, vals)};
  });
}

template <typename T>
void Print(T r) {
  auto& blk = *GetBuilder().CurrentBlock();
  if constexpr (ir::IsRegOr<T>::value) {
    blk.cmd_buffer_.append_index<PrintCmd>();
    blk.cmd_buffer_.append(
        PrintCmd::MakeControlBits<typename T::type>(r.is_reg()));
    r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  } else {
    Print(RegOr<T>(r));
  }
}

template <typename T,
          typename std::enable_if_t<std::is_same_v<T, EnumVal> or
                                    std::is_same_v<T, FlagsVal>>* = nullptr>
void Print(RegOr<T> r, type::Type const* t) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<PrintCmd>();
  blk.cmd_buffer_.append(PrintCmd::MakeControlBits<T>(r.is_reg()));
  r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  blk.cmd_buffer_.append(t);
}

template <typename T>
Reg MakeReg(T t) {
  static_assert(not std::is_same_v<T, Reg>);
  if constexpr (ir::IsRegOr<T>::value) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append_index<RegisterCmd>();
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
    blk.cmd_buffer_.append_index<ReturnCmd>();
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

inline void SetRet(uint16_t n, type::Typed<Results> const& r) {
  // if (r.type()->is<type::GenericStruct>()) {
  //   SetRet(n, r->get<AnyFunc>(0));
  if (r.type()->is<type::Jump>()) {
    // TODO currently this has to be implemented outside type::Apply because
    // that's in type.h which is wrong because it forces weird instantiation
    // order issues (type/type.h can't depend on type/jump.h).
    SetRet(n, r->get<AnyFunc>(0));
  } else {
    type::Apply(r.type(), [&](auto tag) {
      using T = typename decltype(tag)::type;
      // if constexpr (std::is_same_v<T, type::Struct const*>) {
      //   auto* t = GetBuilder().CurrentGroup()->type_->output[n];
      //   // TODO guaranteed move-elision
      //   visitor::EmitIr visitor;
      //   t->EmitMoveAssign(&visitor, t, r.get(), GetRet(n, t), ctx);
      //   visitor.CompleteDeferredBodies();
      // } else {
      SetRet(n, r->get<T>(0));
      // }
    });
  }
}

inline base::Tagged<Addr, Reg> GetRet(uint16_t n, type::Type const* t) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append(ReturnCmd::MakeControlBits<int>(false, true));
  blk.cmd_buffer_.append(n);
  Reg r = MakeResult(t);
  blk.cmd_buffer_.append(r);
  return r;
}

// TODO "Handler" doesn't really make sense in the name for these.
Reg BlockHandler(compiler::Compiler* compiler,
                 absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<JumpHandler const*> const> afters);

Reg ScopeHandler(
    compiler::Compiler* compiler,
    absl::Span<RegOr<JumpHandler const*> const> inits,
    absl::Span<RegOr<AnyFunc> const> dones,
    absl::flat_hash_map<std::string_view, BlockDef*> const& blocks);

template <typename T>
void Store(T r, RegOr<Addr> addr) {
  if constexpr (IsRegOr<T>::value) {
    auto& blk = *GetBuilder().CurrentBlock();
    blk.cmd_buffer_.append_index<StoreCmd>();
    blk.cmd_buffer_.append(
        StoreCmd::MakeControlBits<typename T::type>(r.is_reg(), addr.is_reg()));
    r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
    addr.apply([&](auto v) {
      DEBUG_LOG("store")(v);
      blk.cmd_buffer_.append(v);
    });
  } else {
    Store(RegOr<T>(r), addr);
  }
}

RegOr<type::Function const*> Arrow(
    absl::Span<RegOr<type::Type const*> const> ins,
    absl::Span<RegOr<type::Type const*> const> outs);

RegOr<type::Type const*> Array(RegOr<ArrayCmd::length_t> len,
                               RegOr<type::Type const*> data_type);

Reg OpaqueType(module::BasicModule const* mod);

namespace internal {

template <typename CmdType>
struct UnaryHandler {
  template <typename... Args,
            typename std::enable_if_t<not std::conjunction_v<
                std::is_same<Args, RegOr<UnwrapTypeT<Args>>>...>>* = nullptr>
  auto operator()(Args... args) const {
    return operator()(RegOr<UnwrapTypeT<Args>>(std::forward<Args>(args))...);
  }

  template <typename T>
  auto operator()(RegOr<T> operand) const {
    auto& blk         = *GetBuilder().CurrentBlock();
    using fn_type     = typename CmdType::fn_type;
    using result_type = decltype(fn_type{}(operand.value()));
    if constexpr (CmdType::template IsSupported<T>()) {
      if (not operand.is_reg()) {
        return RegOr<result_type>{fn_type{}(operand.value())};
      }
    }

    blk.cmd_buffer_.append_index<CmdType>();
    blk.cmd_buffer_.append(
        CmdType::template MakeControlBits<T>(operand.is_reg()));

    operand.apply([&](auto v) { blk.cmd_buffer_.append(v); });

    Reg result = MakeResult<T>();
    blk.cmd_buffer_.append(result);
    return RegOr<result_type>{result};
  }
};

template <typename CmdType>
struct BinaryHandler {
  template <typename... Args,
            typename std::enable_if_t<not std::conjunction_v<
                std::is_same<Args, RegOr<UnwrapTypeT<Args>>>...>>* = nullptr>
  auto operator()(Args... args) const {
    return operator()(RegOr<UnwrapTypeT<Args>>(std::forward<Args>(args))...);
  }

  template <typename T>
  auto operator()(RegOr<T> lhs, RegOr<T> rhs) const {
    auto& blk         = *GetBuilder().CurrentBlock();
    using fn_type     = typename CmdType::fn_type;
    using result_type = decltype(fn_type{}(lhs.value(), rhs.value()));
    if constexpr (CmdType::template IsSupported<T>()) {
      if (not lhs.is_reg() and not rhs.is_reg()) {
        return RegOr<result_type>{fn_type{}(lhs.value(), rhs.value())};
      }
    }

    blk.cmd_buffer_.append_index<CmdType>();
    blk.cmd_buffer_.append(
        CmdType::template MakeControlBits<T>(lhs.is_reg(), rhs.is_reg()));

    lhs.apply([&](auto v) { blk.cmd_buffer_.append(v); });
    rhs.apply([&](auto v) { blk.cmd_buffer_.append(v); });

    Reg result = MakeResult<T>();
    blk.cmd_buffer_.append(result);
    return RegOr<result_type>{result};
  }
};

template <typename CmdType>
RegOr<typename CmdType::type> MakeVariadicImpl(
    absl::Span<RegOr<typename CmdType::type> const> vals) {
  using T = typename CmdType::type;
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

  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<CmdType>();
  Serialize<uint16_t>(&blk.cmd_buffer_, vals);

  Reg result = MakeResult<T>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("variadic")(blk.cmd_buffer_.to_string());
  return RegOr<T>{result};
}
}  // namespace internal

constexpr auto Add      = internal::BinaryHandler<AddCmd>{};
constexpr auto Sub      = internal::BinaryHandler<SubCmd>{};
constexpr auto Mul      = internal::BinaryHandler<MulCmd>{};
constexpr auto Div      = internal::BinaryHandler<DivCmd>{};
constexpr auto Mod      = internal::BinaryHandler<ModCmd>{};
constexpr auto Lt       = internal::BinaryHandler<LtCmd>{};
constexpr auto Le       = internal::BinaryHandler<LeCmd>{};
constexpr auto Eq       = internal::BinaryHandler<EqCmd>{};
constexpr auto Ne       = internal::BinaryHandler<NeCmd>{};
constexpr auto Ge       = internal::BinaryHandler<GeCmd>{};
constexpr auto Gt       = internal::BinaryHandler<GtCmd>{};
constexpr auto Neg      = internal::UnaryHandler<NegCmd>{};
constexpr auto Not      = internal::UnaryHandler<NotCmd>{};
constexpr auto XorFlags = internal::BinaryHandler<XorFlagsCmd>{};
constexpr auto AndFlags = internal::BinaryHandler<AndFlagsCmd>{};
constexpr auto OrFlags  = internal::BinaryHandler<OrFlagsCmd>{};

inline RegOr<type::Type const*> Var(
    absl::Span<RegOr<type::Type const*> const> types) {
  return internal::MakeVariadicImpl<VariantCmd>(types);
}

inline RegOr<type::Type const*> Tup(
    absl::Span<RegOr<type::Type const*> const> types) {
  return internal::MakeVariadicImpl<TupleCmd>(types);
}

Reg Enum(module::BasicModule* mod, absl::Span<std::string_view const> names,
         absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const&
             specified_values);

Reg Flags(module::BasicModule* mod, absl::Span<std::string_view const> names,
          absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const&
              specified_values);

// TODO handle initial values.
Reg Struct(
    ast::Scope const* scope, module::BasicModule* mod,
    std::vector<std::tuple<std::string_view, RegOr<type::Type const*>>> fields);

constexpr inline auto Ptr    = internal::UnaryHandler<PtrCmd>{};
constexpr inline auto BufPtr = internal::UnaryHandler<BufPtrCmd>{};

}  // namespace ir

#endif  // ICARUS_IR_BUILDER_H
