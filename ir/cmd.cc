#include "ir/cmd.h"

#include <cmath>
#include <iostream>
#include <vector>

#include "ast/struct_literal.h"
#include "base/bag.h"
#include "core/arch.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "type/generic_struct.h"
#include "type/typed_value.h"

namespace ir {
thread_local BlockIndex BasicBlock::Current;

void Move(type::Type const *t, Reg from, RegisterOr<Addr> to) {
  auto &cmd     = MakeCmd(nullptr, Op::Move);
  cmd.special2_ = {t, from, to};
}

void Copy(type::Type const *t, Reg from, RegisterOr<Addr> to) {
  auto &cmd     = MakeCmd(nullptr, Op::Copy);
  cmd.special2_ = {t, from, to};
}

void Init(type::Type const *t, Reg r) {
  auto &cmd     = MakeCmd(nullptr, Op::Init);
  cmd.special1_ = {t, r};
}

void Destroy(type::Type const *t, Reg r) {
  auto &cmd     = MakeCmd(nullptr, Op::Destroy);
  cmd.special1_ = {t, r};
}

void VerifyType(ast::Node *node, Reg ctx) {
  auto &cmd = MakeCmd(nullptr, Op::VerifyType);
  cmd.ast_  = {node, ctx};
}

Reg CreateContext(Module *mod) {
  auto &cmd = MakeCmd(type::Ctx, Op::CreateContext);
  cmd.mod_  = mod;
  return cmd.result;
}

void AddBoundConstant(Reg ctx, ast::Declaration *decl,
                      RegisterOr<type::Type const *> type) {
  auto &cmd   = MakeCmd(nullptr, Op::AddBoundConstant);
  cmd.add_bc_ = {ctx, decl, type};
}

void DestroyContext(Reg r) {
  auto &cmd = MakeCmd(nullptr, Op::DestroyContext);
  cmd.reg_  = r;
}

Reg EvaluateAsType(ast::Node *node, Reg ctx) {
  auto &cmd = MakeCmd(type::Type_, Op::EvaluateAsType);
  cmd.ast_  = {node, ctx};
  return cmd.result;
}

Cmd &MakeCmd(type::Type const *t, Op op) {
  auto &cmd = ASSERT_NOT_NULL(CompiledFn::Current)
                  ->block(BasicBlock::Current)
                  .cmds_.emplace_back(t, op);
  return cmd;
}

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *t) {
  auto &cmd     = MakeCmd(t, Op::LoadSymbol);
  cmd.load_sym_ = {name, t};
  return type::Typed<Reg>{cmd.result, t};
}

// TODO pass atyped_reg? not sure that's right.
Reg CastPtr(Reg r, type::Pointer const *t) {
  auto &cmd      = MakeCmd(t, Op::CastPtr);
  cmd.typed_reg_ = type::Typed<Reg>(r, t);
  return cmd.result;
}

RegisterOr<int64_t> Bytes(RegisterOr<type::Type const *> r) {
  auto &cmd     = MakeCmd(type::Int64, Op::Bytes);
  cmd.type_arg_ = r;
  return cmd.result;
}

RegisterOr<int64_t> Align(RegisterOr<type::Type const *> r) {
  auto &cmd     = MakeCmd(type::Int64, Op::Align);
  cmd.type_arg_ = r;
  return cmd.result;
}

template <typename T>
static Results CastTo(type::Type const *from, Results const &val) {
  if (val.is_reg(0)) {
    auto *to       = type::Get<T>();
    auto &cmd      = MakeCmd(to, Cmd::OpCode<Cmd::CastTag, T>());
    cmd.typed_reg_ = type::Typed<Reg>(val.get<Reg>(0), from);
    return Results{cmd.result};
  } else {
    return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                            uint16_t, uint32_t, uint64_t, float>(
        from, [&](auto type_holder) {
          using FromType = typename decltype(type_holder)::type;
          return Results{static_cast<T>(val.get<FromType>(0).val_)};
        });
  }
  UNREACHABLE("To ", type::Get<T>(), " from ", from);
}

Results Cast(type::Type const *from, type::Type const *to, Results const &val) {
  if (from == type::NullPtr) { return val; }

  // Note: ir::Cast is called with types associated IR commands and registers.
  // Because arrays are considered big, they would never be stored directly in
  // a register, so we would not see an empty array but rather a pointer to an
  // empty arry.
  if (from == type::Ptr(type::EmptyArray)) { return val; }

  if (to->is<type::Enum>()) {
    ASSERT(from == type::Int32);
    auto x = val.get<int32_t>(0);
    if (x.is_reg_) {
      auto &cmd = MakeCmd(to, Op::CastToEnum);
      cmd.reg_  = x.reg_;
      return Results{cmd.result};
    } else {
      return Results{EnumVal(x.val_)};
    }
  } else if (to->is<type::Flags>()) {
    ASSERT(from == type::Int32);
    auto x = val.get<int32_t>(0);
    if (x.is_reg_) {
      auto &cmd = MakeCmd(to, Op::CastToFlags);
      cmd.reg_  = x.reg_;
      return Results{cmd.result};
    } else {
      return Results{FlagsVal(x.val_)};
    }
  }

  // TODO We only need to include int8_t and uint8_t here for supporting loose
  // casting. If that disappears, we can remove those types.
  return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                          uint32_t, uint64_t, float, double>(
      to, [&](auto type_holder) {
        using ToType = typename decltype(type_holder)::type;
        return CastTo<ToType>(from, val);
      });
}

RegisterOr<bool> Not(RegisterOr<bool> r) {
  if (!r.is_reg_) { return !r.val_; }
  auto &cmd = MakeCmd(type::Bool, Op::NotBool);
  cmd.reg_  = r.reg_;
  CompiledFn::Current->references_[cmd.reg_].insert(cmd.result);
  return cmd.result;
}

RegisterOr<FlagsVal> Not(type::Typed<RegisterOr<FlagsVal>, type::Flags> r) {
  if (!r->is_reg_) { return NotFlags(r->val_, r.type()); }
  auto &cmd      = MakeCmd(r.type(), Op::NotFlags);
  cmd.typed_reg_ = type::Typed<Reg>(r->reg_, r.type());
  return cmd.result;
}

RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r) {
  if (!r.is_reg_) { return type::Ptr(r.val_); }
  auto &cmd = MakeCmd(type::Type_, Op::Ptr);
  cmd.reg_  = r.reg_;
  return cmd.result;
}

RegisterOr<type::Type const *> BufPtr(RegisterOr<type::Type const *> r) {
  if (!r.is_reg_) { return type::BufPtr(r.val_); }
  auto &cmd = MakeCmd(type::Type_, Op::BufPtr);
  cmd.reg_  = r.reg_;
  return cmd.result;
}

RegisterOr<type::Type const *> Arrow(RegisterOr<type::Type const *> v1,
                                     RegisterOr<type::Type const *> v2) {
  if (!v1.is_reg_ && !v2.is_reg_) {
    std::vector<type::Type const *> ins =
        v1.val_->is<type::Tuple>() ? v1.val_->as<type::Tuple>().entries_
                                   : std::vector<type::Type const *>{v1.val_};
    std::vector<type::Type const *> outs =
        v2.val_->is<type::Tuple>() ? v2.val_->as<type::Tuple>().entries_
                                   : std::vector<type::Type const *>{v2.val_};
    return type::Func(std::move(ins), std::move(outs));
  }
  auto &cmd = MakeCmd(type::Type_, Op::Arrow);
  cmd.set<Cmd::ArrowTag, type::Type const *>(v1, v2);
  auto &refs = CompiledFn::Current->references_;
  if (v1.is_reg_) { refs[v1.reg_].insert(cmd.result); }
  if (v2.is_reg_) { refs[v2.reg_].insert(cmd.result); }
  return cmd.result;
}

RegisterOr<type::Type const *> Array(RegisterOr<int64_t> len,
                                     RegisterOr<type::Type const *> data_type) {
  if (!data_type.is_reg_ && !len.is_reg_) {
    return type::Arr(len.val_, data_type.val_);
  }

  auto &cmd  = MakeCmd(type::Type_, Op::Array);
  cmd.array_ = {len, data_type};
  return cmd.result;
}

Reg CreateTuple() { return MakeCmd(type::Type_, Op::CreateTuple).result; }

void AppendToTuple(Reg tup, RegisterOr<type::Type const *> entry) {
  auto &cmd       = MakeCmd(nullptr, Op::AppendToTuple);
  cmd.store_type_ = {tup, entry};
}

Reg FinalizeTuple(Reg r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeTuple);
  cmd.reg_  = r;
  return cmd.result;
}

RegisterOr<type::Type const *> Tup(
    std::vector<RegisterOr<type::Type const *>> const &entries) {
  if (std::all_of(
          entries.begin(), entries.end(),
          [](RegisterOr<type::Type const *> r) { return !r.is_reg_; })) {
    std::vector<type::Type const *> types;
    for (auto const &val : entries) { types.push_back(val.val_); }
    return type::Tup(std::move(types));
  }

  ir::Reg tup = ir::CreateTuple();
  for (auto const &val : entries) { ir::AppendToTuple(tup, val); }
  return ir::FinalizeTuple(tup);
}

Reg CreateVariant() { return MakeCmd(type::Type_, Op::CreateVariant).result; }

void AppendToVariant(Reg var, RegisterOr<type::Type const *> entry) {
  auto &cmd       = MakeCmd(nullptr, Op::AppendToVariant);
  cmd.store_type_ = {var, entry};
}

Reg FinalizeVariant(Reg r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeVariant);
  cmd.reg_  = r;
  return cmd.result;
}

RegisterOr<type::Type const *> Variant(
    std::vector<RegisterOr<type::Type const *>> const &vals) {
  if (std::all_of(
          vals.begin(), vals.end(),
          [](RegisterOr<type::Type const *> const &v) { return !v.is_reg_; })) {
    std::vector<type::Type const *> types;
    types.reserve(vals.size());
    for (auto const &v : vals) { types.push_back(v.val_); }
    return type::Var(std::move(types));
  }
  ir::Reg var = ir::CreateVariant();
  for (auto const &val : vals) { ir::AppendToVariant(var, val); }
  return ir::FinalizeVariant(var);
}

RegisterOr<bool> XorBool(RegisterOr<bool> v1, RegisterOr<bool> v2) {
  if (!v1.is_reg_) { return v1.val_ ? Not(v2) : v2; }
  if (!v2.is_reg_) { return v2.val_ ? Not(v1) : v1; }
  auto &cmd = MakeCmd(type::Bool, Op::XorBool);
  cmd.set<Cmd::XorTag, bool>(v1, v2);
  return cmd.result;
}

type::Typed<Reg> Field(RegisterOr<Addr> r, type::Tuple const *t, size_t n) {
  auto *p    = type::Ptr(t->entries_.at(n));
  auto &cmd  = MakeCmd(p, Op::Field);
  cmd.field_ = {r, t, n};
  return type::Typed<Reg>(cmd.result, p);
}

type::Typed<Reg> Field(RegisterOr<Addr> r, type::Struct const *t, size_t n) {
  auto *p    = type::Ptr(t->fields().at(n).type);
  auto &cmd  = MakeCmd(p, Op::Field);
  cmd.field_ = {r, t, n};
  return type::Typed<Reg>(cmd.result, p);
}

Reg Reserve(type::Type const *t) {
  auto arch   = core::Interpretter();
  auto offset = FwdAlign(CompiledFn::Current->reg_size_, t->alignment(arch));
  CompiledFn::Current->reg_size_ = offset + t->bytes(arch);

  // TODO starts at `n`, where `n` is the number of function arguments.
  ir::Reg r{CompiledFn::Current->compiler_reg_to_offset_.size()};
  CompiledFn::Current->compiler_reg_to_offset_.emplace(r, offset.value());
  ++CompiledFn::Current->num_regs_;
  return r;
}

Cmd::Cmd(type::Type const *t, Op op) : op_code_(op) {
  ASSERT(CompiledFn::Current != nullptr);
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<int32_t>(
          CompiledFn::Current->block(BasicBlock::Current).cmds_.size())};
  if (t == nullptr) {
    result = Reg();
    CompiledFn::Current->references_[result];  // Guarantee it exists.
    CompiledFn::Current->reg_to_cmd_.emplace(result, cmd_index);
    return;
  }

  result = Reserve(t);
  CompiledFn::Current->references_[result];  // Guarantee it exists.
  // TODO for implicitly declared out-params of a Call, map them to the call.
  CompiledFn::Current->reg_to_cmd_.emplace(result, cmd_index);
}

BlockSequence MakeBlockSeq(std::vector<ir::BlockSequence> const &blocks);

Reg CreateBlockSeq() { return MakeCmd(type::Type_, Op::CreateBlockSeq).result; }

void AppendToBlockSeq(Reg block_seq,
                      RegisterOr<ir::BlockSequence> more_block_seq) {
  auto &cmd        = MakeCmd(nullptr, Op::AppendToBlockSeq);
  cmd.store_block_ = {block_seq, more_block_seq};
}

Reg FinalizeBlockSeq(Reg r) {
  auto &cmd = MakeCmd(type::Blk(), Op::FinalizeBlockSeq);
  cmd.reg_  = r;
  return cmd.result;
}

RegisterOr<bool> BlockSeqContains(RegisterOr<BlockSequence> r,
                                  ast::BlockLiteral *lit) {
  if (r.is_reg_) {
    auto &cmd               = MakeCmd(type::Bool, Op::BlockSeqContains);
    cmd.block_seq_contains_ = Cmd::BlockSeqContains{r.reg_, lit};
    return cmd.result;
  }

  return std::any_of(r.val_.seq_->begin(), r.val_.seq_->end(),
                     [lit](ast::BlockLiteral *l) { return lit == l; });
}

Reg CreateStruct(core::Scope const *scope, ast::StructLiteral const *parent) {
  auto &cmd          = MakeCmd(type::Type_, Op::CreateStruct);
  cmd.create_struct_ = {scope, parent};
  return cmd.result;
}

Reg CreateInterface(core::Scope const *scope) {
  auto &cmd  = MakeCmd(type::Type_, Op::CreateInterface);
  cmd.scope_ = scope;
  return cmd.result;
}

Reg ArgumentCache(ast::StructLiteral *sl) {
  auto &cmd = MakeCmd(type::Ptr(type::Type_), Op::ArgumentCache);
  cmd.sl_   = sl;
  return cmd.result;
}

TypedRegister<type::Interface const *> FinalizeStruct(Reg r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeStruct);
  cmd.reg_  = r;
  return cmd.result;
}

Reg FinalizeInterface(Reg r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeInterface);
  cmd.reg_  = r;
  return cmd.result;
}

void DebugIr() { MakeCmd(nullptr, Op::DebugIr); }

Reg VariantType(RegisterOr<Addr> r) {
  auto &cmd     = MakeCmd(Ptr(type::Type_), Op::VariantType);
  cmd.addr_arg_ = r;
  return cmd.result;
}

Reg VariantValue(type::Type const *t, RegisterOr<Addr> r) {
  auto &cmd     = MakeCmd(type::Ptr(t), Op::VariantValue);
  cmd.addr_arg_ = r;
  return cmd.result;
}

void CreateStructField(Reg struct_type, RegisterOr<type::Type const *> type) {
  auto &cmd                = MakeCmd(nullptr, Op::CreateStructField);
  cmd.create_struct_field_ = {struct_type, std::move(type)};
}

void SetStructFieldName(Reg struct_type, std::string_view field_name) {
  auto &cmd                  = MakeCmd(nullptr, Op::SetStructFieldName);
  cmd.set_struct_field_name_ = {struct_type, field_name};
}

void AddHashtagToField(Reg struct_type, ast::Hashtag hashtag) {
  auto &cmd        = MakeCmd(nullptr, Op::AddHashtagToField);
  cmd.add_hashtag_ = {struct_type, hashtag};
}

void AddHashtagToStruct(Reg struct_type, ast::Hashtag hashtag) {
  auto &cmd        = MakeCmd(nullptr, Op::AddHashtagToStruct);
  cmd.add_hashtag_ = {struct_type, hashtag};
}

TypedRegister<Addr> Alloca(type::Type const *t) {
  auto &cmd = ASSERT_NOT_NULL(CompiledFn::Current)
                  ->block(CompiledFn::Current->entry())
                  .cmds_.emplace_back(type::Ptr(t), Op::Alloca);
  cmd.type_ = t;
  return cmd.result;
}

TypedRegister<Addr> TmpAlloca(type::Type const *t, Context *ctx) {
  auto reg = Alloca(t);
  ctx->temporaries_to_destroy_->emplace_back(reg, t);
  return reg;
}

TypedRegister<Addr> GetRet(size_t n, type::Type const *t) {
  ASSERT(t->is_big() == true);
  auto &cmd    = MakeCmd(type::Ptr(t), Op::GetRet);
  cmd.get_ret_ = n;
  return cmd.result;
}

void SetRet(size_t n, type::Typed<Results> const &r, Context *ctx) {
  if (r.type()->is<type::GenericStruct>()) {
    SetRet(n, r->get<AnyFunc>(0));
  } else {
    type::Apply(r.type(), [&](auto type_holder) {
      using T = typename decltype(type_holder)::type;
      if constexpr (std::is_same_v<T, type::Struct const *>) {
        auto *t = ir::CompiledFn::Current->type_->output[n];
        // TODO guaranteed move-elision
        t->EmitMoveAssign(t, r.get(), GetRet(n, t), ctx);
      } else {
        SetRet(n, r->get<T>(0));
      }
    });
  }
}

TypedRegister<Addr> PtrIncr(RegisterOr<Addr> ptr, RegisterOr<int64_t> inc,
                            type::Pointer const *t) {
  if (!inc.is_reg_ && inc.val_ == 0 &&
      /* TODO get rid of this last condition */ ptr.is_reg_) {
    return TypedRegister<Addr>{ptr.reg_};
  }
  auto &cmd     = MakeCmd(t, Op::PtrIncr);
  cmd.ptr_incr_ = {ptr, t->pointee, inc};
  return cmd.result;
}

RegisterOr<FlagsVal> XorFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ ^ rhs.val_; }
  if (!lhs.is_reg_) {
    if (lhs.val_.value == 0) { return rhs; }
    if (lhs.val_.value == (1ull << type->members_.size()) - 1) {
      return Not(type::Typed<RegisterOr<FlagsVal>, type::Flags>(rhs, type));
    }
  }

  if (!rhs.is_reg_) {
    if (rhs.val_.value == 0) { return rhs; }
    if (rhs.val_.value == (1ull << type->members_.size()) - 1) {
      return Not(type::Typed<RegisterOr<FlagsVal>, type::Flags>(lhs, type));
    }
  }

  auto &cmd = MakeCmd(type, Op::XorFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> OrFlags(type::Flags const *type,
                             RegisterOr<FlagsVal> const &lhs,
                             RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ | rhs.val_; }
  if (!lhs.is_reg_) {
    if (lhs.val_.value == 0) { return rhs; }
    if (lhs.val_.value == (1ull << type->members_.size()) - 1) {
      return FlagsVal{(1ull << type->members_.size()) - 1};
    }
  }

  if (!rhs.is_reg_) {
    if (rhs.val_.value == 0) { return lhs; }
    if (rhs.val_.value == (1ull << type->members_.size()) - 1) {
      return FlagsVal{(1ull << type->members_.size()) - 1};
    }
  }

  auto &cmd = MakeCmd(type, Op::OrFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> AndFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ & rhs.val_; }
  if (!lhs.is_reg_) {
    if (lhs.val_.value == 0) { return FlagsVal{0}; }
    if (lhs.val_.value == (1ull << type->members_.size()) - 1) { return rhs; }
  }

  if (!rhs.is_reg_) {
    if (rhs.val_.value == 0) { return FlagsVal{0}; }
    if (rhs.val_.value == (1ull << type->members_.size()) - 1) { return lhs; }
  }

  auto &cmd = MakeCmd(type, Op::AndFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

Reg Load(RegisterOr<Addr> r, type::Type const *t) {
  if (t->is<type::Function>()) { return Load<AnyFunc>(r, t); }
  return type::Apply(t, [&](auto type_holder) -> Reg {
    using T = typename decltype(type_holder)::type;
    if constexpr (std::is_same_v<T, ir::Addr> ||
                  std::is_same_v<T, ir::EnumVal> ||
                  std::is_same_v<T, ir::FlagsVal>) {
      return Load<T>(r, t);
    } else {
      return Load<T>(r);
    }
  });
}

TypedRegister<Addr> Index(type::Pointer const *t, Reg array_ptr,
                          RegisterOr<int64_t> offset) {
  auto *array_type = &t->pointee->as<type::Array>();
  // TODO this works but generates worse ir (both here and in llvm). It's worth
  // figuring out how to do this better. Is this still true without
  // variable-length arrays?
  return PtrIncr(array_ptr, offset, type::Ptr(array_type->data_type));
}

void Call(RegisterOr<AnyFunc> const &f, Arguments arguments) {
  auto &block     = CompiledFn::Current->block(BasicBlock::Current);
  Arguments *args = &block.arguments_.emplace_back(std::move(arguments));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  cmd.call_ = Cmd::Call(f, args, nullptr);
}

void Call(RegisterOr<AnyFunc> const &f, Arguments arguments, OutParams outs) {
  auto &block    = CompiledFn::Current->block(BasicBlock::Current);
  auto *args     = &block.arguments_.emplace_back(std::move(arguments));
  auto *outs_ptr = &block.outs_.emplace_back(std::move(outs));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  cmd.call_ = Cmd::Call(f, args, outs_ptr);
}

Results CallInline(CompiledFn *f, Arguments const &arguments) {
  std::vector<Results> return_vals;
  return_vals.resize(f->type_->output.size());

  // TODO is this used?
  auto &block = CompiledFn::Current->block(BasicBlock::Current);

  // In order to inline the entire function, we have to be careful because
  // reading the blocks in numeric order may not yield registers in an order
  // such that we only see registers depending on previously seen registers.
  // Such an ordering cannot exist because phi-nodes could depend on values
  // determined later in the same block. It is conceivable that phi-nodes are
  // the only exception, but relying on this puts some strange limitations on
  // how we build the IR (cannot jump to another block and then back), so we
  // would rather not enforce that.
  //
  // We do know that block 0 consists entirely of allocas and an unconditional
  // jump to block 1.
  //
  // 1. Map parameter registers to arguments.
  // 2. Initialize block 0.
  // 3. Ignoring phi-nodes, iterate over commands in blocks, skipping to the
  //    next one whenever you get stuck.
  // 4. After all such blocks are handled, go back with a second pass over
  //    phi-nodes.
  absl::flat_hash_map<ir::Reg, ir::Results> reg_relocs;

  // 1. Map parameter registers to arguments.
  for (size_t i = 0; i < f->type_->input.size(); ++i) {
    reg_relocs.emplace(Reg::Arg(i), arguments.results_.GetResult(i));
  }

  // 2. Initialize block 0.
  for (auto const &cmd : f->block(f->entry()).cmds_) {
    switch (cmd.op_code_) {
      case Op::Alloca:
        reg_relocs.emplace(cmd.result, ir::Results{Alloca(cmd.type_)});
        continue;
      case Op::UncondJump: continue;
      default: UNREACHABLE();
    }
  }

  // 3. Ignoring phi-nodes, iterate over commands in blocks, skipping to the
  //    next one whenever you get stuck.
  base::bag<std::pair<BasicBlock *, size_t>> blocks;
  for (size_t i = 1; i < f->blocks_.size(); ++i) {
    blocks.emplace(&f->blocks_.at(i), 0);
  }

  while (!blocks.empty()) {
    auto iter           = blocks.begin();
    auto [block, index] = *iter;
    for (; index < block->cmds_.size(); ++index) {
      auto const &cmd = block->cmds_.at(index);
      switch (cmd.op_code_) {
#define CASE(op_code, op_fn, args, type)                                       \
  case op_code: {                                                              \
    auto iter0 = reg_relocs.find(args[0].reg_);                                \
    if (iter0 == reg_relocs.end()) { goto next_block; }                        \
    auto iter1 = reg_relocs.find(args[1].reg_);                                \
    if (iter1 == reg_relocs.end()) { goto next_block; }                        \
    reg_relocs.emplace(cmd.result, op_fn(iter0->second.get<type>(0),           \
                                         iter1->second.get<type>(0)));         \
  } break
        CASE(Op::AddNat8, Add, cmd.u8_args_.args_, uint8_t);
        CASE(Op::AddNat16, Add, cmd.u16_args_.args_, uint16_t);
        CASE(Op::AddNat32, Add, cmd.u32_args_.args_, uint32_t);
        CASE(Op::AddNat64, Add, cmd.u64_args_.args_, uint64_t);
        CASE(Op::AddInt8, Add, cmd.i8_args_.args_, int8_t);
        CASE(Op::AddInt16, Add, cmd.i16_args_.args_, int16_t);
        CASE(Op::AddInt32, Add, cmd.i32_args_.args_, int32_t);
        CASE(Op::AddInt64, Add, cmd.i64_args_.args_, int64_t);

        CASE(Op::SubNat8, Sub, cmd.u8_args_.args_, uint8_t);
        CASE(Op::SubNat16, Sub, cmd.u16_args_.args_, uint16_t);
        CASE(Op::SubNat32, Sub, cmd.u32_args_.args_, uint32_t);
        CASE(Op::SubNat64, Sub, cmd.u64_args_.args_, uint64_t);
        CASE(Op::SubInt8, Sub, cmd.i8_args_.args_, int8_t);
        CASE(Op::SubInt16, Sub, cmd.i16_args_.args_, int16_t);
        CASE(Op::SubInt32, Sub, cmd.i32_args_.args_, int32_t);
        CASE(Op::SubInt64, Sub, cmd.i64_args_.args_, int64_t);

        CASE(Op::MulNat8, Mul, cmd.u8_args_.args_, uint8_t);
        CASE(Op::MulNat16, Mul, cmd.u16_args_.args_, uint16_t);
        CASE(Op::MulNat32, Mul, cmd.u32_args_.args_, uint32_t);
        CASE(Op::MulNat64, Mul, cmd.u64_args_.args_, uint64_t);
        CASE(Op::MulInt8, Mul, cmd.i8_args_.args_, int8_t);
        CASE(Op::MulInt16, Mul, cmd.i16_args_.args_, int16_t);
        CASE(Op::MulInt32, Mul, cmd.i32_args_.args_, int32_t);
        CASE(Op::MulInt64, Mul, cmd.i64_args_.args_, int64_t);

        CASE(Op::DivNat8, Div, cmd.u8_args_.args_, uint8_t);
        CASE(Op::DivNat16, Div, cmd.u16_args_.args_, uint16_t);
        CASE(Op::DivNat32, Div, cmd.u32_args_.args_, uint32_t);
        CASE(Op::DivNat64, Div, cmd.u64_args_.args_, uint64_t);
        CASE(Op::DivInt8, Div, cmd.i8_args_.args_, int8_t);
        CASE(Op::DivInt16, Div, cmd.i16_args_.args_, int16_t);
        CASE(Op::DivInt32, Div, cmd.i32_args_.args_, int32_t);
        CASE(Op::DivInt64, Div, cmd.i64_args_.args_, int64_t);

        CASE(Op::ModNat8, Mod, cmd.u8_args_.args_, uint8_t);
        CASE(Op::ModNat16, Mod, cmd.u16_args_.args_, uint16_t);
        CASE(Op::ModNat32, Mod, cmd.u32_args_.args_, uint32_t);
        CASE(Op::ModNat64, Mod, cmd.u64_args_.args_, uint64_t);
        CASE(Op::ModInt8, Mod, cmd.i8_args_.args_, int8_t);
        CASE(Op::ModInt16, Mod, cmd.i16_args_.args_, int16_t);
        CASE(Op::ModInt32, Mod, cmd.i32_args_.args_, int32_t);
        CASE(Op::ModInt64, Mod, cmd.i64_args_.args_, int64_t);

#undef CASE
        case Op::SetRetInt64: {
          if (cmd.set_ret_i64_.val_.is_reg_) {
            auto iter = reg_relocs.find(cmd.set_ret_i64_.val_.reg_);
            if (iter == reg_relocs.end()) { goto next_block; }
            return_vals.at(cmd.set_ret_i64_.ret_num_) = iter->second;
          } else {
            return_vals.at(cmd.set_ret_i64_.ret_num_) =
                ir::Results{cmd.set_ret_i64_.val_.val_};
          }
        } break;
        default:;  // NOT_YET(static_cast<int>(cmd.op_code_));
      }
      continue;

    next_block:
      blocks.emplace(block, index);
      break;
    }
    blocks.erase(iter);
  }

  Results results;
  for (auto const &r : return_vals) { results.append(r); }
  return results;
}

void CondJump(RegisterOr<bool> cond, BlockIndex true_block,
              BlockIndex false_block) {
  if (!cond.is_reg_) {
    UncondJump(cond.val_ ? true_block : false_block);
    return;
  }
  auto &cmd      = MakeCmd(nullptr, Op::CondJump);
  cmd.cond_jump_ = Cmd::CondJump{cond.reg_, {false_block, true_block}};
}

void UncondJump(BlockIndex block) {
  MakeCmd(nullptr, Op::UncondJump).block_ = block;
}

void ReturnJump() { auto &cmd = MakeCmd(nullptr, Op::ReturnJump); }

TypedRegister<type::Type const *> NewOpaqueType(::Module *mod) {
  auto &cmd = MakeCmd(type::Type_, Op::NewOpaqueType);
  cmd.mod_  = mod;
  return cmd.result;
}

void BlockSeqJump(RegisterOr<BlockSequence> bseq,
                  absl::flat_hash_map<ast::BlockLiteral const *,
                                      ir::BlockIndex> const *jump_table) {
  auto &cmd           = MakeCmd(nullptr, Op::BlockSeqJump);
  cmd.block_seq_jump_ = Cmd::BlockSeqJump{bseq, jump_table};
}

template <typename T>
static std::ostream &operator<<(std::ostream &os,
                                std::array<RegisterOr<T>, 2> r) {
  return os << r[0] << " " << r[1];
}

char const *OpCodeStr(Op op) {
  switch (op) {
#define OP_MACRO(op, ...)                                                      \
  case Op::op:                                                                 \
    return #op;
#include "ir/op.xmacro.h"
#undef OP_MACRO
  }
  __builtin_unreachable();
}

template <typename T>
static auto Stringify(T &&val) {
  if constexpr (std::is_same_v<std::decay_t<T>, type::Type const *>) {
    return val->to_string();
  } else if constexpr (std::is_same_v<std::decay_t<T>,
                                      RegisterOr<type::Type const *>>) {
    std::stringstream ss;
    if (val.is_reg_) {
      ss << val.reg_;
    } else {
      if (val.val_ == nullptr) {
        ss << "0x0";
      } else {
        ss << val.val_->to_string();
      }
    }
    return ss.str();
  } else {
    return val;
  }
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::Store<T> const &s) {
  return os << s.addr_ << " <- " << Stringify(s.val_);
}

template <typename T>
std::ostream &operator<<(std::ostream &os, Cmd::Args<T> const &a) {
  return os << Stringify(a.args_[0]) << " " << Stringify(a.args_[1]);
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::SetRet<T> const &s) {
  return os << s.ret_num_ << " " << Stringify(s.val_);
}

static std::ostream &operator<<(std::ostream &os, Cmd::PrintEnum const &p) {
  return os << p.arg_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::PrintFlags const &p) {
  return os << p.arg_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::BlockSeqJump const &b) {
  return os << b.bseq_;
}

static std::ostream &operator<<(std::ostream &os,
                                Cmd::BlockSeqContains const &b) {
  return os << b.lit_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::PtrIncr const &p) {
  return os << p.ptr_ << " " << p.pointee_type_ << " " << p.incr_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::Array const &a) {
  return os << a.type_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::Field const &f) {
  return os << f.ptr_ << " " << f.type_->to_string() << " " << f.num_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::CondJump const &j) {
  return os << j.cond_ << " false -> " << j.blocks_[0] << " true -> "
            << j.blocks_[1];
}

static std::ostream &operator<<(std::ostream &os, Cmd::LoadSymbol const &ls) {
  return os << ls.name_ << ": " << ASSERT_NOT_NULL(ls.type_)->to_string();
}

static std::ostream &operator<<(std::ostream &os, Cmd::Call const &call) {
  if (call.fn_.is_reg_) {
    os << call.fn_.reg_;
  } else if (call.fn_.val_.is_fn()) {
    os << call.fn_.val_.func();
  } else {
    os << "foreign("
       << reinterpret_cast<uintptr_t>(call.fn_.val_.foreign().get()) << ")";
  }
  os << call.arguments_->to_string();

  if (call.outs_) {
    for (size_t i = 0; i < call.outs_->size(); ++i) {
      if (call.outs_->is_loc_[i]) { os << "*"; }
      os << call.outs_->regs_[i];
    }
  }

  return os;
}

static std::ostream &operator<<(std::ostream &os, Cmd::AddEnumerator const &s) {
  return os << s.enum_ << " " << s.name_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::SetEnumerator const &s) {
  return os << s.enum_ << " " << s.val_;
}

std::ostream &operator<<(std::ostream &os, Cmd const &cmd) {
  if (cmd.result != Reg{}) { os << cmd.result << " = "; }
  os << OpCodeStr(cmd.op_code_) << " ";
  switch (cmd.op_code_) {
#define OP_MACRO(op, tag, type, field)                                         \
  case Op::op:                                                                 \
    return os << Stringify(cmd.field);
#include "ir/op.xmacro.h"
#undef OP_MACRO
  }
  UNREACHABLE();
}
}  // namespace ir
