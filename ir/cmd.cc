#include "ir/cmd.h"

#include <cmath>
#include <iostream>
#include <vector>

#include "ast/ast.h"
#include "base/bag.h"
#include "core/arch.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/typed_value.h"

namespace ir {
thread_local BlockIndex BasicBlock::Current;

Reg CreateScopeDef(::Module const *mod, ScopeDef*scope_def) {
  auto &cmd = MakeCmd(type::Scope, Op::CreateScopeDef);
  // TODO you don't need the module.
  cmd.create_scope_def_ = {const_cast<::Module *>(mod), scope_def};
  return cmd.result;
}

Reg CreateBlockDef(ast::BlockLiteral const *parent) {
  auto &cmd      = MakeCmd(type::Block, Op::CreateBlockDef);
  cmd.block_lit_ = parent;
  return cmd.result;
}

void FinishScopeDef() { MakeCmd(type::Block, Op::FinishScopeDef); }

void FinishBlockDef(std::string_view name) {
  auto &cmd          = MakeCmd(nullptr, Op::FinishBlockDef);
  cmd.byte_view_arg_ = name;
}

void AddBlockDefBefore(RegisterOr<AnyFunc> f) {
  auto &cmd   = MakeCmd(nullptr, Op::AddBlockDefBefore);
  cmd.any_fn_ = f;
}

void AddBlockDefAfter(RegisterOr<AnyFunc> f) {
  auto &cmd   = MakeCmd(nullptr, Op::AddBlockDefAfter);
  cmd.any_fn_ = f;
}

void AddScopeDefInit(Reg reg, RegisterOr<AnyFunc> f) {
  auto &cmd               = MakeCmd(nullptr, Op::AddScopeDefInit);
  cmd.add_scope_def_init_ = {reg, f};
}

void AddScopeDefDone(Reg reg, RegisterOr<AnyFunc> f) {
  auto &cmd               = MakeCmd(nullptr, Op::AddScopeDefDone);
  cmd.add_scope_def_done_ = {reg, f};
}

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

void VerifyType(ast::Node const *node, Reg ctx) {
  auto &cmd = MakeCmd(nullptr, Op::VerifyType);
  cmd.ast_  = {node, ctx};
}

Reg CreateContext(Module *mod) {
  auto &cmd = MakeCmd(type::Ctx, Op::CreateContext);
  cmd.mod_  = mod;
  return cmd.result;
}

void AddBoundConstant(Reg ctx, ast::Declaration const *decl,
                      RegisterOr<type::Type const *> type) {
  auto &cmd   = MakeCmd(nullptr, Op::AddBoundConstant);
  cmd.add_bc_ = {ctx, decl, type};
}

void DestroyContext(Reg r) {
  auto &cmd = MakeCmd(nullptr, Op::DestroyContext);
  cmd.reg_  = r;
}

Reg EvaluateAsType(ast::Node const *node, Reg ctx) {
  auto &cmd = MakeCmd(type::Type_, Op::EvaluateAsType);
  cmd.ast_  = {node, ctx};
  return cmd.result;
}

Cmd &MakeCmd(type::Type const *t, Op op) {
  auto &cmd = *ASSERT_NOT_NULL(CompiledFn::Current)
                   ->block(BasicBlock::Current)
                   .cmds_.emplace_back(std::make_unique<Cmd>(t, op));
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

void JumpPlaceholder(BlockDef const *block_def) {
  auto &cmd                         = MakeCmd(nullptr, Op::JumpPlaceholder);
  cmd.block_def_                    = block_def;
  CompiledFn::Current->jumps_.push_back(block_def);
  // TODO implied by jumps_ being non-empty.
  CompiledFn::Current->must_inline_ = true;
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

  Reg tup = CreateTuple();
  for (auto const &val : entries) { AppendToTuple(tup, val); }
  return FinalizeTuple(tup);
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

Reg CreateStruct(core::Scope const *scope, ast::StructLiteral const *parent) {
  auto &cmd          = MakeCmd(type::Type_, Op::CreateStruct);
  cmd.create_struct_ = {scope, parent};
  return cmd.result;
}



Reg ArgumentCache(ast::StructLiteral const *sl) {
  auto &cmd = MakeCmd(type::Ptr(type::Type_), Op::ArgumentCache);
  cmd.sl_   = sl;
  return cmd.result;
}

Reg FinalizeStruct(Reg r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeStruct);
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
  auto &cmd =
      *ASSERT_NOT_NULL(CompiledFn::Current)
          ->block(CompiledFn::Current->entry())
          .cmds_.emplace_back(std::make_unique<Cmd>(type::Ptr(t), Op::Alloca));
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
        visitor::EmitIr visitor;
        t->EmitMoveAssign(&visitor, t, r.get(), GetRet(n, t), ctx);
        visitor.CompleteDeferredBodies();
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

template <typename T>
static void InlinePhiNode(
    CmdIndex cmd_index,
    PhiArgs<T> const &phi_args,
    absl::flat_hash_map<BlockIndex, BlockIndex> const &block_relocs,
    absl::flat_hash_map<Reg, Results> const &reg_relocs) {
  absl::flat_hash_map<BlockIndex, RegisterOr<T>> phi_map;
  for (auto [block, val] : phi_args.map_) {
    phi_map.emplace(block_relocs.at(block),
                    val.is_reg_ ? reg_relocs.at(val.reg_).template get<T>(0)
                                : RegisterOr<T>{val.val_});
  }
  MakePhi(cmd_index, std::move(phi_map));
}

std::pair<Results, bool> CallInline(
    CompiledFn *f, Arguments const &arguments,
    absl::flat_hash_map<ir::BlockDef const *, ir::BlockIndex> const
        &block_map) {
  bool is_jump = false;
  std::vector<Results> return_vals;
  return_vals.resize(f->type_->output.size());

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
    reg_relocs.emplace(Reg::Arg(i), arguments.results().GetResult(i));
  }

  // 2. Initialize block 0.
  for (auto const &cmd : f->block(f->entry()).cmds_) {
    switch (cmd->op_code_) {
      case Op::Alloca:
        reg_relocs.emplace(cmd->result, ir::Results{Alloca(cmd->type_)});
        continue;
      case Op::UncondJump: continue;
      default: UNREACHABLE();
    }
  }

  // 3. Ignoring phi-nodes, iterate over commands in blocks, skipping to the
  //    next one whenever you get stuck.
  std::queue<std::pair<BlockIndex, size_t>> blocks;
  // TODO block relocation is much simpler: 0 -> 0, 1 -> current block, all
  // others are now add are just their index + current size
  absl::flat_hash_map<BlockIndex, BlockIndex> block_relocs;
  block_relocs.emplace(BlockIndex{1}, BasicBlock::Current);
  blocks.emplace(BlockIndex{1}, 0);

  for (size_t i = 2; i < f->blocks_.size(); ++i) {
    blocks.emplace(BlockIndex(i), 0);
    block_relocs.emplace(BlockIndex(i), CompiledFn::AddBlock());
  }

  std::vector<std::pair<GenericPhiArgs *, CmdIndex>> deferred_phis;
  while (!blocks.empty()) {
    auto [block, index] = blocks.front();
    blocks.pop();
    BasicBlock::Current = block_relocs.at(block);
    // TODO could just as easily be a ref
    auto const &block_ref = f->block(block);
    for (; index < block_ref.cmds_.size(); ++index) {
      auto const &cmd = *block_ref.cmds_.at(index);
      switch (cmd.op_code_) {
        case Op::Death: UNREACHABLE();
        case Op::Bytes: {
          RegisterOr<type::Type const *> r;
          if (cmd.type_arg_.is_reg_) {
            auto iter = reg_relocs.find(cmd.type_arg_.reg_);
            if (iter == reg_relocs.end()) { goto next_block; }
            r = iter->second.get<type::Type const *>(0).reg_;
          } else {
            r = cmd.type_arg_;
          }
          reg_relocs.emplace(cmd.result, Bytes(r));
        } break;
        case Op::Align: {
          RegisterOr<type::Type const *> r;
          if (cmd.type_arg_.is_reg_) {
            auto iter = reg_relocs.find(cmd.type_arg_.reg_);
            if (iter == reg_relocs.end()) { goto next_block; }
            r = iter->second.get<type::Type const *>(0).reg_;
          } else {
            r = cmd.type_arg_;
          }
          reg_relocs.emplace(cmd.result, Align(r));
        } break;
#define CASE(op_code, op_fn, type, args)                                       \
  case Op::op_code: {                                                          \
    auto iter = reg_relocs.find(cmd.args);                                     \
    if (iter == reg_relocs.end()) { goto next_block; }                         \
    reg_relocs.emplace(cmd.result, op_fn(iter->second.get<type>(0)));          \
  } break
          CASE(NotBool, Not, bool, reg_);
        case Op::NotFlags: {
          auto iter = reg_relocs.find(cmd.typed_reg_.get());
          if (iter == reg_relocs.end()) { goto next_block; }
          reg_relocs.emplace(cmd.result,
                             Not(type::Typed<RegisterOr<FlagsVal>, type::Flags>{
                                 iter->second.get<FlagsVal>(0),
                                 &cmd.typed_reg_.type()->as<type::Flags>()}));
        } break;
          CASE(NegInt8, Neg, int8_t, reg_);
          CASE(NegInt16, Neg, int16_t, reg_);
          CASE(NegInt32, Neg, int32_t, reg_);
          CASE(NegInt64, Neg, int64_t, reg_);
          CASE(NegFloat32, Neg, float, reg_);
          CASE(NegFloat64, Neg, double, reg_);
#undef CASE

#define CASE(op_code, op_fn, type)                                             \
  case Op::op_code: {                                                          \
    RegisterOr<Addr> r;                                                        \
    if (cmd.addr_arg_.is_reg_) {                                               \
      auto iter = reg_relocs.find(cmd.addr_arg_.reg_);                         \
      if (iter == reg_relocs.end()) { goto next_block; }                       \
      r = op_fn<type>(iter->second.get<Addr>(0));                              \
    } else {                                                                   \
      r = cmd.addr_arg_;                                                       \
    }                                                                          \
    reg_relocs.emplace(cmd.result, r);                                         \
  } break;
          CASE(LoadBool, Load, bool);
          CASE(LoadInt8, Load, int8_t);
          CASE(LoadInt16, Load, int16_t);
          CASE(LoadInt32, Load, int32_t);
          CASE(LoadInt64, Load, int64_t);
          CASE(LoadNat8, Load, uint8_t);
          CASE(LoadNat16, Load, uint16_t);
          CASE(LoadNat32, Load, uint32_t);
          CASE(LoadNat64, Load, uint64_t);
          CASE(LoadFloat32, Load, float);
          CASE(LoadFloat64, Load, double);
          CASE(LoadType, Load, type::Type const *);
          CASE(LoadEnum, Load, EnumVal);
          CASE(LoadFlags, Load, FlagsVal);
          CASE(LoadFunc, Load, AnyFunc);
#undef CASE
        case Op::LoadAddr: {
          RegisterOr<Addr> r;
          if (cmd.addr_arg_.is_reg_) {
            auto iter = reg_relocs.find(cmd.addr_arg_.reg_);
            if (iter == reg_relocs.end()) { goto next_block; }
            // TODO I don't actually care what type I pass in here as long as
            // it's big enough to hold an address. Fix this hack.
            r = Load<Addr>(iter->second.get<Addr>(0), type::Ptr(type::Int64));
          } else {
            r = cmd.addr_arg_;
          }
          reg_relocs.emplace(cmd.result, r);
        } break;

#define CASE(op_code, op_fn, type, arg)                                        \
  case Op::op_code: {                                                          \
    RegisterOr<Addr> r0;                                                       \
    RegisterOr<type> r1;                                                       \
    if (cmd.arg.addr_.is_reg_) {                                               \
      auto iter0 = reg_relocs.find(cmd.arg.addr_.reg_);                        \
      if (iter0 == reg_relocs.end()) { goto next_block; }                      \
      r0 = iter0->second.get<Addr>(0);                                         \
    } else {                                                                   \
      r0 = cmd.arg.addr_;                                                      \
    }                                                                          \
    if (cmd.arg.val_.is_reg_) {                                                \
      auto iter1 = reg_relocs.find(cmd.arg.val_.reg_);                         \
      if (iter1 == reg_relocs.end()) { goto next_block; }                      \
      r1 = iter1->second.get<type>(0);                                         \
    } else {                                                                   \
      r1 = cmd.arg.val_;                                                       \
    }                                                                          \
    op_fn(r1, r0);                                                             \
  } break
          CASE(StoreBool, Store, bool, store_bool_);
          CASE(StoreInt8, Store, int8_t, store_i8_);
          CASE(StoreInt16, Store, int16_t, store_i16_);
          CASE(StoreInt32, Store, int32_t, store_i32_);
          CASE(StoreInt64, Store, int64_t, store_i64_);
          CASE(StoreNat8, Store, uint8_t, store_u8_);
          CASE(StoreNat16, Store, uint16_t, store_u16_);
          CASE(StoreNat32, Store, uint32_t, store_u32_);
          CASE(StoreNat64, Store, uint64_t, store_u64_);
          CASE(StoreFloat32, Store, float, store_float32_);
          CASE(StoreFloat64, Store, double, store_float64_);
          CASE(StoreType, Store, type::Type const *, store_type_);
          CASE(StoreEnum, Store, EnumVal, store_enum_);
          CASE(StoreFunc, Store, ir::AnyFunc, store_func_);
          CASE(StoreFlags, Store, FlagsVal, store_flags_);
          CASE(StoreAddr, Store, ir::Addr, store_addr_);
#undef CASE

#define CASE(op_code, op_fn, type, arg)                                        \
  case Op::op_code: {                                                          \
    RegisterOr<type> r;                                                        \
    if (cmd.arg.is_reg_) {                                                     \
      auto iter = reg_relocs.find(cmd.arg.reg_);                               \
      if (iter == reg_relocs.end()) { goto next_block; }                       \
      r = iter->second.get<type>(0);                                           \
    } else {                                                                   \
      r = cmd.arg;                                                             \
    }                                                                          \
    op_fn(r);                                                                  \
  } break;
          CASE(PrintBool, Print, bool, bool_arg_)
          CASE(PrintInt8, Print, int8_t, i8_arg_)
          CASE(PrintInt16, Print, int16_t, i16_arg_)
          CASE(PrintInt32, Print, int32_t, i32_arg_)
          CASE(PrintInt64, Print, int64_t, i64_arg_)
          CASE(PrintNat8, Print, uint8_t, u8_arg_)
          CASE(PrintNat16, Print, uint16_t, u16_arg_)
          CASE(PrintNat32, Print, uint32_t, u32_arg_)
          CASE(PrintNat64, Print, uint64_t, u64_arg_)
          CASE(PrintFloat32, Print, float, float32_arg_)
          CASE(PrintFloat64, Print, double, float64_arg_)
          CASE(PrintType, Print, type::Type const *, type_arg_)
          // TODO CASE(PrintEnum, Print, EnumVal, print_enum_)
          // TODO CASE(PrintFlags, Print, FlagsVal, print_flags_)
          CASE(PrintAddr, Print, ir::Addr, addr_arg_)
          CASE(PrintByteView, Print, std::string_view, byte_view_arg_)
#undef CASE

#define CASE(op_code, op_fn, type, args)                                       \
  case Op::op_code: {                                                          \
    RegisterOr<type> r0, r1;                                                   \
    if (cmd.args.args_[0].is_reg_) {                                           \
      auto iter0 = reg_relocs.find(cmd.args.args_[0].reg_);                    \
      if (iter0 == reg_relocs.end()) { goto next_block; }                      \
      r0 = iter0->second.get<type>(0);                                         \
    } else {                                                                   \
      r0 = cmd.args.args_[0];                                                  \
    }                                                                          \
    if (cmd.args.args_[1].is_reg_) {                                           \
      auto iter1 = reg_relocs.find(cmd.args.args_[1].reg_);                    \
      if (iter1 == reg_relocs.end()) { goto next_block; }                      \
      r1 = iter1->second.get<type>(0);                                         \
    } else {                                                                   \
      r1 = cmd.args.args_[1];                                                  \
    }                                                                          \
    reg_relocs.emplace(cmd.result, op_fn(r0, r1));                             \
  } break
          CASE(AddNat8, Add, uint8_t, u8_args_);
          CASE(AddNat16, Add, uint16_t, u16_args_);
          CASE(AddNat32, Add, uint32_t, u32_args_);
          CASE(AddNat64, Add, uint64_t, u64_args_);
          CASE(AddInt8, Add, int8_t, i8_args_);
          CASE(AddInt16, Add, int16_t, i16_args_);
          CASE(AddInt32, Add, int32_t, i32_args_);
          CASE(AddInt64, Add, int64_t, i64_args_);
          CASE(AddFloat32, Add, float, float32_args_);
          CASE(AddFloat64, Add, double, float64_args_);
          CASE(SubNat8, Sub, uint8_t, u8_args_);
          CASE(SubNat16, Sub, uint16_t, u16_args_);
          CASE(SubNat32, Sub, uint32_t, u32_args_);
          CASE(SubNat64, Sub, uint64_t, u64_args_);
          CASE(SubInt8, Sub, int8_t, i8_args_);
          CASE(SubInt16, Sub, int16_t, i16_args_);
          CASE(SubInt32, Sub, int32_t, i32_args_);
          CASE(SubInt64, Sub, int64_t, i64_args_);
          CASE(SubFloat32, Sub, float, float32_args_);
          CASE(SubFloat64, Sub, double, float64_args_);
          CASE(MulNat8, Mul, uint8_t, u8_args_);
          CASE(MulNat16, Mul, uint16_t, u16_args_);
          CASE(MulNat32, Mul, uint32_t, u32_args_);
          CASE(MulNat64, Mul, uint64_t, u64_args_);
          CASE(MulInt8, Mul, int8_t, i8_args_);
          CASE(MulInt16, Mul, int16_t, i16_args_);
          CASE(MulInt32, Mul, int32_t, i32_args_);
          CASE(MulInt64, Mul, int64_t, i64_args_);
          CASE(MulFloat32, Mul, float, float32_args_);
          CASE(MulFloat64, Mul, double, float64_args_);
          CASE(DivNat8, Div, uint8_t, u8_args_);
          CASE(DivNat16, Div, uint16_t, u16_args_);
          CASE(DivNat32, Div, uint32_t, u32_args_);
          CASE(DivNat64, Div, uint64_t, u64_args_);
          CASE(DivInt8, Div, int8_t, i8_args_);
          CASE(DivInt16, Div, int16_t, i16_args_);
          CASE(DivInt32, Div, int32_t, i32_args_);
          CASE(DivInt64, Div, int64_t, i64_args_);
          CASE(DivFloat32, Div, float, float32_args_);
          CASE(DivFloat64, Div, double, float64_args_);
          CASE(ModNat8, Mod, uint8_t, u8_args_);
          CASE(ModNat16, Mod, uint16_t, u16_args_);
          CASE(ModNat32, Mod, uint32_t, u32_args_);
          CASE(ModNat64, Mod, uint64_t, u64_args_);
          CASE(ModInt8, Mod, int8_t, i8_args_);
          CASE(ModInt16, Mod, int16_t, i16_args_);
          CASE(ModInt32, Mod, int32_t, i32_args_);
          CASE(ModInt64, Mod, int64_t, i64_args_);
          CASE(LtInt8, Lt, int8_t, i8_args_);
          CASE(LtInt16, Lt, int16_t, i16_args_);
          CASE(LtInt32, Lt, int32_t, i32_args_);
          CASE(LtInt64, Lt, int64_t, i64_args_);
          CASE(LtNat8, Lt, uint8_t, u8_args_);
          CASE(LtNat16, Lt, uint16_t, u16_args_);
          CASE(LtNat32, Lt, uint32_t, u32_args_);
          CASE(LtNat64, Lt, uint64_t, u64_args_);
          CASE(LtFloat32, Lt, float, float32_args_);
          CASE(LtFloat64, Lt, double, float64_args_);
          CASE(LtFlags, Lt, FlagsVal, flags_args_);
          CASE(LeInt8, Le, int8_t, i8_args_);
          CASE(LeInt16, Le, int16_t, i16_args_);
          CASE(LeInt32, Le, int32_t, i32_args_);
          CASE(LeInt64, Le, int64_t, i64_args_);
          CASE(LeNat8, Le, uint8_t, u8_args_);
          CASE(LeNat16, Le, uint16_t, u16_args_);
          CASE(LeNat32, Le, uint32_t, u32_args_);
          CASE(LeNat64, Le, uint64_t, u64_args_);
          CASE(LeFloat32, Le, float, float32_args_);
          CASE(LeFloat64, Le, double, float64_args_);
          CASE(LeFlags, Le, FlagsVal, flags_args_);
          CASE(GtInt8, Gt, int8_t, i8_args_);
          CASE(GtInt16, Gt, int16_t, i16_args_);
          CASE(GtInt32, Gt, int32_t, i32_args_);
          CASE(GtInt64, Gt, int64_t, i64_args_);
          CASE(GtNat8, Gt, uint8_t, u8_args_);
          CASE(GtNat16, Gt, uint16_t, u16_args_);
          CASE(GtNat32, Gt, uint32_t, u32_args_);
          CASE(GtNat64, Gt, uint64_t, u64_args_);
          CASE(GtFloat32, Gt, float, float32_args_);
          CASE(GtFloat64, Gt, double, float64_args_);
          CASE(GtFlags, Gt, FlagsVal, flags_args_);
          CASE(GeInt8, Ge, int8_t, i8_args_);
          CASE(GeInt16, Ge, int16_t, i16_args_);
          CASE(GeInt32, Ge, int32_t, i32_args_);
          CASE(GeInt64, Ge, int64_t, i64_args_);
          CASE(GeNat8, Ge, uint8_t, u8_args_);
          CASE(GeNat16, Ge, uint16_t, u16_args_);
          CASE(GeNat32, Ge, uint32_t, u32_args_);
          CASE(GeNat64, Ge, uint64_t, u64_args_);
          CASE(GeFloat32, Ge, float, float32_args_);
          CASE(GeFloat64, Ge, double, float64_args_);
          CASE(GeFlags, Ge, FlagsVal, flags_args_);
          CASE(EqBool, Eq, bool, bool_args_);
          CASE(EqInt8, Eq, int8_t, i8_args_);
          CASE(EqInt16, Eq, int16_t, i16_args_);
          CASE(EqInt32, Eq, int32_t, i32_args_);
          CASE(EqInt64, Eq, int64_t, i64_args_);
          CASE(EqNat8, Eq, uint8_t, u8_args_);
          CASE(EqNat16, Eq, uint16_t, u16_args_);
          CASE(EqNat32, Eq, uint32_t, u32_args_);
          CASE(EqNat64, Eq, uint64_t, u64_args_);
          CASE(EqFloat32, Eq, float, float32_args_);
          CASE(EqFloat64, Eq, double, float64_args_);
          CASE(EqType, Eq, type::Type const *, type_args_);
          CASE(EqEnum, Eq, EnumVal, enum_args_);
          CASE(EqFlags, Eq, FlagsVal, flags_args_);
          CASE(EqAddr, Eq, ir::Addr, addr_args_);
          CASE(NeInt8, Ne, int8_t, i8_args_);
          CASE(NeInt16, Ne, int16_t, i16_args_);
          CASE(NeInt32, Ne, int32_t, i32_args_);
          CASE(NeInt64, Ne, int64_t, i64_args_);
          CASE(NeNat8, Ne, uint8_t, u8_args_);
          CASE(NeNat16, Ne, uint16_t, u16_args_);
          CASE(NeNat32, Ne, uint32_t, u32_args_);
          CASE(NeNat64, Ne, uint64_t, u64_args_);
          CASE(NeFloat32, Ne, float, float32_args_);
          CASE(NeFloat64, Ne, double, float64_args_);
          CASE(NeType, Ne, type::Type const *, type_args_);
          CASE(NeEnum, Ne, EnumVal, enum_args_);
          CASE(NeFlags, Ne, FlagsVal, flags_args_);
          CASE(NeAddr, Ne, ir::Addr, addr_args_);
#undef CASE

        case Op::CondJump: {
          auto iter = reg_relocs.find(cmd.cond_jump_.cond_);
          if (iter == reg_relocs.end()) { goto next_block; }
          CondJump(iter->second.get<bool>(0),
                   block_relocs.at(cmd.cond_jump_.blocks_[true]),
                   block_relocs.at(cmd.cond_jump_.blocks_[false]));
        } break;
        case Op::UncondJump: {
          auto block_index = block_relocs.at(cmd.block_index_);
          UncondJump(block_index);
          ir::BasicBlock::Current = block_index;
        } break;
        case Op::ReturnJump: {
          // TODO jump to landing block
        } break;
        case Op::JumpPlaceholder: {
          // TODO multiple blocks
          auto iter = block_map.find(cmd.block_def_);
          if (iter != block_map.end()) {
            is_jump = true;
            UncondJump(iter->second);
            ir::BasicBlock::Current = iter->second;
            goto found_valid_jump;
          }
          UNREACHABLE();
        found_valid_jump:;
        } break;

#define CASE(op_code, phi_type, args)                                          \
  case Op::op_code: {                                                          \
    auto cmd_index = Phi(phi_type);                                            \
    auto reg       = CompiledFn::Current->Command(cmd_index).result;           \
    reg_relocs.emplace(cmd.result, reg);                                       \
    deferred_phis.emplace_back(cmd.args, cmd_index);                           \
  } break
          CASE(PhiBool, type::Bool, phi_bool_);
          CASE(PhiInt8, type::Int8, phi_i8_);
          CASE(PhiInt16, type::Int16, phi_i16_);
          CASE(PhiInt32, type::Int32, phi_i32_);
          CASE(PhiInt64, type::Int64, phi_i64_);
          CASE(PhiNat8, type::Nat8, phi_u8_);
          CASE(PhiNat16, type::Nat16, phi_u16_);
          CASE(PhiNat32, type::Nat32, phi_u32_);
          CASE(PhiNat64, type::Nat64, phi_u64_);
          CASE(PhiFloat32, type::Float32, phi_float32_);
          CASE(PhiFloat64, type::Float64, phi_float64_);
          CASE(PhiType, type::Type_, phi_type_);
          // TODO CASE(PhiBlock, ____, phi_block_);
          // TODO CASE(PhiAddr, ____, phi_addr_);
          // TODO CASE(PhiEnum, ____, phi_enum_);
          // TODO CASE(PhiFlags, ____, phi_flags_);
          // TODO CASE(PhiFunc, ____, phi_func_);
#undef CASE
        case Op::GetRet: NOT_YET();
#define CASE(op_code, args)                                                    \
  case Op::op_code: {                                                          \
    if (cmd.args.val_.is_reg_) {                                               \
      auto iter = reg_relocs.find(cmd.args.val_.reg_);                         \
      if (iter == reg_relocs.end()) { goto next_block; }                       \
      return_vals.at(cmd.args.ret_num_) = iter->second;                        \
    } else {                                                                   \
      return_vals.at(cmd.args.ret_num_) = ir::Results{cmd.args.val_.val_};     \
    }                                                                          \
  } break
          CASE(SetRetBool, set_ret_bool_);
          CASE(SetRetInt8, set_ret_i8_);
          CASE(SetRetInt16, set_ret_i16_);
          CASE(SetRetInt32, set_ret_i32_);
          CASE(SetRetInt64, set_ret_i64_);
          CASE(SetRetNat8, set_ret_u8_);
          CASE(SetRetNat16, set_ret_u16_);
          CASE(SetRetNat32, set_ret_u32_);
          CASE(SetRetNat64, set_ret_u64_);
          CASE(SetRetFloat32, set_ret_float32_);
          CASE(SetRetFloat64, set_ret_float64_);
          CASE(SetRetType, set_ret_type_);
          CASE(SetRetEnum, set_ret_enum_);
          CASE(SetRetFlags, set_ret_flags_);
          CASE(SetRetByteView, set_ret_byte_view_);
          CASE(SetRetAddr, set_ret_addr_);
          CASE(SetRetFunc, set_ret_func_);
          CASE(SetRetScope, set_ret_scope_);
          CASE(SetRetGeneric, set_ret_generic_);
          CASE(SetRetModule, set_ret_module_);
          CASE(SetRetBlock, set_ret_block_);
#undef CASE
        case Op::ArgumentCache: NOT_YET();
        case Op::NewOpaqueType: NOT_YET();
        case Op::LoadSymbol: NOT_YET();
        case Op::Init: NOT_YET();
        case Op::Destroy: NOT_YET();
        case Op::Move: NOT_YET();
        case Op::Copy: NOT_YET();
        case Op::VerifyType: UNREACHABLE();
        case Op::EvaluateAsType: UNREACHABLE();
        case Op::CreateContext: UNREACHABLE();
        case Op::AddBoundConstant: UNREACHABLE();
        case Op::DestroyContext: UNREACHABLE();
        case Op::Call: {
          RegisterOr<AnyFunc> r_fn;
          if (cmd.call_.fn_.is_reg_) {
            auto iter = reg_relocs.find(cmd.call_.fn_.reg_);
            if (iter == reg_relocs.end()) { goto next_block; }
            r_fn = iter->second.get<AnyFunc>(0).reg_;
          } else {
            r_fn = cmd.call_.fn_;
          }

          Results new_arg_results;
          for (size_t i = 0; i < cmd.call_.arguments_->results().size(); ++i) {
            if (cmd.call_.arguments_->results().is_reg(i)) {
              auto iter =
                  reg_relocs.find(cmd.call_.arguments_->results().get<Reg>(i));
              if (iter == reg_relocs.end()) { goto next_block; }
              new_arg_results.append(iter->second.GetResult(0));
            } else {
              new_arg_results.append(
                  cmd.call_.arguments_->results().GetResult(i));
            }
          }
          Arguments new_args(cmd.call_.arguments_->type_,
                             std::move(new_arg_results));

          if (cmd.call_.outs_) {
            OutParams outs;
            for (size_t i = 0; i < cmd.call_.outs_->regs_.size(); ++i) {
              if (cmd.call_.outs_->is_loc_[i]) {
                auto old_r = cmd.call_.outs_->regs_[i];
                auto iter  = reg_relocs.find(old_r);
                if (iter == reg_relocs.end()) { goto next_block; }
                // TODO reg_relocs.emplace(, op_fn(r0, r1));
              } else {
                auto r =
                    Reserve(type::Int64);  // TODO this type is probably wrong.
                outs.is_loc_.push_back(false);
                outs.regs_.push_back(r);
                reg_relocs.emplace(cmd.call_.outs_->regs_[i], r);
              }
            }
            Call(r_fn, std::move(new_args), std::move(outs));
          } else {
            Call(r_fn, std::move(new_args));
          }
        } break;
        case Op::PtrIncr: {
          RegisterOr<Addr> r_ptr;
          if (cmd.ptr_incr_.ptr_.is_reg_) {
            auto iter = reg_relocs.find(cmd.ptr_incr_.ptr_.reg_);
            if (iter == reg_relocs.end()) { goto next_block; }
            r_ptr = iter->second.get<Addr>(0);
          } else {
            r_ptr = cmd.ptr_incr_.ptr_.val_;
          }
          RegisterOr<int64_t> r_inc;
          if (cmd.ptr_incr_.ptr_.is_reg_) {
            auto iter = reg_relocs.find(cmd.ptr_incr_.incr_.reg_);
            if (iter == reg_relocs.end()) { goto next_block; }
            r_inc = iter->second.get<int64_t>(0);
          } else {
            r_inc = cmd.ptr_incr_.incr_.val_;
          }
          reg_relocs.emplace(
              cmd.result,
              PtrIncr(r_ptr, r_inc, type::Ptr(cmd.ptr_incr_.pointee_type_)));
        } break;
        default:; NOT_YET(static_cast<int>(cmd.op_code_));
      }
      continue;
    next_block:
      blocks.emplace(block, index);
      break;
    }
  }

  // 4. Go back with a second pass over phi-nodes.
  for (auto [gen_phi_args, cmd_index] : deferred_phis) {
    if (auto *phi_args = gen_phi_args->if_as<PhiArgs<bool>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int8_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int16_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int32_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int64_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint8_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint16_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint32_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint64_t>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<float>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<double>>()) {
      InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
    } else {
      UNREACHABLE();
    }
  }

  Results results;
  for (auto const &r : return_vals) { results.append(r); }
  return std::pair{results, is_jump};
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
  MakeCmd(nullptr, Op::UncondJump).block_index_ = block;
}

void ReturnJump() { auto &cmd = MakeCmd(nullptr, Op::ReturnJump); }

TypedRegister<type::Type const *> NewOpaqueType(::Module *mod) {
  auto &cmd = MakeCmd(type::Type_, Op::NewOpaqueType);
  cmd.mod_  = mod;
  return cmd.result;
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
   if (cmd.op_code_ == Op::PhiBool) { return cmd.phi_bool_->print(os); }
   if (cmd.op_code_ == Op::PhiInt8) { return cmd.phi_i8_->print(os); }
   if (cmd.op_code_ == Op::PhiInt16) { return cmd.phi_i16_->print(os); }
   if (cmd.op_code_ == Op::PhiInt32) { return cmd.phi_i32_->print(os); }
   if (cmd.op_code_ == Op::PhiInt64) { return cmd.phi_i64_->print(os); }
   if (cmd.op_code_ == Op::PhiNat8) { return cmd.phi_u8_->print(os); }
   if (cmd.op_code_ == Op::PhiNat16) { return cmd.phi_u16_->print(os); }
   if (cmd.op_code_ == Op::PhiNat32) { return cmd.phi_u32_->print(os); }
   if (cmd.op_code_ == Op::PhiNat64) { return cmd.phi_u64_->print(os); }
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
