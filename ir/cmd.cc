#include "ir/cmd.h"

#include <cmath>
#include <iostream>
#include <vector>

#include "ast/ast.h"
#include "base/bag.h"
#include "core/arch.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/register.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "ir/reg.h"
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

void AddBlockDefBefore(RegOr<AnyFunc> f) {
  auto &cmd   = MakeCmd(nullptr, Op::AddBlockDefBefore);
  cmd.any_fn_ = f;
}

void AddBlockDefAfter(RegOr<AnyFunc> f) {
  auto &cmd   = MakeCmd(nullptr, Op::AddBlockDefAfter);
  cmd.any_fn_ = f;
}

void AddScopeDefInit(Reg reg, RegOr<AnyFunc> f) {
  auto &cmd               = MakeCmd(nullptr, Op::AddScopeDefInit);
  cmd.add_scope_def_init_ = {reg, f};
}

void AddScopeDefDone(Reg reg, RegOr<AnyFunc> f) {
  auto &cmd               = MakeCmd(nullptr, Op::AddScopeDefDone);
  cmd.add_scope_def_done_ = {reg, f};
}

void Move(type::Type const *t, Reg from, RegOr<Addr> to) {
  auto &cmd     = MakeCmd(nullptr, Op::Move);
  cmd.special2_ = {t, from, to};
}

void Copy(type::Type const *t, Reg from, RegOr<Addr> to) {
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
                      RegOr<type::Type const *> type) {
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

BasicBlock &GetBlock() {
  return ASSERT_NOT_NULL(CompiledFn::Current)->block(BasicBlock::Current);
}

Cmd &MakeCmd(type::Type const *t, Op op) {
  auto &blk = ASSERT_NOT_NULL(CompiledFn::Current)->block(BasicBlock::Current);
  auto &cmd = *blk.cmds_.emplace_back(std::make_unique<Cmd>(t, op));
  blk.cmd_buffer_.append_index<LegacyCmd>();
  DEBUG_LOG("LegacyCmd")(&cmd);
  blk.cmd_buffer_.append(&cmd);
  return cmd;
}

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *t) {
  auto &cmd     = MakeCmd(t, Op::LoadSymbol);
  cmd.load_sym_ = {name, t};
  return type::Typed<Reg>{cmd.result, t};
}

RegOr<int64_t> Bytes(RegOr<type::Type const *> r) {
  auto &cmd     = MakeCmd(type::Int64, Op::Bytes);
  cmd.type_arg_ = r;
  return cmd.result;
}

RegOr<int64_t> Align(RegOr<type::Type const *> r) {
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

RegOr<type::Type const *> Array(RegOr<int64_t> len,
                                     RegOr<type::Type const *> data_type) {
  if (!data_type.is_reg_ && !len.is_reg_) {
    return type::Arr(len.val_, data_type.val_);
  }

  auto &cmd  = MakeCmd(type::Type_, Op::Array);
  cmd.array_ = {len, data_type};
  return cmd.result;
}

type::Typed<Reg> Field(RegOr<Addr> r, type::Tuple const *t, size_t n) {
  auto *p    = type::Ptr(t->entries_.at(n));
  auto &cmd  = MakeCmd(p, Op::Field);
  cmd.field_ = {r, t, n};
  return type::Typed<Reg>(cmd.result, p);
}

type::Typed<Reg> Field(RegOr<Addr> r, type::Struct const *t, size_t n) {
  auto *p    = type::Ptr(t->fields().at(n).type);
  auto &cmd  = MakeCmd(p, Op::Field);
  cmd.field_ = {r, t, n};
  return type::Typed<Reg>(cmd.result, p);
}

Reg Reserve(type::Type const *t) { return CompiledFn::Current->Reserve(t); }

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

  result = MakeResult(t);
  // TODO this isn't done for cmd-buffer commands and needs to be eventually, at
  // least for phi nodes. properties want it to.
  // TODO for implicitly declared out-params of a Call, map them to the call.
  CompiledFn::Current->reg_to_cmd_.emplace(result, cmd_index);
}

Reg MakeResult(type::Type const *t) {
  Reg result = CompiledFn::Current->Reserve(t);
  CompiledFn::Current->references_[result];  // Guarantee it exists.
  return result;
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

Reg VariantType(RegOr<Addr> r) {
  auto &cmd     = MakeCmd(Ptr(type::Type_), Op::VariantType);
  cmd.addr_arg_ = r;
  return cmd.result;
}

Reg VariantValue(type::Type const *t, RegOr<Addr> r) {
  auto &cmd     = MakeCmd(type::Ptr(t), Op::VariantValue);
  cmd.addr_arg_ = r;
  return cmd.result;
}

void CreateStructField(Reg struct_type, RegOr<type::Type const *> type) {
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
  return CompiledFn::Current->Alloca(t);
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

TypedRegister<Addr> PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                            type::Pointer const *t) {
  if (!inc.is_reg_ && inc.val_ == 0 &&
      /* TODO get rid of this last condition */ ptr.is_reg_) {
    return TypedRegister<Addr>{ptr.reg_};
  }
  auto &cmd     = MakeCmd(t, Op::PtrIncr);
  cmd.ptr_incr_ = {ptr, t->pointee, inc};
  return cmd.result;
}

TypedRegister<Addr> Index(type::Pointer const *t, Reg array_ptr,
                          RegOr<int64_t> offset) {
  auto *array_type = &t->pointee->as<type::Array>();
  // TODO this works but generates worse ir (both here and in llvm). It's worth
  // figuring out how to do this better. Is this still true without
  // variable-length arrays?
  return PtrIncr(array_ptr, offset, type::Ptr(array_type->data_type));
}

void Call(RegOr<AnyFunc> const &f, Arguments arguments) {
  auto &block     = CompiledFn::Current->block(BasicBlock::Current);
  Arguments *args = &block.arguments_.emplace_back(std::move(arguments));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  cmd.call_ = Cmd::Call(f, args, nullptr);
}

void Call(RegOr<AnyFunc> const &f, Arguments arguments, OutParams outs) {
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
  absl::flat_hash_map<BlockIndex, RegOr<T>> phi_map;
  for (auto [block, val] : phi_args.map_) {
    phi_map.emplace(block_relocs.at(block),
                    val.is_reg_ ? reg_relocs.at(val.reg_).template get<T>(0)
                                : RegOr<T>{val.val_});
  }
  MakePhi(cmd_index, std::move(phi_map));
}

std::pair<Results, bool> CallInline(
    CompiledFn *f, Arguments const &arguments,
    absl::flat_hash_map<ir::BlockDef const *, ir::BlockIndex> const
        &block_map) {

  bool is_jump = false; // TODO remove this
  std::vector<Results> return_vals;
  return_vals.resize(f->type_->output.size());

  // Note: It is important that the inliner is created before making registers
  // for each of the arguments, because creating the inliner looks state on the
  // current function (counting which register it should start on), and this
  // should exclude the registers we create to hold the arguments.
  auto inliner = CompiledFn::Current->inliner();

  std::vector<Reg> arg_regs;
  arg_regs.reserve(f->type_->input.size());
  for (size_t i = 0; i < f->type_->input.size(); ++i) {
    arg_regs.push_back(
        type::Apply(f->type_->input[i], [&](auto type_holder) -> Reg {
          using T = typename decltype(type_holder)::type;
          return MakeReg(arguments.results().get<T>(i));
        }));
  }

  BlockIndex start(CompiledFn::Current->blocks_.size());

  for (size_t i = 1; i < f->blocks_.size(); ++i) {
    auto &block = CompiledFn::Current->block(CompiledFn::AddBlock());
    block       = f->blocks_.at(i);
    block.cmd_buffer_.UpdateForInlining(inliner);
  }

  auto &block = CompiledFn::Current->block(BasicBlock::Current);

  UncondJump(start);
  BasicBlock::Current = inliner.landing();

  size_t i = 0;
  for (auto const &block : CompiledFn::Current->blocks_) {
    DEBUG_LOG("str")(i, ": ", block.cmd_buffer_.to_string());
    i++;
  }

  inliner.MergeAllocations(CompiledFn::Current, f->allocs());
  for (auto const &cmd : f->block(f->entry()).cmds_) {
    switch (cmd->op_code_) {
      default: UNREACHABLE();
    }
  }

  // // 4. Go back with a second pass over phi-nodes.
  // for (auto [gen_phi_args, cmd_index] : deferred_phis) {
  //   if (auto *phi_args = gen_phi_args->if_as<PhiArgs<bool>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int8_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int16_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int32_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<int64_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint8_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint16_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint32_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<uint64_t>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<float>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else if (auto *phi_args = gen_phi_args->if_as<PhiArgs<double>>()) {
  //     InlinePhiNode(cmd_index, *phi_args, block_relocs, reg_relocs);
  //   } else {
  //     UNREACHABLE();
  //   }
  // }

  Results results;
  for (auto const &r : return_vals) { results.append(r); }
  return std::pair{results, is_jump};
}

TypedRegister<type::Type const *> NewOpaqueType(::Module *mod) {
  auto &cmd = MakeCmd(type::Type_, Op::NewOpaqueType);
  cmd.mod_  = mod;
  return cmd.result;
}

template <typename T>
static std::ostream &operator<<(std::ostream &os,
                                std::array<RegOr<T>, 2> r) {
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
                                      RegOr<type::Type const *>>) {
    std::stringstream ss;
    if (val.is_reg_) {
      ss << stringify(val.reg_);
    } else {
      if (val.val_ == nullptr) {
        ss << "0x0";
      } else {
        ss << val.val_->to_string();
      }
    }
    return ss.str();
  } else if constexpr (std::is_same_v<std::decay_t<T>, Reg>) {
    return stringify(val);
  } else {
    return val;
  }
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::Store<T> const &s) {
  return os << stringify(s.addr_) << " <- " << Stringify(s.val_);
}

template <typename T>
std::ostream &operator<<(std::ostream &os, Cmd::Args<T> const &a) {
  return os << Stringify(a.args_[0]) << " " << Stringify(a.args_[1]);
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::SetRet<T> const &s) {
  return os << s.ret_num_ << " " << Stringify(s.val_);
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

static std::ostream &operator<<(std::ostream &os, Cmd::LoadSymbol const &ls) {
  return os << ls.name_ << ": " << ASSERT_NOT_NULL(ls.type_)->to_string();
}

static std::ostream &operator<<(std::ostream &os, Cmd::Call const &call) {
  if (call.fn_.is_reg_) {
    os << stringify(call.fn_.reg_);
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
      os << stringify( call.outs_->regs_[i]);
    }
  }

  return os;
}

static std::ostream &operator<<(std::ostream &os, Cmd::AddEnumerator const &s) {
  return os << stringify(s.enum_) << " " << s.name_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::SetEnumerator const &s) {
  return os << stringify(s.enum_) << " " << s.val_;
}

std::ostream &operator<<(std::ostream &os, Cmd const &cmd) {
  if (cmd.result != Reg{}) { os << stringify(cmd.result) << " = "; }
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
