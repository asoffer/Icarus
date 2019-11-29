#include "ir/builder.h"

#include <memory>

#include "ir/block_group.h"

namespace ir {

thread_local Builder current;

Builder &GetBuilder() { return current; }

BasicBlock *Builder::AddBlock() { return CurrentGroup()->AppendBlock(); }

SetCurrent::SetCurrent(internal::BlockGroup *group, Builder *builder)
    : builder_(builder ? builder : &GetBuilder()),
      old_group_(builder_->CurrentGroup()),
      old_block_(builder_->CurrentBlock()) {
  builder_->CurrentGroup()  = group;
  builder_->current_.block_ = group->entry();
}

SetCurrent::~SetCurrent() {
  builder_->CurrentGroup() = old_group_;
  builder_->CurrentBlock() = old_block_;
}

base::Tagged<Addr, Reg> Builder::Alloca(type::Type const* t) {
  return CurrentGroup()->Alloca(t);
}

base::Tagged<Addr, Reg> Builder::TmpAlloca(type::Type const* t) {
  auto reg = Alloca(t);
  current_.temporaries_to_destroy_.emplace_back(reg, t);
  return reg;
}

Reg Reserve(core::Bytes b, core::Alignment a) {
  return current.CurrentGroup()->Reserve(b, a);
}

Reg Reserve(type::Type const* t) { return current.CurrentGroup()->Reserve(t); }

void Builder::Call(RegOr<AnyFunc> const &fn, type::Function const *f,
                   absl::Span<Results const> arguments, OutParams outs) {
  auto &buf = CurrentBlock()->cmd_buffer_;
  ASSERT(arguments.size() == f->input.size());
  buf.append_index<CallCmd>();
  buf.append(fn.is_reg());
  internal::WriteBits<uint16_t, Results>(&buf, arguments, [](Results const &r) {
    ASSERT(r.size() == 1u);
    return r.is_reg(0);
  });

  fn.apply([&](auto v) { buf.append(v); });
  size_t bytes_written_slot = buf.reserve<core::Bytes>();
  size_t arg_index          = 0;
  for (Results const &arg : arguments) {
    if (arg.is_reg(0)) {
      buf.append(arg.get<Reg>(0));
    } else {
      type::Apply(f->input[arg_index], [&](auto tag) {
        using T = typename decltype(tag)::type;
        buf.append(arg.get<T>(0).value());
      });
    }
    ++arg_index;
  }
  buf.set(bytes_written_slot,
          core::Bytes{buf.size() - bytes_written_slot - sizeof(core::Bytes)});

  buf.append<uint16_t>(f->output.size());
  for (Reg r : outs.regs_) { buf.append(r); }
}

void Builder::UncondJump(BasicBlock const *block) {
  auto &buf = CurrentBlock()->cmd_buffer_;
  DEBUG_LOG()("UncondJump");
  buf.append_index<JumpCmd>();
  buf.append(JumpCmd::Kind::kUncond);
  buf.append(block);
}

void Builder::ReturnJump() {
  auto &buf = CurrentBlock()->cmd_buffer_;
  buf.append_index<JumpCmd>();
  buf.append(JumpCmd::Kind::kRet);
  // This extra block index is so that when inlined, we don't have to worry
  // about iterator invalidation, as a return becomes an unconditional jump
  // needing extra space.
  buf.append(ReturnBlock());
}

void Builder::CondJump(RegOr<bool> cond, BasicBlock const *true_block,
                       BasicBlock const *false_block) {
  auto &buf = CurrentBlock()->cmd_buffer_;
  if (cond.is_reg()) {
    buf.append_index<JumpCmd>();
    buf.append(JumpCmd::Kind::kCond);
    buf.append(cond.reg());
    buf.append(false_block);
    buf.append(true_block);
  } else {
    UncondJump(cond.value() ? true_block : false_block);
  }
}

void Builder::ChooseJump(absl::Span<std::string_view const> names,
                         absl::Span<BasicBlock *const> blocks) {
  ASSERT(names.size() == blocks.size());
  auto &buf = CurrentBlock()->cmd_buffer_;
  buf.append_index<JumpCmd>();
  buf.append(JumpCmd::Kind::kChoose);
  buf.append<uint16_t>(names.size());
  for (std::string_view name : names) { buf.append(name); }
  for (BasicBlock* block : blocks) { buf.append(block); }
}

namespace {

void MakeSemanticCmd(SemanticCmd::Kind k, type::Type const *t, Reg r,
                     Builder *bldr) {
  auto &buf = bldr->CurrentBlock()->cmd_buffer_;
  buf.append_index<SemanticCmd>();
  buf.append(k);
  buf.append(t);
  buf.append(r);
}

void MakeSemanticCmd(SemanticCmd::Kind k, type::Type const *t, Reg from,
                     RegOr<Addr> to, Builder *bldr) {
  auto &buf = bldr->CurrentBlock()->cmd_buffer_;
  buf.append_index<SemanticCmd>();
  buf.append(k);
  buf.append(to.is_reg());
  buf.append(t);
  buf.append(from);
  to.apply([&](auto v) { buf.append(v); });
}
}  // namespace

void Builder::Init(type::Type const *t, Reg r) {
  MakeSemanticCmd(SemanticCmd::Kind::Init, t, r, this);
}

void Builder::Destroy(type::Type const *t, Reg r) {
  MakeSemanticCmd(SemanticCmd::Kind::Destroy, t, r, this);
}

void Builder::Move(type::Type const *t, Reg from, RegOr<Addr> to) {
  MakeSemanticCmd(SemanticCmd::Kind::Move, t, from, to, this);
}

void Builder::Copy(type::Type const *t, Reg from, RegOr<Addr> to) {
  MakeSemanticCmd(SemanticCmd::Kind::Copy, t, from, to, this);
}

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *type) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<LoadSymbolCmd>();
  blk.cmd_buffer_.append(name);
  blk.cmd_buffer_.append(type);
  Reg result = [&] {
    if (type->is<type::Function>()) { return MakeResult<AnyFunc>(); }
    if (type->is<type::Pointer>()) { return MakeResult<Addr>(); }
    NOT_YET(type->to_string());
  }();
  blk.cmd_buffer_.append(result);
  return type::Typed<Reg>(result, type);
}

base::Tagged<core::Alignment, Reg> Builder::Align(RegOr<type::Type const *> r) {
  auto &buf = CurrentBlock()->cmd_buffer_;
  buf.append_index<TypeInfoCmd>();
  buf.append<uint8_t>(r.is_reg() ? 0x01 : 0x00);

  r.apply([&](auto v) { buf.append(v); });
  Reg result = MakeResult<core::Alignment>();
  buf.append(result);
  return result;
}

base::Tagged<core::Bytes, Reg> Builder::Bytes(RegOr<type::Type const *> r) {
  auto &buf = CurrentBlock()->cmd_buffer_;
  buf.append_index<TypeInfoCmd>();
  buf.append<uint8_t>(0x02 + (r.is_reg() ? 0x01 : 0x00));
  r.apply([&](auto v) { buf.append(v); });
  Reg result = MakeResult<core::Bytes>();
  buf.append(result);
  return result;
}

namespace {
template <bool IsArray>
Reg MakeAccessCmd(Builder *bldr, RegOr<Addr> ptr, RegOr<int64_t> inc,
                  type::Type const *t) {
  auto &buf = bldr->CurrentBlock()->cmd_buffer_;
  buf.append_index<AccessCmd>();
  buf.append(AccessCmd::MakeControlBits(IsArray, ptr.is_reg(), inc.is_reg()));
  buf.append(t);

  ptr.apply([&](auto v) { buf.append(v); });
  inc.apply([&](auto v) { buf.append(v); });

  Reg result = MakeResult<Addr>();
  buf.append(result);
  return result;
}
}  // namespace

base::Tagged<Addr, Reg> Builder::PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                                         type::Pointer const *t) {
  return base::Tagged<Addr, Reg>{MakeAccessCmd<true>(this, ptr, inc, t)};
}

type::Typed<Reg> Builder::Field(RegOr<Addr> r, type::Tuple const *t, int64_t n) {
  return type::Typed<Reg>(MakeAccessCmd<false>(this, r, n, t),
                          type::Ptr(t->entries_.at(n)));
}

type::Typed<Reg> Builder::Field(RegOr<Addr> r, type::Struct const *t,
                                int64_t n) {
  return type::Typed<Reg>(MakeAccessCmd<false>(this, r, n, t),
                          type::Ptr(t->fields().at(n).type));
}

Reg Builder::VariantType(RegOr<Addr> const &r) {
  auto &blk = *CurrentBlock();
  blk.cmd_buffer_.append_index<VariantAccessCmd>();
  blk.cmd_buffer_.append(false);
  blk.cmd_buffer_.append(r.is_reg());
  r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  Reg result = MakeResult<Addr>();
  blk.cmd_buffer_.append(result);
  return result;
}

Reg Builder::VariantValue(type::Variant const *v, RegOr<Addr> const &r) {
  auto &blk = *CurrentBlock();
  blk.cmd_buffer_.append_index<VariantAccessCmd>();
  blk.cmd_buffer_.append(true);
  blk.cmd_buffer_.append(r.is_reg());
  r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  Reg result = MakeResult<Addr>();
  blk.cmd_buffer_.append(result);
  return result;
}

Reg BlockHandler(ir::BlockDef *block_def,
                 absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<Jump const *> const> afters) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<BlockCmd>();
  blk.cmd_buffer_.append(block_def);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, befores);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, afters);
  Reg r = MakeResult<BlockDef const *>();
  blk.cmd_buffer_.append(r);
  return r;
}

Reg ScopeHandler(
    ir::ScopeDef *scope_def, absl::Span<RegOr<Jump const *> const> inits,
    absl::Span<RegOr<AnyFunc> const> dones,
    absl::flat_hash_map<std::string_view, BlockDef *> const &blocks) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<ScopeCmd>();
  blk.cmd_buffer_.append(scope_def);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, inits);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, dones);
  blk.cmd_buffer_.append<uint16_t>(blocks.size());
  for (auto [name, block] : blocks) {
    blk.cmd_buffer_.append(name);
    blk.cmd_buffer_.append(block);
  }
  Reg r = MakeResult<ScopeDef const *>();
  blk.cmd_buffer_.append(r);
  return r;
}

namespace {

template <bool IsEnumNotFlags>
Reg EnumerationImpl(
    module::BasicModule *mod, absl::Span<std::string_view const> names,
    absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
        &specified_values) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<EnumerationCmd>();
  blk.cmd_buffer_.append(IsEnumNotFlags);
  blk.cmd_buffer_.append<uint16_t>(names.size());
  blk.cmd_buffer_.append<uint16_t>(specified_values.size());
  blk.cmd_buffer_.append(mod);
  for (auto name : names) { blk.cmd_buffer_.append(name); }

  for (auto const &[index, val] : specified_values) {
    // TODO these could be packed much more efficiently.
    blk.cmd_buffer_.append(index);
    blk.cmd_buffer_.append<bool>(val.is_reg());
    val.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  }

  Reg result =
      MakeResult<std::conditional_t<IsEnumNotFlags, EnumVal, FlagsVal>>();
  blk.cmd_buffer_.append(result);
  return result;
}
}  // namespace

Reg Enum(module::BasicModule *mod, absl::Span<std::string_view const> names,
         absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
             &specified_values) {
  return EnumerationImpl<true>(mod, names, specified_values);
}

Reg Flags(module::BasicModule *mod, absl::Span<std::string_view const> names,
          absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
              &specified_values) {
  return EnumerationImpl<false>(mod, names, specified_values);
}

Reg Struct(ast::Scope const *scope, module::BasicModule *mod,
           std::vector<std::tuple<std::string_view, RegOr<type::Type const *>>>
               fields) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<StructCmd>();
  blk.cmd_buffer_.append<uint16_t>(fields.size());
  blk.cmd_buffer_.append(scope);
  blk.cmd_buffer_.append(mod);
  // TODO determine if order randomization makes sense here. Or perhaps you want
  // to do it later? Or not at all?
  std::shuffle(fields.begin(), fields.end(), absl::BitGen{});
  for (auto &[name, t] : fields) { blk.cmd_buffer_.append(name); }

  // TODO performance: Serialize requires an absl::Span here, but we'd love to
  // not copy out the elements of `fields`.
  std::vector<RegOr<type::Type const *>> types;
  types.reserve(fields.size());
  for (auto &[name, t] : fields) { types.push_back(t); }
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, absl::MakeConstSpan(types));

  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  return result;
}

RegOr<type::Function const *> Builder::Arrow(
    absl::Span<RegOr<type::Type const *> const> ins,
    absl::Span<RegOr<type::Type const *> const> outs) {
  if (absl::c_all_of(
          ins, [](RegOr<type::Type const *> r) { return not r.is_reg(); }) and
      absl::c_all_of(
          outs, [](RegOr<type::Type const *> r) { return not r.is_reg(); })) {
    std::vector<type::Type const *> in_vec, out_vec;
    in_vec.reserve(ins.size());
    for (auto in : ins) { in_vec.push_back(in.value()); }
    out_vec.reserve(outs.size());
    for (auto out : outs) { out_vec.push_back(out.value()); }
    return type::Func(std::move(in_vec), std::move(out_vec));
  }

  auto &buf = CurrentBlock()->cmd_buffer_;
  buf.append_index<ArrowCmd>();
  internal::Serialize<uint16_t>(&buf, ins);
  internal::Serialize<uint16_t>(&buf, outs);

  Reg result = MakeResult<type::Type const *>();
  buf.append(result);
  return RegOr<type::Function const *>{result};
}

Reg Builder::OpaqueType(module::BasicModule const *mod) {
  auto &buf = CurrentBlock()->cmd_buffer_;
  buf.append_index<OpaqueTypeCmd>();
  buf.append(mod);
  Reg result = MakeResult<type::Type const *>();
  buf.append(result);
  return result;
}

RegOr<type::Type const *> Builder::Array(RegOr<ArrayCmd::length_t> len,
                                         RegOr<type::Type const *> data_type) {
  if (not len.is_reg() and data_type.is_reg()) {
    return type::Arr(len.value(), data_type.value());
  }

  auto &buf = CurrentBlock()->cmd_buffer_;
  buf.append_index<ArrayCmd>();
  buf.append(ArrayCmd::MakeControlBits(len.is_reg(), data_type.is_reg()));

  len.apply([&](auto v) { buf.append(v); });
  data_type.apply([&](auto v) { buf.append(v); });
  Reg result = MakeResult<type::Type const *>();
  buf.append(result);
  return result;
}

}  // namespace ir
