#include "ir/cmd/misc.h"

#include <dlfcn.h>

namespace ir {
namespace {
void MakeSemanticCmd(SemanticCmd::Kind k, type::Type const *t, Reg r) {
  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<SemanticCmd>();
  blk.cmd_buffer_.append(k);
  blk.cmd_buffer_.append(t);
  blk.cmd_buffer_.append(r);
}

void MakeSemanticCmd(SemanticCmd::Kind k, type::Type const *t, Reg from,
                     RegOr<Addr> to) {
  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<SemanticCmd>();
  blk.cmd_buffer_.append(k);
  blk.cmd_buffer_.append(to.is_reg_);
  blk.cmd_buffer_.append(t);
  blk.cmd_buffer_.append(from);
  if (to.is_reg_) {
    blk.cmd_buffer_.append(to.reg_);
  } else {
    blk.cmd_buffer_.append(to.val_);
  }
}
}  // namespace

std::optional<BlockIndex> SemanticCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  ir::AnyFunc f;
  base::untyped_buffer call_buf(sizeof(ir::Addr));
  switch (iter->read<Kind>()) {
    case Kind::Init: {
      auto *t = iter->read<type::Type const *>();
      call_buf.append(ctx->resolve<Addr>(iter->read<Reg>()));

      if (auto *s = t->if_as<type::Struct>()) {
        f = s->init_func_;
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->init_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->init_func_.get();
      } else {
        NOT_YET();
      }
    } break;
    case Kind::Destroy: {
      auto *t = iter->read<type::Type const *>();
      call_buf.append(ctx->resolve<Addr>(iter->read<Reg>()));

      if (auto *s = t->if_as<type::Struct>()) {
        f = s->destroy_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->destroy_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->destroy_func_.get();
      } else {
        NOT_YET();
      }
    } break;
    case Kind::Move: {
      bool to_reg = iter->read<bool>();
      auto *t     = iter->read<type::Type const *>();
      call_buf.append(ctx->resolve<Addr>(iter->read<Reg>()));
      call_buf.append(to_reg ? ctx->resolve<Addr>(iter->read<Reg>())
                             : iter->read<Addr>());

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->move_assign_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->move_assign_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->move_assign_func_.get();
      } else {
        NOT_YET();
      }
    } break;
    case Kind::Copy: {
      bool to_reg = iter->read<bool>();
      auto *t     = iter->read<type::Type const *>();
      call_buf.append(ctx->resolve<Addr>(iter->read<Reg>()));
      call_buf.append(to_reg ? ctx->resolve<Addr>(iter->read<Reg>())
                             : iter->read<Addr>());
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->copy_assign_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->copy_assign_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->copy_assign_func_.get();
      } else {
        NOT_YET();
      }
    } break;
  }

  backend::Execute(f, call_buf, ret_slots, ctx);
  return std::nullopt;
}

std::string SemanticCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  size_t num_args = 0;
  std::string_view cmd_name;
  switch (iter->read<Kind>()) {
    case Kind::Init: {
      num_args = 1;
      cmd_name = "init";
    } break;
    case Kind::Destroy: {
      num_args = 1;
      cmd_name = "destroy";
    } break;
    case Kind::Move: {
      num_args = 2;
      cmd_name = "move";
    } break;
    case Kind::Copy: {
      num_args = 2;
      cmd_name = "move";
    } break;
  }

  using base::stringify;
  switch (num_args) {
    case 1: {
      auto *t = iter->read<type::Type const *>();
      return absl::StrCat(cmd_name, " ", t->to_string(), " ",
                          stringify((iter->read<Reg>())));
    } break;
    case 2: {
      bool to_reg = iter->read<bool>();
      auto *t     = iter->read<type::Type const *>();
      auto reg    = iter->read<Reg>();
      return absl::StrCat(cmd_name, " ", stringify(reg), " ",
                          to_reg ? stringify(iter->read<Reg>())
                                 : stringify(iter->read<Addr>()));
    } break;
    default: UNREACHABLE();
  }
}

void SemanticCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                    Inliner const &inliner) {
  size_t num_args = 0;
  switch (iter->read<Kind>()) {
    case Kind::Init: num_args = 1; break;
    case Kind::Destroy: num_args = 1; break;
    case Kind::Move: num_args = 2; break;
    case Kind::Copy: num_args = 2; break;
  }

  switch (num_args) {
    case 1: {
      iter->read<type::Type const *>();
      inliner.Inline(&iter->read<Reg>());
    } break;
    case 2: {
      bool to_reg = iter->read<bool>();
      iter->read<type::Type const *>();
      inliner.Inline(&iter->read<Reg>());
      if (to_reg) {
        inliner.Inline(&iter->read<Reg>());
      } else {
        iter->read<Addr>();
      }
    } break;
    default: UNREACHABLE();
  }
}

void Init(type::Type const *t, Reg r) {
  MakeSemanticCmd(SemanticCmd::Kind::Init, t, r);
}

void Destroy(type::Type const *t, Reg r) {
  MakeSemanticCmd(SemanticCmd::Kind::Destroy, t, r);
}

void Move(type::Type const *t, Reg from, RegOr<Addr> to) {
  MakeSemanticCmd(SemanticCmd::Kind::Move, t, from, to);
}

void Copy(type::Type const *t, Reg from, RegOr<Addr> to) {
  MakeSemanticCmd(SemanticCmd::Kind::Copy, t, from, to);
}

std::optional<BlockIndex> LoadSymbolCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  auto name = iter->read<std::string_view>();
  auto type = iter->read<type::Type const *>();
  auto reg  = iter->read<Reg>();

  void *sym = [&]() -> void * {
    // TODO: this is a hack for now untill we figure out why we can load
    // stderr as a symbol but not write to it.
    if (name == "stderr") { return stderr; }
    if (name == "stdout") { return stdout; }
    return ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, std::string(name).c_str()));
  }();

  auto &frame = ctx->call_stack.top();
  if (type->is<type::Function>()) {
    frame.regs_.set(GetOffset(frame.fn_, reg),
                    ir::AnyFunc{ir::Foreign(sym, type)});
  } else if (type->is<type::Pointer>()) {
    frame.regs_.set(GetOffset(frame.fn_, reg), ir::Addr::Heap(sym));
  } else {
    NOT_YET(type->to_string());
  }

  return std::nullopt;
}

std::string LoadSymbolCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  auto name = iter->read<std::string_view>();
  auto type = iter->read<type::Type const *>();
  auto reg  = iter->read<Reg>();
  return absl::StrCat(stringify(reg), " = load-symbol(", name, ", ",
                      type->to_string(), ")");
}

void LoadSymbolCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                      Inliner const &inliner) {
  iter->read<std::string_view>();
  iter->read<type::Type const *>();
  inliner.Inline(&iter->read<Reg>());
}

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *type) {
  auto &blk = GetBlock();
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

std::optional<BlockIndex> TypeInfoCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  auto ctrl_bits = iter->read<uint8_t>();
  type::Type const *type =
      (ctrl_bits & 0x01) ? ctx->resolve<type::Type const *>(iter->read<Reg>())
                         : iter->read<type::Type const *>();
  auto reg = iter->read<Reg>();

  auto &frame = ctx->call_stack.top();
  if (ctrl_bits & 0x02) {
    frame.regs_.set(GetOffset(frame.fn_, reg),
                    type->alignment(core::Interpretter()));

  } else {
    frame.regs_.set(GetOffset(frame.fn_, reg),
                    type->bytes(core::Interpretter()));
  }

  return std::nullopt;
}

std::string TypeInfoCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  auto ctrl_bits  = iter->read<uint8_t>();
  std::string arg = (ctrl_bits & 0x01)
                        ? stringify(iter->read<Reg>())
                        : iter->read<type::Type const *>()->to_string();
  auto reg = iter->read<Reg>();
  return absl::StrCat(stringify(reg),
                      (ctrl_bits & 0x02) ? " = alignment " : " = bytes ", arg);
}

void TypeInfoCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                    Inliner const &inliner) {
  auto ctrl_bits = iter->read<uint8_t>();
  if (ctrl_bits & 0x01) {
    inliner.Inline(&iter->read<Reg>());
  } else {
    iter->read<type::Type const *>();
  }
  inliner.Inline(&iter->read<Reg>());
}

TypedRegister<core::Alignment> Align(RegOr<type::Type const *> r) {
  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<TypeInfoCmd>();
  blk.cmd_buffer_.append<uint8_t>(r.is_reg_ ? 0x01 : 0x00);
  if (r.is_reg_) {
    blk.cmd_buffer_.append(r.reg_);
  } else {
    blk.cmd_buffer_.append(r.val_);
  }

  Reg result = MakeResult<core::Alignment>();
  blk.cmd_buffer_.append(result);
  return result;
}

TypedRegister<core::Bytes> Bytes(RegOr<type::Type const *> r) {
  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<TypeInfoCmd>();
  blk.cmd_buffer_.append<uint8_t>(0x02 + (r.is_reg_ ? 0x01 : 0x00));
  if (r.is_reg_) {
    blk.cmd_buffer_.append(r.reg_);
  } else {
    blk.cmd_buffer_.append(r.val_);
  }

  Reg result = MakeResult<core::Bytes>();
  blk.cmd_buffer_.append(result);
  return result;
}

std::optional<BlockIndex> AccessCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  auto ctrl_bits   = iter->read<control_bits>();
  auto const *type = iter->read<type::Type const *>();

  Addr addr = ctrl_bits.reg_ptr ? ctx->resolve<Addr>(iter->read<Reg>())
                                : iter->read<Addr>();
  int64_t index = ctrl_bits.reg_index ? ctx->resolve<int64_t>(iter->read<Reg>())
                                      : iter->read<int64_t>();
  auto reg = iter->read<Reg>();

  auto arch = core::Interpretter();
  core::Bytes offset;
  if (ctrl_bits.is_array) {
    offset = core::FwdAlign(type->bytes(arch), type->alignment(arch)) * index;
  } else if (auto *struct_type = type->if_as<type::Struct>()) {
    offset = struct_type->offset(index, arch);
  } else if (auto *tuple_type = type->if_as<type::Tuple>()) {
    offset = struct_type->offset(index, arch);
  }

  auto &frame = ctx->call_stack.top();
  frame.regs_.set(GetOffset(frame.fn_, reg), addr + offset);

  return std::nullopt;
}

std::string AccessCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  auto ctrl_bits   = iter->read<control_bits>();
  auto const *type = iter->read<type::Type const *>();

  auto addr = ctrl_bits.reg_ptr ? RegOr<Addr>(iter->read<Reg>())
                                : RegOr<Addr>(iter->read<Addr>());
  auto index = ctrl_bits.reg_index ? RegOr<int64_t>(iter->read<Reg>())
                                   : RegOr<int64_t>(iter->read<int64_t>());
  auto reg = iter->read<Reg>();
  return absl::StrCat(
      stringify(reg),
      ctrl_bits.is_array ? " = access-array " : " = access-index ",
      type->to_string(), " ", stringify(addr), " ", stringify(index));
}

void AccessCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                  Inliner const &inliner) {
  auto ctrl_bits = iter->read<control_bits>();
  iter->read<type::Type const *>();

  if (ctrl_bits.reg_ptr) {
    inliner.Inline(&iter->read<Reg>());
  } else {
    iter->read<Addr>();
  }

  if (ctrl_bits.reg_index) {
    inliner.Inline(&iter->read<Reg>());
  } else {
    iter->read<int64_t>();
  }

  inliner.Inline(&iter->read<Reg>());
}

namespace {
Reg MakeAccessCmd(RegOr<Addr> ptr, RegOr<int64_t> inc, type::Type const *t,
                  bool is_array) {
  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<AccessCmd>();
  blk.cmd_buffer_.append(
      AccessCmd::MakeControlBits(is_array, ptr.is_reg_, inc.is_reg_));
  blk.cmd_buffer_.append(t);
  if (ptr.is_reg_) {
    blk.cmd_buffer_.append(ptr.reg_);
  } else {
    blk.cmd_buffer_.append(ptr.val_);
  }
  if (inc.is_reg_) {
    blk.cmd_buffer_.append(inc.reg_);
  } else {
    blk.cmd_buffer_.append(inc.val_);
  }

  Reg result = MakeResult<Addr>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("access")(blk.cmd_buffer_.to_string());
  return result;
}
}  // namespace

TypedRegister<Addr> PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                            type::Pointer const *t) {
  return TypedRegister<Addr>{MakeAccessCmd(ptr, inc, t, true)};
}

type::Typed<Reg> Field(RegOr<Addr> r, type::Tuple const *t, int64_t n) {
  auto *p = type::Ptr(t->entries_.at(n));
  return type::Typed<Reg>(MakeAccessCmd(r, n, t, false), p);
}

type::Typed<Reg> Field(RegOr<Addr> r, type::Struct const *t, int64_t n) {
  auto *p = type::Ptr(t->fields().at(n).type);
  return type::Typed<Reg>(MakeAccessCmd(r, n, t, false), p);
}

std::optional<BlockIndex> VariantAccessCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  auto &frame  = ctx->call_stack.top();
  bool get_val = iter->read<bool>();
  bool is_reg  = iter->read<bool>();

  Addr addr =
      is_reg ? ctx->resolve<Addr>(iter->read<Reg>()) : iter->read<Addr>();
  DEBUG_LOG("variant")(addr);
  if (get_val) {
    auto const *variant = iter->read<type::Variant const *>();
    DEBUG_LOG("variant")(variant);
    DEBUG_LOG("variant")(variant->to_string());
    auto arch = core::Interpretter();
    addr += core::FwdAlign(type::Type_->bytes(arch),
                           variant->alternative_alignment(arch));
    DEBUG_LOG("variant")(variant->to_string());
    DEBUG_LOG("variant")(addr);
  }

  Reg reg = iter->read<Reg>();
  DEBUG_LOG("variant")(reg);
  frame.regs_.set(GetOffset(frame.fn_, reg), addr);
  return std::nullopt;
}

std::string VariantAccessCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  bool get_val = iter->read<bool>();
  bool is_reg  = iter->read<bool>();
  auto addr =
      is_reg ? RegOr<Addr>(iter->read<Reg>()) : RegOr<Addr>(iter->read<Addr>());
  if (get_val) {
    auto const *variant = iter->read<type::Variant const *>();
    Reg reg             = iter->read<Reg>();
    return absl::StrCat(stringify(reg), "variant-value ", variant->to_string(),
                        " ", stringify(addr));
  } else {
    Reg reg = iter->read<Reg>();
    return absl::StrCat(stringify(reg), " = variant-type ", stringify(addr));
  }
}

void VariantAccessCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                         Inliner const &inliner) {
  bool get_val = iter->read<bool>();
  bool is_reg  = iter->read<bool>();

  if (is_reg) {
    inliner.Inline(&iter->read<Reg>());
  } else {
    iter->read<Addr>();
  }

  if (get_val) { iter->read<type::Variant const *>(); }

  inliner.Inline(&iter->read<Reg>());
}

namespace {
Reg MakeVariantAccessCmd(RegOr<Addr> const &r, type::Variant const *v) {
  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<VariantAccessCmd>();
  bool get_val = (v != nullptr);
  blk.cmd_buffer_.append(get_val);
  blk.cmd_buffer_.append(r.is_reg_);
  if (r.is_reg_) {
    blk.cmd_buffer_.append(r.reg_);
  } else {
    blk.cmd_buffer_.append(r.val_);
  }
  if (get_val) { blk.cmd_buffer_.append(v); }

  Reg result = MakeResult<Addr>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("variant")(blk.cmd_buffer_.to_string());
  DEBUG_LOG("variant")(blk.cmd_buffer_.buf_.to_string());
  return result;
}
}  // namespace

Reg VariantType(RegOr<Addr> const &r) {
  return MakeVariantAccessCmd(r, nullptr);
}

Reg VariantValue(type::Variant const *v, RegOr<Addr> const &r) {
  return MakeVariantAccessCmd(r, v);
}

}  // namespace ir
