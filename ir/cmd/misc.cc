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
  auto ctrl_bits = iter->read<uint8_t>();
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
  blk.cmd_buffer_.append<uint8_t>(r.is_reg_ ? 0x01: 0x00);
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
  blk.cmd_buffer_.append<uint8_t>(0x02 + (r.is_reg_ ? 0x01: 0x00));
  if (r.is_reg_) {
    blk.cmd_buffer_.append(r.reg_);
  } else {
    blk.cmd_buffer_.append(r.val_);
  }

  Reg result = MakeResult<core::Bytes>();
  blk.cmd_buffer_.append(result);
  return result;
}


}  // namespace ir
