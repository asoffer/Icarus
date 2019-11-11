#include "ir/cmd/misc.h"

namespace ir {
namespace {
void MakeSemanticCmd(SemanticCmd::Kind k, type::Type const *t, Reg r) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<SemanticCmd>();
  blk.cmd_buffer_.append(k);
  blk.cmd_buffer_.append(t);
  blk.cmd_buffer_.append(r);
}

void MakeSemanticCmd(SemanticCmd::Kind k, type::Type const *t, Reg from,
                     RegOr<Addr> to) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<SemanticCmd>();
  blk.cmd_buffer_.append(k);
  blk.cmd_buffer_.append(to.is_reg());
  blk.cmd_buffer_.append(t);
  blk.cmd_buffer_.append(from);
  to.apply([&](auto v) { blk.cmd_buffer_.append(v); });
}
}  // namespace

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

std::string LoadSymbolCmd::DebugString(
    base::untyped_buffer::const_iterator *iter) {
  auto name = iter->read<std::string_view>();
  auto type = iter->read<type::Type const *>();
  auto reg  = iter->read<Reg>();
  return absl::StrCat(stringify(reg), " = load-symbol(", name, ", ",
                      type->to_string(), ")");
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

base::Tagged<core::Alignment, Reg> Align(RegOr<type::Type const *> r) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<TypeInfoCmd>();
  blk.cmd_buffer_.append<uint8_t>(r.is_reg() ? 0x01 : 0x00);

  r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  Reg result = MakeResult<core::Alignment>();
  blk.cmd_buffer_.append(result);
  return result;
}

base::Tagged<core::Bytes, Reg> Bytes(RegOr<type::Type const *> r) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<TypeInfoCmd>();
  blk.cmd_buffer_.append<uint8_t>(0x02 + (r.is_reg() ? 0x01 : 0x00));
  r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  Reg result = MakeResult<core::Bytes>();
  blk.cmd_buffer_.append(result);
  return result;
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

namespace {
Reg MakeAccessCmd(RegOr<Addr> ptr, RegOr<int64_t> inc, type::Type const *t,
                  bool is_array) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<AccessCmd>();
  blk.cmd_buffer_.append(
      AccessCmd::MakeControlBits(is_array, ptr.is_reg(), inc.is_reg()));
  blk.cmd_buffer_.append(t);

  ptr.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  inc.apply([&](auto v) { blk.cmd_buffer_.append(v); });

  Reg result = MakeResult<Addr>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("access")(blk.cmd_buffer_.to_string());
  return result;
}
}  // namespace

base::Tagged<Addr, Reg> PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                                type::Pointer const *t) {
  return base::Tagged<Addr, Reg>{MakeAccessCmd(ptr, inc, t, true)};
}

type::Typed<Reg> Field(RegOr<Addr> r, type::Tuple const *t, int64_t n) {
  auto *p = type::Ptr(t->entries_.at(n));
  return type::Typed<Reg>(MakeAccessCmd(r, n, t, false), p);
}

type::Typed<Reg> Field(RegOr<Addr> r, type::Struct const *t, int64_t n) {
  auto *p = type::Ptr(t->fields().at(n).type);
  return type::Typed<Reg>(MakeAccessCmd(r, n, t, false), p);
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

namespace {
Reg MakeVariantAccessCmd(RegOr<Addr> const &r, type::Variant const *v) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<VariantAccessCmd>();
  bool get_val = (v != nullptr);
  blk.cmd_buffer_.append(get_val);
  blk.cmd_buffer_.append(r.is_reg());
  r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
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
