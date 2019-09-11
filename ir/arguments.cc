#include "ir/arguments.h"

#include "core/arch.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/util.h"

namespace ir {

std::string Arguments::to_string() const { return results_.to_string(); }

std::vector<type::Type const *> const &Arguments::input_types() const {
  if (auto *f = type_->if_as<type::Function>()) { return f->input; }
  if (auto *g = type_->if_as<type::GenericStruct>()) { return g->deps_; }
  UNREACHABLE();
}

base::untyped_buffer Arguments::PrepareCallBuffer(
    absl::flat_hash_map<Reg, size_t> const &reg_to_offset,
    base::untyped_buffer const &regs) {
  // TODO we can compute the exact required size.
  base::untyped_buffer call_buf(32);

  auto offset = core::Bytes{0};
  auto arch   = core::Interpretter();

  std::vector<type::Type const *> const &ins = input_types();
  auto outs                                  = [&] {
    if (auto *f = type_->if_as<type::Function>()) { return f->output; }
    if (auto *g = type_->if_as<type::GenericStruct>()) {
      return std::vector<type::Type const *>{type::Type_};
    }
    UNREACHABLE();
  }();

  for (size_t i = 0; i < results_.size(); ++i) {
    bool is_reg = results_.is_reg(i);
    auto *t     = (i < ins.size()) ? ins.at(i) : outs.at(i - ins.size());

    // TODO registers this way are kind of a hack around the type system.
    offset = is_reg ? core::FwdAlign(offset, core::Alignment{alignof(Reg)})
                    : core::FwdAlign(offset, t->alignment(arch));
    call_buf.pad_to(offset.value());

    if (t->is_big()) {
      auto reg_or_addr = results_.get<Addr>(i);
      call_buf.append(reg_or_addr.is_reg_
                          ? regs.get<Addr>(reg_to_offset.at(reg_or_addr.reg_))
                          : reg_or_addr.val_);
    } else {
      type::Apply(t, [&](auto tag) {
        using T = typename decltype(tag)::type;
        auto reg_or_val = results_.get<T>(i);
        call_buf.append(reg_or_val.is_reg_
                            ? regs.get<T>(reg_to_offset.at(reg_or_val.reg_))
                            : reg_or_val.val_);

      });
    }

    // TODO bytes(sizeof()) is a hack around the type system.
    offset += is_reg ? core::Bytes{sizeof(Reg)} : t->bytes(arch);
  }

  return call_buf;
}
}  // namespace ir
