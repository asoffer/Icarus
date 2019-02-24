#include "ir/arguments.h"

#include "ir/func.h"
#include "ir/val.h"
#include "layout/arch.h"
#include "type/callable.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/util.h"

namespace ir {

std::string Arguments::to_string() const {
  std::stringstream ss;
  size_t i                                  = 0;
  std::vector<type::Type const *> const &ts = [&] {
    if (auto *f = type_->if_as<type::Function>()) { return f->input; }
    if (auto *g = type_->if_as<type::GenericStruct>()) { return g->deps_; }
    UNREACHABLE();
  }();
  for (auto *t : ts) {
    type::Apply(t, [&](auto type_holder) {
      ss << " " << results_.get<typename decltype(type_holder)::type>(i);
    });
    ++i;
  }
  ss << ": ";
  return ss.str();
}

void Arguments::append(RegisterOr<Addr> r) { results_.append(r); }

Arguments::Arguments(type::Callable const *c, Results results)
    : type_(c), results_(std::move(results)) {}

void Arguments::append(const ir::Val &val) {
  std::visit([&](auto v) { results_.append(v); }, val.value);
}

base::untyped_buffer Arguments::PrepareCallBuffer(
    ir::Func *fn, base::untyped_buffer const &regs) {
  // TODO we can compute the exact required size.
  base::untyped_buffer call_buf(32);

  auto offset = layout::Bytes{0};
  auto arch   = layout::Interpretter();

  std::vector<type::Type const *> const &ins = [&] {
    if (auto *f = type_->if_as<type::Function>()) { return f->input; }
    if (auto *g = type_->if_as<type::GenericStruct>()) { return g->deps_; }
    UNREACHABLE();
  }();

  auto outs = [&] {
    if (auto *f = type_->if_as<type::Function>()) { return f->output; }
    if (auto *g = type_->if_as<type::GenericStruct>()) {
      return std::vector<type::Type const *>{type::Type_};
    }
    UNREACHABLE();
  }();

  for (size_t i = 0; i < results_.size(); ++i) {
    bool is_reg = results_.is_reg(i);
    auto *t = (i < ins.size()) ? ins.at(i) : outs.at(i - ins.size());

    if (is_reg) {
      // TODO registers this way are kind of a hack around the type system.
      offset = layout::FwdAlign(offset, layout::Alignment{alignof(ir::Register)});
    } else {
      offset = layout::FwdAlign(offset, t->alignment(arch));
    }
    call_buf.pad_to(offset.value());

    if (t->is_big()) {
      auto reg_or_addr = results_.get<Addr>(i);
      call_buf.append(reg_or_addr.is_reg_
                          ? regs.get<Addr>(fn->compiler_reg_to_offset_.at(
                                reg_or_addr.reg_.value()))
                          : reg_or_addr.val_);
    } else {
      type::Apply(t, [&](auto type_holder) {
        using T = typename decltype(type_holder)::type;
        // NOTE: the use of call_stack.top()... is the same as in resolve<T>,
        // but that's apparently uncapturable due to a GCC bug.
        auto reg_or_val = results_.get<T>(i);
        call_buf.append(reg_or_val.is_reg_
                            ? regs.get<T>(fn->compiler_reg_to_offset_.at(
                                  reg_or_val.reg_.value()))
                            : reg_or_val.val_);

      });
    }

    // TODO bytes(sizeof()) is a hack around the type system.
    offset += is_reg ? layout::Bytes{sizeof(ir::Register)} : t->bytes(arch);
  }

  return call_buf;
}
}  // namespace ir
