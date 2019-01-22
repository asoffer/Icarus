#include "ir/arguments.h"

#include "architecture.h"
#include "ir/val.h"
#include "type/all.h"

namespace ir {
std::string Arguments::to_string() const {
  std::stringstream ss;
  auto arch     = Architecture::InterprettingMachine();
  size_t offset = 0;
  size_t i      = 0;
  base::vector<type::Type const *> const &ts = [&] {
    if (auto *f = type_->if_as<type::Function>()) { return f->input; }
    if (auto *g = type_->if_as<type::GenericStruct>()) { return g->deps_; }
    UNREACHABLE();
  }();
  for (auto *t : ts) {
    if (is_reg_[i]) {
      // TODO wrap this up somewhere.
      offset = ((offset - 1) | (alignof(Register) - 1)) + 1;
      ss << " " << args_.get<Register>(offset).to_string();
      offset += sizeof(Register);
    } else {
      offset = arch.MoveForwardToAlignment(t, offset);
      type::Apply(t, [&](auto type_holder) {
        using T = typename decltype(type_holder)::type;
        ss << " " << args_.get<T>(offset);
      });
      offset += arch.bytes(t);
    }
    ++i;
  }
  ss << ": ";
  return ss.str();
}

void Arguments::append(RegisterOr<Addr> r) {
  is_reg_.push_back(r.is_reg_);
  if (r.is_reg_) {
    args_.append(r.reg_);
  } else {
    args_.append(r.val_);
  }
}

void Arguments::append(const ir::Val &val) {
  // TODO deal with alignment?
  std::visit(
      base::overloaded{
          [&](auto &&v) {
            args_.append(v);
            is_reg_.push_back(
                std::is_same_v<ir::Register, std::decay_t<decltype(v)>>);
          }},
      val.value);
}

base::untyped_buffer Arguments::PrepareCallBuffer(
    base::untyped_buffer const &regs) {
  // TODO we can compute the exact required size.
  base::untyped_buffer call_buf(32);

  size_t offset   = 0;
  auto arch       = Architecture::InterprettingMachine();

  base::vector<type::Type const *> const &ins = [&] {
    if (auto *f = type_->if_as<type::Function>()) { return f->input; }
    if (auto *g = type_->if_as<type::GenericStruct>()) { return g->deps_; }
    UNREACHABLE();
  }();

  auto outs = [&] {
    if (auto *f = type_->if_as<type::Function>()) { return f->output; }
    if (auto *g = type_->if_as<type::GenericStruct>()) {
      return base::vector<type::Type const *>{type::Type_};
    }
    UNREACHABLE();
  }();

  for (size_t i = 0; i < is_reg_.size(); ++i) {
    bool is_reg = is_reg_[i];
    auto *t = (i < ins.size()) ? ins.at(i) : outs.at(i - ins.size());

    if (is_reg) {
      offset = ((offset - 1) | (alignof(ir::Register) - 1)) + 1;
    } else {
      offset = arch.MoveForwardToAlignment(t, offset);
    }
    call_buf.pad_to(offset);

    if (t->is_big()) {
      call_buf.append(
          is_reg ? regs.get<ir::Addr>(args_.get<ir::Register>(offset).value)
                 : args_.get<ir::Addr>(offset));
    } else {
      type::Apply(t, [&](auto type_holder) {
        using T = typename decltype(type_holder)::type;
        // NOTE: the use of call_stack.top()... is the same as in resolve<T>,
        // but that's apparently uncapturable due to a GCC bug.
        call_buf.append(is_reg
                            ? regs.get<T>(args_.get<ir::Register>(offset).value)
                            : args_.get<T>(offset));
      });
    }

    offset += is_reg ? sizeof(ir::Register) : arch.bytes(t);
  }
  return call_buf;
}
}  // namespace ir
