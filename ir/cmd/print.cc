#include "ir/cmd/print.h"

namespace ir {
std::optional<BlockIndex> PrintCmd::Execute(
    base::untyped_buffer::iterator* iter, std::vector<Addr> const& ret_slots,
    backend::ExecContext* ctx) {
  auto ctrl = iter->read<control_bits>();
  PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
    using T = typename std::decay_t<decltype(tag)>::type;
    T val   = ctrl.reg ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
    DEBUG_LOG("print")(typeid(T).name());
    if constexpr (std::is_same_v<T, bool>) {
      std::cerr << (val ? "true" : "false");
    } else if constexpr (std::is_same_v<T, uint8_t>) {
      std::cerr << static_cast<unsigned int>(val);
    } else if constexpr (std::is_same_v<T, int8_t>) {
      std::cerr << static_cast<int>(val);
    } else if constexpr (std::is_same_v<T, type::Type const*>) {
      std::cerr << val->to_string();
    } else if constexpr (std::is_same_v<T, Addr>) {
      std::cerr << val.to_string();
    } else if constexpr (std::is_same_v<T, EnumVal>) {
      std::optional<std::string_view> name =
          iter->read<type::Enum const*>()->name(val);
      std::cerr << (name.has_value() ? *name : absl::StrCat(val.value));
    } else if constexpr (std::is_same_v<T, FlagsVal>) {
      auto numeric_val = val.value;
      std::vector<std::string> vals;
      auto flags_type = iter->read<type::Flags const*>();

      while (numeric_val != 0) {
        size_t mask = (numeric_val & ((~numeric_val) + 1));
        numeric_val -= mask;

        std::optional<std::string_view> name =
            flags_type->name(ir::FlagsVal(mask));
        vals.emplace_back(name.has_value() ? std::string{*name}
                                           : std::to_string(mask));
      }

      if (vals.empty()) {
        std::cerr << "(empty)";
      } else {
        auto iter = vals.begin();
        std::cerr << *iter++;
        while (iter != vals.end()) { std::cerr << " | " << *iter++; }
      }
    } else {
      std::cerr << val;
    }
  });
  return std::nullopt;
}

std::string PrintCmd::DebugString(base::untyped_buffer::const_iterator* iter) {
  std::string s;
  auto ctrl = iter->read<control_bits>();
  if (ctrl.reg) {
    s.append(stringify(iter->read<Reg>()));
  } else {
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using T = typename std::decay_t<decltype(tag)>::type;
      using base::stringify;
      s.append(stringify(iter->read<T>()));
    });
  }
  return s;
}

void PrintCmd::UpdateForInlining(base::untyped_buffer::iterator* iter,
                                 Inliner const& inliner) {
  auto ctrl = iter->read<control_bits>();
  if (ctrl.reg) {
    inliner.Inline(&iter->read<Reg>());
  } else {
    // TODO: Add core::LayoutRequirements so you can skip forward by the
    // appropriate amount without instantiating so many templates.
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      iter->read<typename std::decay_t<decltype(tag)>::type>();
    });
  }
}

}  // namespace ir
