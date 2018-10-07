#include "ir/interface.h"

#include "type/all.h"

namespace IR {
base::vector<std::string> Interface::MatchErrors(const type::Type* t) const {
  base::vector<std::string> errors;
  if (!field_map_.empty()) {
    auto CheckStruct = [&errors, this](const type::Type* t) {
      if (!t->is<type::Struct>()) {
        errors.push_back(
            t->to_string() +
            " is not a struct and therefore has no named members.");
        return;
      }
      const auto* s = &t->as<type::Struct>();
      for (const auto & [ name, type ] : field_map_) {
        if (auto* field = s->field(name)) {
          if (field->type != type) {
            errors.push_back(s->to_string() + " has a meber named " + name +
                             ", but it has type " + field->type->to_string() +
                             ", rather than the required " + type->to_string());
          }
        } else {
          errors.push_back(s->to_string() + " has no member named " + name +
                           ".");
        }
      }
    };

    if (t->is<type::Variant>()) {
      for (auto* v : t->as<type::Variant>().variants_) { CheckStruct(v); }
    } else {
      CheckStruct(t);
    }
  }

  return errors;
}
}  // namespace IR
