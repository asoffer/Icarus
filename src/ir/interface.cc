#include "ir/interface.h"

#include "type/all.h"

namespace IR {
std::vector<std::string> Interface::MatchErrors(const type::Type* t) const {
  std::vector<std::string> errors;
  if (!field_map_.empty()) {
    if (!t->is<type::Struct>()) {
      errors.push_back(t->to_string() +
                       " is not a struct and therefore has no named members.");
      return errors;
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
        errors.push_back(s->to_string() + " has no member named " + name + ".");
      }
    }
  }

  return errors;
}
}  // namespace IR
