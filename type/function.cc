#include "type/function.h"

#include "base/guarded.h"
#include "type/typed_value.h"

namespace type {
Type const *Generic = new GenericFunction;

core::Bytes GenericFunction::bytes(core::Arch const &) const {
  return core::Host().ptr_bytes;
}

core::Alignment GenericFunction::alignment(core::Arch const &) const {
  return core::Host().ptr_alignment;
}

static base::guarded<std::map<std::vector<Type const *>,
                               std::map<std::vector<Type const *>, Function>>>
    funcs_;
Function const *Func(std::vector<Type const *> in,
                     std::vector<Type const *> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);
  return &(*funcs_.lock())[std::move(in)]
              .emplace(std::move(out), std::move(f))
              .first->second;
}

void Function::WriteTo(std::string *result) const {
  if (input.empty()) {
    result->append("()");
  } else if (input.size() == 1 && !input[0]->is<Function>()) {
    input.at(0)->WriteTo(result);
  } else {
    result->append("(");
    input.at(0)->WriteTo(result);
    for (size_t i = 1; i < input.size(); ++i) {
      result->append(", ");
      input.at(i)->WriteTo(result);
    }
    result->append(")");
  }

  result->append(" -> ");

  if (output.empty()) {
    result->append("()");
  } else if (output.size() == 1) {
    output.at(0)->WriteTo(result);
  } else {
    result->append("(");
    output.at(0)->WriteTo(result);
    for (size_t i = 1; i < output.size(); ++i) {
      result->append(", ");
      output.at(i)->WriteTo(result);
    }
    result->append(")");
  }
}

core::Bytes Function::bytes(core::Arch const &a) const {
  return a.fn_ptr_bytes;
}

core::Alignment Function::alignment(core::Arch const &a) const {
  return a.fn_ptr_alignment;
}

core::FnParams<type::Typed<ast::Expression const *>> Function::AnonymousFnParams()
    const {
  core::FnParams<type::Typed<ast::Expression const*>> result;
  for (type::Type const *t : input) {
    result.append("", type::Typed<ast::Expression const *>(nullptr, t));
  }
  return result;
}

}  // namespace type
