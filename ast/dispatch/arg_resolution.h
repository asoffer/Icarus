#ifndef ICARUS_AST_DISPATCH_ARG_RESOLUTION_H
#define ICARUS_AST_DISPATCH_ARG_RESOLUTION_H

#include <vector>

#include "ast/dispatch/call_obstruction.h"
#include "ast/expression.h"
#include "ast/fn_args.h"
#include "ast/fn_params.h"
#include "base/expected.h"
#include "base/util.h"
#include "type/tuple.h"
#include "type/typed_value.h"

namespace ast {
struct ArgResolution;
std::ostream &operator<<(std::ostream &os, ArgResolution const &res);


// Because expression representing tuples may be expanded, we need a way to map
// arguments onto potentially a different number of function parameters. This
// struct refers to external expressions and allows indexing into them.
struct ArgResolution {
  struct Entry {
    Entry() = default;
    Entry(Expression *expr, int argument_index, int expansion_index,
          size_t parameter_index)
        : expr(expr),
          argument_index(argument_index),
          expansion_index(expansion_index),
          parameter_index(parameter_index) {}

    constexpr bool defaulted() const { return expr == nullptr; }

    Expression *expr   = nullptr;
    int argument_index = -1;  // Positive numbers indicate positional arguments.
                              // -1 indicates named argument.
    int expansion_index = -1;     // Positive numbers indicate the index of an
                                  // expansion. -1 indicates no expansion
                                  // necessary.
    size_t parameter_index = -1;  // Which parameter this gets associated with.
    type::Type const *type = nullptr;
  };

  template <typename T>
  static base::expected<ArgResolution, CallObstruction> Make(
      FnParams<T> const &params,
      FnArgs<type::Typed<Expression *>> const &args) {
    ArgResolution res;
    auto p_iter = params.begin();
    auto a_iter = args.pos_.begin();

    size_t argument_index  = 0;
    size_t parameter_index = 0;
    while (p_iter != params.end() && a_iter != args.pos_.end()) {
      auto *t = a_iter->type();
      if (a_iter->get()->needs_expansion()) {
        auto const &tuple_entries = t->template as<type::Tuple>().entries_;

        // TODO check that there is enough space in params
        size_t expansion_index = 0;
        for (auto *t : tuple_entries) {
          res.entries_.emplace_back(a_iter->get(), argument_index, expansion_index,
                                parameter_index);
          ++p_iter;
          ++parameter_index;
          ++expansion_index;
        }
        ++argument_index;
        ++a_iter;
      } else {
        res.entries_.emplace_back(a_iter->get(), argument_index, -1,
                                  parameter_index);
        ++parameter_index;
        ++argument_index;
        ++p_iter;
        ++a_iter;
      }
    }

    // Fill all named entries with defaults. In a moment we'll overwrite them
    // with the named arguments that we have available.
    size_t num_positional = res.entries_.size();
    size_t total_size =
        num_positional + params.lookup_.size() - parameter_index;
    res.entries_.reserve(total_size);
    while (res.entries_.size() < total_size) {
      res.entries_.emplace_back(nullptr, -1, -1, parameter_index++);
    }

    // TODO this assumes we aren't splatting into a named argument, which might
    // make sense for named variadics.
    for (auto const &[name, expr] : args.named_) {
      if (auto iter = params.lookup_.find(name); iter != params.lookup_.end()) {
        auto &entry           = res.entries_.at(iter->second);
        entry.expr            = expr.get();
        entry.parameter_index = iter->second;
      } else {
        return CallObstruction::NoParameterNamed(name);
      }
    }

    return res;
  }

  template <typename E>
  CallObstruction SetTypes(std::vector<type::Type const *> const &input_types,
                           FnParams<E> const &params, Context *ctx,   FnArgs<type::Type const *> *call_arg_types) {
    for (auto &entry : entries_) {
      type::Type const *input_type;
      if constexpr (std::is_same_v<E, std::unique_ptr<Declaration>>) {
        Declaration &decl = *params.at(entry.parameter_index).value;
        if (entry.defaulted()) {
          // The naming here is super confusing but if a declaration
          // "IsDefaultInitialized" that means it's of the form `foo: bar`, and
          // so it does NOT have a default value.
          ASSIGN_OR(return _.error(), input_type, TypeFromDefaultDecl(decl, ctx));
        } else {
          input_type = TypeFromDecl(decl, ctx);
        }
      } else if constexpr (std::is_same_v<E, Expression *> ||
                           std::is_same_v<E, std::nullptr_t>) {
        input_type        = input_types.at(entry.parameter_index);
        auto const &param = params.at(entry.parameter_index);
        if (entry.defaulted() && param.value == nullptr) {
          return CallObstruction::NoDefault(param.name);
        }

      } else {
        static_assert(base::always_false<E>());
      }

      if (!entry.defaulted()) {
        // TODO for constant-parameters ordered processing won't work.
        // TODO variadics there may be more than one of these.
        ASSIGN_OR(return _.error(), auto *t, MeetWithBoundType(entry, input_type, ctx));

        if (entry.parameter_index < call_arg_types->pos_.size()) {
          call_arg_types->pos_.at(entry.parameter_index) = t;
        } else {
          auto iter =
              call_arg_types->find(params.at(entry.parameter_index).name);
          ASSERT(iter != call_arg_types->named_.end());
          iter->second = t;
        }
      }

      // Loop because the same parameter can show up multiple times with variadics.
      for (auto &e : entries_) {
        if (e.parameter_index == entry.parameter_index) {
          e.type = ASSERT_NOT_NULL(input_type);
        }
      }
    }
    return CallObstruction::None();
  }

  ir::Results Results(
      FnParams<Expression *> *const_params,
      std::unordered_map<Expression *, ir::Results const *> const &expr_map,
      Context *ctx) const;

  size_t num_positional_arguments() const {
    size_t num = 0;
    for (auto const &entry : entries_) {
      if (entry.argument_index != -1) { ++num; }
    }
    return num;
  }

// private:
  base::expected<type::Type const *, CallObstruction> TypeFromDefaultDecl(
      Declaration &decl, Context *ctx);

  // This is simply a call to ctx->type_of, but We can't do it in the header
  // because Context is incomplete.
  type::Type const *TypeFromDecl(Declaration const &decl, Context *ctx);

  base::expected<type::Type const *, CallObstruction> MeetWithBoundType(
      Entry const &e, type::Type const *input_type, Context *ctx);

  friend std::ostream &operator<<(std::ostream &os, ArgResolution const &res);
  std::vector<Entry> entries_;
};

}  // namespace ast

#endif  // ICARUS_AST_DISPATCH_ARG_RESOLUTION_H
