#include "compiler/dispatch/table.h"

#include "ast/expression.h"
#include "ast/methods/dump.h"
#include "base/debug.h"
#include "compiler/dispatch/extract_params.h"
#include "core/fn_params.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/variant.h"

namespace compiler {
namespace {
template <typename IndexT>
void AddType(IndexT &&index, type::Type const *t,
             std::vector<core::FnArgs<type::Type const *>> *args) {
  if (auto *vt = t->if_as<type::Variant>()) {
    std::vector<core::FnArgs<type::Type const *>> new_args;
    for (auto *v : vt->variants_) {
      for (auto fnargs : *args) {
        if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
          fnargs.pos_emplace(v);
        } else {
          fnargs.named_emplace(index, v);
        }
        new_args.push_back(std::move(fnargs));
      }
    }
    *args = std::move(new_args);
  } else {
    std::for_each(
        args->begin(), args->end(),
        [&](core::FnArgs<type::Type const *> &fnargs) {
          if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
            fnargs.pos_emplace(t);
          } else {
            fnargs.named_emplace(index, t);
          }
        });
  }
}

// TODO: Ideally we wouldn't create these all at once but rather iterate through
// the possibilities. Doing this the right way involves having sum and product
// iterators.
std::vector<core::FnArgs<type::Type const *>> ExpandedFnArgs(
    core::FnArgs<VerifyResult> const &fnargs) {
  std::vector<core::FnArgs<type::Type const *>> all_expanded_options(1);
  fnargs.ApplyWithIndex([&](auto &&index, compiler::VerifyResult r) {
    // TODO also maybe need the expression this came from to see if it needs
    // to be expanded.
    AddType(index, r.type(), &all_expanded_options);
  });

  return all_expanded_options;
}

}  // namespace

base::expected<DispatchTable> DispatchTable::Verify(
    Compiler *compiler, ast::OverloadSet const &os,
    core::FnArgs<VerifyResult> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", os.members().size(), " members.");

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ast::Expression const *, FailedMatch> failures;
  DispatchTable table;
  for (ast::Expression const *overload : os.members()) {
    // TODO the type of the specific overload could *correctly* be null and we
    // need to handle that case.
    DEBUG_LOG("dispatch-verify")
    ("Verifying ", overload, ": ", ast::Dump::ToString(overload));
    auto result = MatchArgsToParams(ExtractParams(compiler, overload), args);
    if (not result) {
      failures.emplace(overload, result.error());
    } else {
      table.table_.emplace(overload, *result);
    }
  }

  auto expanded_fnargs = ExpandedFnArgs(args);

  expanded_fnargs.erase(
      std::remove_if(expanded_fnargs.begin(), expanded_fnargs.end(),
                     [&](core::FnArgs<type::Type const *> const &fnargs) {
                       return absl::c_any_of(
                           table.table_, [&fnargs](auto const &expr_params) {
                             return core::IsCallable(expr_params.second, fnargs,
                                                     [](type::Type const *from,
                                                        type::Type const *to) {
                                                       return type::CanCast(
                                                           from, to);
                                                     });
                           });
                     }),
      expanded_fnargs.end());

  if (not expanded_fnargs.empty()) { NOT_YET("log an error"); }
  return table;
}

}  // namespace compiler
