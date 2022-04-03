#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/declaration.h"
#include "ast/module.h"
#include "ast/scope.h"
#include "base/cast.h"
#include "base/guarded.h"
#include "base/macros.h"
#include "type/qual_type.h"

namespace module {

enum class Linkage { Internal, External };

// Module:
//
// Represents a unit of compilation, beyond which all intercommunication must be
// explicit.
struct Module : base::Cast<Module> {
  explicit Module(std::string identifier)
      : identifier_(std::move(identifier)) {}
  virtual ~Module() {}

  struct SymbolInformation {
    type::QualType qualified_type;
    ir::CompleteResultBuffer value;
    // TODO: Remove this. It's only here as a temporary mechanism to work nicely
    // with generics until we have a decent cross-module solution for them. It
    // will only be populated for generics.
    ast::Declaration::Id const *id;
  };

  // Returns an identifier for this module unique across all modules being
  // linked together.
  std::string_view identifier() const { return identifier_; }

  // Given a symbol `name`, returns a range of `SymbolInformation` describing
  // any exported symbols of that name in the module. The range of symbol
  // information has no ordering guarantees.
  virtual absl::Span<SymbolInformation const> Exported(
      std::string_view name) const = 0;

 private:
  std::string identifier_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
