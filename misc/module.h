#ifndef ICARUS_MODULE_H
#define ICARUS_MODULE_H

#include <filesystem>
#include <map>
#include <memory>
#include <mutex>
#include <queue>
#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "core/pending_module.h"
#include "core/scope.h"
#include "error/log.h"

namespace type {
struct Type;
}  // namespace type

namespace ast {
struct StructLiteral;
}  // namespace ast

struct Module {
  explicit Module();
  ~Module();

  // We take pointers to the module, so it cannot be moved.
  Module(Module &&) = delete;

  ast::Declaration *GetDecl(std::string_view name) const;

  error::Log error_log_;

  core::ModuleScope scope_;

  // TODO long-term this is not a good way to store these. We should probably
  // extract the declarations determine which are public, etc.
  std::vector<std::unique_ptr<ast::Node>> statements_;

  // TODO support more than just a single type argument to generic structs.
  struct GenericStructCache {
    std::map<std::vector<type::Type const *>, type::Type const *> fwd_;
    absl::flat_hash_map<type::Type const *,
      std::vector<type::Type const *> const *>
        back_;
  };

  absl::flat_hash_map<ast::StructLiteral const *, GenericStructCache>
    generic_struct_cache_;

  std::filesystem::path const *path_ = nullptr;
};

Module *CompileModule(Module *mod, std::filesystem::path const *path);

#endif  // ICARUS_MODULE_H
