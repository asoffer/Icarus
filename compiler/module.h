#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include "compiler/context.h"
#include "module/module.h"

namespace compiler {

struct CompiledModule : module::BasicModule {
  explicit CompiledModule(frontend::SourceBuffer const *buffer,
                          Context *context = nullptr)
      : context_(ASSERT_NOT_NULL(context)), buffer_(ASSERT_NOT_NULL(buffer)) {
    context_->set_callback(
        [&](ast::Declaration::Id const *id, type::QualType qt) {
          exported_[id->name()].push_back(
              Module::SymbolInformation{.qualified_type = qt});
        });
  }

  Context const &context() const { return *context_; }
  Context &context() { return *context_; }

  template <std::input_iterator Iter>
  base::PtrSpan<ast::Node const> insert(Iter b, Iter e) {
    auto ptr_span = module_.insert(b, e);

    for (auto const *node : ptr_span) {
      if (auto *decl = node->template if_as<ast::Declaration>();
          decl and decl->hashtags.contains(ir::Hashtag::Export)) {
        for (auto const &id : decl->ids()) {
          exported_declarations_[id.name()].push_back(&id);
        }
      }
    }

    return ptr_span;
  }

  absl::Span<Module::SymbolInformation const> Exported(std::string_view name) {
    auto iter = exported_.find(name);
    if (iter == exported_.end()) { return {}; }

    // TODO: handle exported embedded modules here too.
    return iter->second;
  }

  frontend::SourceBuffer const &buffer() const {
    return *ASSERT_NOT_NULL(buffer_);
  }

  bool has_error_in_dependent_module() const {
    return depends_on_module_with_errors_;
  }
  void set_dependent_module_with_errors() {
    depends_on_module_with_errors_ = true;
  }

 private:
  Context *context_;
  frontend::SourceBuffer const *buffer_;

  absl::flat_hash_map<std::string_view, std::vector<SymbolInformation>>
      exported_;

  // This flag should be set to true if this module is ever found to depend on
  // another which has errors, even if those errors do not effect
  // code-generation in this module.
  //
  // TODO: As we move towards separate compilation in separate processes, this
  // will become irrelevant.
  bool depends_on_module_with_errors_ = false;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
