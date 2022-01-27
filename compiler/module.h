#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include "compiler/context.h"
#include "module/module.h"

namespace compiler {

struct CompiledModule : module::Module {
  explicit CompiledModule(frontend::SourceBuffer const *buffer,
                          Context *context)
      : context_(ASSERT_NOT_NULL(context)),
        buffer_(ASSERT_NOT_NULL(buffer)),
        module_(this) {
    context_->set_qt_callback(
        [&](ast::Declaration::Id const *id, type::QualType qt) {
          if (id->declaration().hashtags.contains(ir::Hashtag::Export)) {
            auto &entries = exported_[id->name()];
            indices_.emplace(id, entries.size());
            entries.push_back(
                Module::SymbolInformation{.qualified_type = qt, .id = id});
          }
        });

    context_->set_value_callback(
        [&](ast::Declaration::Id const *id, ir::CompleteResultBuffer buffer) {
          if (id->declaration().hashtags.contains(ir::Hashtag::Export)) {
            auto iter = indices_.find(id);
            if (iter == indices_.end()) { return; }
            exported_[id->name()][iter->second].value = std::move(buffer);
          }
        });
  }

  Context const &context() const { return *context_; }
  Context &context() { return *context_; }

  template <std::input_iterator Iter>
  base::PtrSpan<ast::Node const> insert(Iter b, Iter e) {
    return module_.insert(b, e);
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

  ast::Scope const &scope() const { return module_.body_scope(); }
  ast::Scope &scope() { return module_.body_scope(); }

 private:
  Context *context_;
  frontend::SourceBuffer const *buffer_;

  absl::flat_hash_map<std::string_view, std::vector<SymbolInformation>>
      exported_;
  absl::flat_hash_map<ast::Declaration::Id const *, size_t> indices_;
  // This flag should be set to true if this module is ever found to depend on
  // another which has errors, even if those errors do not effect
  // code-generation in this module.
  //
  // TODO: As we move towards separate compilation in separate processes, this
  // will become irrelevant.
  bool depends_on_module_with_errors_ = false;

  ast::Module module_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
