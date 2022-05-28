#include "compiler/module.h"

namespace compiler {

CompiledModule::CompiledModule(std::string identifier, ir::ModuleId id)
    : Module(std::move(identifier)),
      ir_module_(id),
      context_(&ir_module_),
      module_(this),
      id_(id) {
  context_.set_qt_callback([&](ast::Declaration::Id const *id,
                               type::QualType qt) {
    auto &entries = exported_[id->name()];
    indices_.emplace(id, entries.size());
    entries.push_back(Module::SymbolInformation{
        .qualified_type = qt,
        .id             = id,
        .visibility = id->declaration().hashtags.contains(ir::Hashtag::Export)
                          ? Visibility::Exported
                          : Visibility::Private,
    });
  });

  context_.set_value_callback(
      [&](ast::Declaration::Id const *id, ir::CompleteResultBuffer buffer) {
        if (id->declaration().hashtags.contains(ir::Hashtag::Export)) {
          auto iter = indices_.find(id);
          if (iter == indices_.end()) { return; }
          exported_[id->name()][iter->second].value = std::move(buffer);
        }
      });
}

precompiled::ModuleProto CompiledModule::ToProto(
    base::flyweight_set<base::MetaValue> const &instruction_ids) const {
  precompiled::ModuleProto proto;
  proto.set_identifier(std::string(identifier()));
  for (auto const &f : ir_module_.functions()) {
    *proto.add_subroutines() = f.subroutine.ToProto(instruction_ids);
  }
  auto &symbols = *proto.mutable_symbols();
  for (auto const &[name, infos] : exported_) {
    auto range = Exported(name);
    if (range.empty()) { continue; }
    auto &named_symbol = symbols[std::string(name)];
    for (auto const &info : range) {
      *named_symbol.add_symbol() =
          precompiled::ToProto(type::GlobalTypeSystem, info);
    }
  }

  *proto.mutable_type_system() = precompiled::ToProto(type::GlobalTypeSystem);

  return proto;
}

}  // namespace compiler
