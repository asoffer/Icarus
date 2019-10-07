ICARUS_TYPE_VISITOR(
    void ExtractDefiningModules(
        absl::flat_hash_set<module::Module const *> *modules) const,
    { return module::ExtractDefiningModules::Extract(this, modules); });
