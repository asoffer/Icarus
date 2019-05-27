ICARUS_CORE_BUILTIN_X(Foreign,   "foreign",    type::Generic)
ICARUS_CORE_BUILTIN_X(Opaque,    "opaque",     type::Func({}, {type::Type_}))
ICARUS_CORE_BUILTIN_X(Bytes,     "bytes",      type::Func({type::Type_}, {type::Int64}))
ICARUS_CORE_BUILTIN_X(Alignment, "alignment",  type::Func({type::Type_}, {type::Int64}))
#ifdef DBG
ICARUS_CORE_BUILTIN_X(DebugIr,   "debug_ir",   type::Func({}, {}))
#endif  // DBG
