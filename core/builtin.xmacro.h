ICARUS_CORE_BUILTIN_X(Foreign, "foreign", type::Generic)
ICARUS_CORE_BUILTIN_X(Opaque, "opaque", type::Func({}, {type::Type_}))
ICARUS_CORE_BUILTIN_X(Bytes, "bytes",
                      type::Func({core::AnonymousParam(type::Type_)},
                                 {type::Int64}))
ICARUS_CORE_BUILTIN_X(Alignment, "alignment",
                      type::Func({core::AnonymousParam(type::Type_)},
                                 {type::Int64}))
#if defined(ICARUS_DEBUG)
ICARUS_CORE_BUILTIN_X(DebugIr, "debug_ir", type::Func({}, {}))
#endif  // defined(ICARUS_DEBUG)
