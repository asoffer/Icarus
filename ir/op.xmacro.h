OP_MACRO(Death,                Death,               void,                    empty_)
OP_MACRO(Bytes,                Bytes,               type::Type const*,       type_arg_)
OP_MACRO(Align,                Align,               type::Type const*,       type_arg_)
OP_MACRO(CreateStruct,         CreateStruct,        core::Scope const *,     create_struct_)
OP_MACRO(CreateStructField,    CreateStructField,   void,                    create_struct_field_)
OP_MACRO(SetStructFieldName,   SetStructFieldName,  void,                    set_struct_field_name_)
OP_MACRO(AddHashtagToField,    AddHashtagToField,   void,                    add_hashtag_)
OP_MACRO(AddHashtagToStruct,   AddHashtagToStruct,  void,                    add_hashtag_)
OP_MACRO(FinalizeStruct,       Finalize,            type::Struct const*,     reg_)
OP_MACRO(CreateEnum,           CreateEnum,          void,                    mod_)
OP_MACRO(AddEnumerator,        AddEnumerator,       void,                    add_enumerator_)
OP_MACRO(SetEnumerator,        SetEnumerator,       void,                    set_enumerator_)
OP_MACRO(FinalizeEnum,         FinalizeEnum,        void,                    reg_)
OP_MACRO(CreateFlags,          CreateFlags,         void,                    mod_)
OP_MACRO(AddFlag,              AddFlags,            void,                    add_enumerator_)
OP_MACRO(SetFlag,              SetFlags,            void,                    set_enumerator_)
OP_MACRO(FinalizeFlags,        FinalizeFlags,       void,                    reg_)
OP_MACRO(DebugIr,              DebugIr,             void,                    empty_)
OP_MACRO(PtrIncr,              PtrIncr,             void,                    ptr_incr_)
OP_MACRO(Field,                Field,               void,                    field_)
OP_MACRO(Array,                Array,               void,                    array_)
OP_MACRO(JumpPlaceholder,      Jump,                void,                    block_def_)
OP_MACRO(VariantType,          VariantType,         void,                    addr_arg_)
OP_MACRO(VariantValue,         VariantValue,        void,                    addr_arg_)
OP_MACRO(Call,                 Call,                void,                    call_)
OP_MACRO(PhiBool,              Phi,                 bool,                    phi_bool_)
OP_MACRO(PhiInt8,              Phi,                 int8_t,                  phi_i8_)
OP_MACRO(PhiInt16,             Phi,                 int16_t,                 phi_i16_)
OP_MACRO(PhiInt32,             Phi,                 int32_t,                 phi_i32_)
OP_MACRO(PhiInt64,             Phi,                 int64_t,                 phi_i64_)
OP_MACRO(PhiNat8,              Phi,                 uint8_t,                 phi_u8_)
OP_MACRO(PhiNat16,             Phi,                 uint16_t,                phi_u16_)
OP_MACRO(PhiNat32,             Phi,                 uint32_t,                phi_u32_)
OP_MACRO(PhiNat64,             Phi,                 uint64_t,                phi_u64_)
OP_MACRO(PhiFloat32,           Phi,                 float,                   phi_float32_)
OP_MACRO(PhiFloat64,           Phi,                 double,                  phi_float64_)
OP_MACRO(PhiType,              Phi,                 type::Type const*,       phi_type_)
OP_MACRO(PhiBlock,             Phi,                 BlockDef *,              phi_block_)
OP_MACRO(PhiAddr,              Phi,                 ir::Addr,                phi_addr_)
OP_MACRO(PhiEnum,              Phi,                 ir::EnumVal,             phi_enum_)
OP_MACRO(PhiFlags,             Phi,                 ir::FlagsVal,            phi_flags_)
OP_MACRO(PhiFunc,              Phi,                 ir::AnyFunc,             phi_func_)
OP_MACRO(GetRet,               GetRet,              void,                    get_ret_)
OP_MACRO(SetRetBool,           SetRet,              bool,                    set_ret_bool_)
OP_MACRO(SetRetInt8,           SetRet,              int8_t,                  set_ret_i8_)
OP_MACRO(SetRetInt16,          SetRet,              int16_t,                 set_ret_i16_)
OP_MACRO(SetRetInt32,          SetRet,              int32_t,                 set_ret_i32_)
OP_MACRO(SetRetInt64,          SetRet,              int64_t,                 set_ret_i64_)
OP_MACRO(SetRetNat8,           SetRet,              uint8_t,                 set_ret_u8_)
OP_MACRO(SetRetNat16,          SetRet,              uint16_t,                set_ret_u16_)
OP_MACRO(SetRetNat32,          SetRet,              uint32_t,                set_ret_u32_)
OP_MACRO(SetRetNat64,          SetRet,              uint64_t,                set_ret_u64_)
OP_MACRO(SetRetFloat32,        SetRet,              float,                   set_ret_float32_)
OP_MACRO(SetRetFloat64,        SetRet,              double,                  set_ret_float64_)
OP_MACRO(SetRetType,           SetRet,              type::Type const*,       set_ret_type_)
OP_MACRO(SetRetEnum,           SetRet,              EnumVal,                 set_ret_enum_)
OP_MACRO(SetRetFlags,          SetRet,              FlagsVal,                set_ret_flags_)
OP_MACRO(SetRetByteView,       SetRet,              std::string_view,        set_ret_byte_view_)
OP_MACRO(SetRetAddr,           SetRet,              ir::Addr,                set_ret_addr_)
OP_MACRO(SetRetFunc,           SetRet,              AnyFunc,                 set_ret_func_)
OP_MACRO(SetRetScope,          SetRet,              ir::ScopeDef*,           set_ret_scope_)
OP_MACRO(SetRetGeneric,        SetRet,              ast::FunctionLiteral  *, set_ret_generic_)
OP_MACRO(SetRetModule,         SetRet,              Module *,                set_ret_module_)
OP_MACRO(SetRetBlock,          SetRet,              BlockDef *,              set_ret_block_)
OP_MACRO(ArgumentCache,        ArgumentCache,       ast::StructLiteral *,    sl_)
OP_MACRO(NewOpaqueType,        NewOpaqueType,       void,                    reg_)
OP_MACRO(LoadSymbol,           LoadSymbol,          void,                    load_sym_)
OP_MACRO(Init,                 Init,                void,                    special1_)
OP_MACRO(Destroy,              Destroy,             void,                    special1_)
OP_MACRO(Move,                 Move,                void,                    special2_)
OP_MACRO(Copy,                 Copy,                void,                    special2_)
OP_MACRO(VerifyType,           VerifyType,          void,                    ast_)
OP_MACRO(EvaluateAsType,       EvaluateAsType,      type::Type const*,       ast_)
OP_MACRO(CreateContext,        CreateContext,       Context *,               mod_)
OP_MACRO(AddBoundConstant,     AddBoundConstant,    void,                    add_bc_)
OP_MACRO(DestroyContext,       DestroyContext,      void,                    reg_)
OP_MACRO(CreateScopeDef,       CreateScopeDef,      void,                    create_scope_def_)
OP_MACRO(AddScopeDefInit,      AddScopeDefInit,     void,                    add_scope_def_init_)
OP_MACRO(AddScopeDefDone,      AddScopeDefDone,     void,                    add_scope_def_done_)
OP_MACRO(FinishScopeDef,       FinishScopeDef,      void,                    empty_)
OP_MACRO(CreateBlockDef,       CreateBlockDef,      void,                    empty_)
OP_MACRO(FinishBlockDef,       FinishBlockDef,      void,                    byte_view_arg_)
OP_MACRO(AddBlockDefBefore,    AddBlockDefBefore,   void,                    any_fn_)
OP_MACRO(AddBlockDefAfter,     AddBlockDefAfter,    void,                    any_fn_)
