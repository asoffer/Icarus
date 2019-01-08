// TODO Combine these if they're on the same line.
// TODO if you stop calling it a string-literal, these error messages will have
// to change too.
MAKE_LOG_ERROR(InvalidEscapedCharacterInStringLiteral,
               "Found an invalid escape sequence in string-literal.")
MAKE_LOG_ERROR(RunawayStringLiteral,
               "Reached end of line before finding the "
               "end of a string-literal. Did you forget "
               "a quotation mark?")
MAKE_LOG_ERROR(EscapedDoubleQuoteInCharacterLiteral,
               "The double quotation mark character (\") does not need to be "
               "esacped in a character-literal.")
MAKE_LOG_ERROR(InvalidEscapedCharacterInCharacterLiteral,
               "Encounterd an invalid escape sequence in character-literal. "
               "The valid escape sequences in a character-literal are \\\\, "
               "\\a, \\b, \\f, \\n, \\r, \\s, \\t, and \\v.")
MAKE_LOG_ERROR(RunawayCharacterLiteral,
               "Found a backtick (`), but did not see a character literal.")
MAKE_LOG_ERROR(SpaceInCharacterLiteral,
               "Found a backtick (`) followed by a space character. Space "
               "character literals are written as `\\s.")
MAKE_LOG_ERROR(TabInCharacterLiteral,
               "Found a tab in your character-literal. Tab charcater literals "
               "are written as `\t.")
MAKE_LOG_ERROR(TooManyDots,
               "There are too many consecutive period (.) characters.")

MAKE_LOG_ERROR(InvalidCharacterQuestionMark,
               "Question mark characters are not valid syntax by themselves. "
               "Did you mean \":?\"?")
MAKE_LOG_ERROR(
    NonWhitespaceAfterNewlineEscape,
    "Found a non-whitespace character following a line-continuation ('\\').")
MAKE_LOG_ERROR(
    NotInMultilineComment,
    "Found a token representing the end of a "
    "multi-line comment (*/), but it was not part of a comment block.")

// TODO handle case where a value is repeated multiple times. build a set and
// highlight them accordingly.
MAKE_LOG_ERROR(RepeatedEnumName, "Repeated enum member.")
MAKE_LOG_ERROR(EnumNeedsIdsOrConstDecls,
               "Enum members must be identifiers or constant declarations.")
MAKE_LOG_ERROR(CallingDeclaration, "Declarations cannot be called.")
MAKE_LOG_ERROR(IndexingDeclaration, "Declaration cannot be indexed")
MAKE_LOG_ERROR(DeclarationInIndex, "Declarations cannot appear inside an index")
MAKE_LOG_ERROR(NonDeclarationInStructDeclaration,
               "Each struct member must be defined using a declaration.")
MAKE_LOG_ERROR(CommaListStatement,
               "Comma-separated lists are not allowed as statements")
MAKE_LOG_ERROR(InvalidImport,
               "Import statements must take a constant string as the name of "
               "the file to be imported.")
MAKE_LOG_ERROR(RHSNonIdInAccess, "Right-hand side must be an identifier")
MAKE_LOG_ERROR(DeclarationInAccess,
               "Declaration not allowed on left-hand side of dot (.) operator.")
MAKE_LOG_ERROR(UninferrableType,
               "Unable to infer the type of the following expression:")

MAKE_LOG_ERROR(InferringHole,
               "Attempting to infer the type of an uninitialized value")
MAKE_LOG_ERROR(UninitializedConstant,
               "Attempting to define a constant with an uninitialized value.")
MAKE_LOG_ERROR(NonComposableFunctions, "Functions cannot be composed.")
MAKE_LOG_ERROR(NonTypeFunctionInput,
               "The specified input type for a function must be a type.")
MAKE_LOG_ERROR(NonTypeFunctionOutput,
               "The specified return type for a function must be a type.")
MAKE_LOG_ERROR(InconsistentArrayType,
               "Type error: Array literal must have consistent type")
MAKE_LOG_ERROR(ArrayIndexType, "Array length indexed by non-integral type")
MAKE_LOG_ERROR(JumpOutsideLoop, "statement must be contained inside a loop.")
MAKE_LOG_ERROR(IndeterminantType, "Cannot determine type from in declaration.")
MAKE_LOG_ERROR(XorEqNeedsBoolOrFlags,
               "Operator '^=' must take boolean or flags arguments.")
MAKE_LOG_ERROR(AndEqNeedsBoolOrFlags,
               "Operator '&=' must take boolean or flags arguments.")
MAKE_LOG_ERROR(OrEqNeedsBoolOrFlags,
               "Operator '|=' must take boolean or flags arguments.")
MAKE_LOG_ERROR(PrintingVoid, "Attempting to print an object of type void")
MAKE_LOG_ERROR(EarlyRequiredBlock,
               "Required block appears on the left-hand side of `|` operator.")

MAKE_LOG_ERROR(UnspecifiedOverload,
               "Attempting to access an overloaded function by name.")
// TODO this error message is truly terrible.
MAKE_LOG_ERROR(StatefulScopeWithoutStateArg,
               "Stateful scope has no handlers whose first argument is a "
               "pointer (to the state object). Did you mean to make this "
               "stateless?")
MAKE_LOG_ERROR(ArrayDataTypeNotAType,
               "Array type has underlying data type specified as a value which "
               "is not a type.")
MAKE_LOG_ERROR(NoExportedSymbol,
               "No exported symbol of given name in this module.")
MAKE_LOG_ERROR(TypeHasNoMembers, "Cannot access a member of a type.")
MAKE_LOG_ERROR(NonConstTypeMemberAccess,
               "Cannot access a member of a non-constant type.")
MAKE_LOG_ERROR(CastToNonConstantType,
               "Cannot cast to a type which is not declared constant.")
MAKE_LOG_ERROR(CastToNonType, "Cannot cast to a non-type.")
