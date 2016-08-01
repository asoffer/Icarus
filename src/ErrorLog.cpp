#include "ErrorLog.h"
#include "util/pstr.h"
#include "SourceFile.h"
#include "AST/AST.h"
#include "Type/Type.h"

extern std::map<std::string, SourceFile *> source_map;

typedef size_t LineNum;
typedef size_t LineOffset;
typedef const char *FileName;
typedef std::string Token;

typedef std::map<Token,
                 std::map<FileName, std::map<LineNum, std::vector<LineOffset>>>>
    TokenToErrorMap;
typedef std::map<FileName, std::vector<LineNum>> FileToLineNumMap;
typedef std::map<const AST::Declaration *,
                 std::map<FileName, std::map<LineNum, std::vector<LineOffset>>>>
    DeclToErrorMap;
typedef std::map<Cursor, std::map<LineNum, std::vector<LineOffset>>>
    LocToErrorMap;

static TokenToErrorMap undeclared_identifiers, ambiguous_identifiers;
static DeclToErrorMap invalid_capture;
static LocToErrorMap case_errors;
static FileToLineNumMap global_non_decl;

std::map<std::string, std::map<size_t, std::vector<std::string>>>
    Error::Log::log_;

size_t Error::Log::num_errs_   = 0;
bool Error::Log::ImmediateMode = false;

static inline size_t NumDigits(size_t n) {
  if (n == 0) return 1;
  size_t counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

static inline std::string NumTimes(size_t n, const char *numeric_prefix, bool capitalize) {
  if (n == 1) { return capitalize ? "Once" : "once"; }
  if (n == 2) { return capitalize ? "Twice" : "twice"; }
  if (numeric_prefix) {
  return numeric_prefix + std::to_string(n) + " times";
  } else {
  return std::to_string(n) + " times";
  }
}

static void GatherAndDisplay(const char *fmt, const TokenToErrorMap &log) {
  for (const auto &kv : log) {
    const char *token   = kv.first.c_str();
    size_t token_length = kv.first.size();

    size_t num_uses = 0;
    for (const auto &file_and_locs : kv.second) {
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses += line_and_offsets.second.size();
      }
    }

    fprintf(stderr, fmt, token);
    if (num_uses == 1) {
      fprintf(stderr, ".\n");
    } else {
      fprintf(stderr, " used %s.\n", NumTimes(num_uses, nullptr, false).c_str());
    }

    for (const auto &file_and_locs : kv.second) {
      size_t max_line_num = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        if (max_line_num < line_and_offsets.first) {
          max_line_num = line_and_offsets.first;
        }
      }
      size_t line_num_width = NumDigits(max_line_num);

      size_t num_uses_in_file = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses_in_file += line_and_offsets.second.size();
      }

      fprintf(stderr, "  %s in '%s':\n", NumTimes(num_uses_in_file, "Used ", true).c_str(), file_and_locs.first);

      for (const auto &line_and_offsets : file_and_locs.second) {
        pstr line = source_map AT(file_and_locs.first)
                        ->lines AT(line_and_offsets.first);

        size_t left_border_width = line_num_width + 6;
        size_t line_length       = strlen(line) + 1;
        char *underline          = new char[left_border_width + line_length + 1];
        underline[line_length + left_border_width] = '\0';
        memset(underline, ' ', left_border_width + line_length);

        for (const auto offset : line_and_offsets.second) {
          memset(underline + left_border_width + offset, '^', token_length);
        }

        fprintf(stderr, "    %*lu| %s\n"
                        "%s\n",
                (int)line_num_width, line_and_offsets.first, line.ptr,
                underline);
        delete[] underline;
      }
    }
  }
}

static void GatherAndDisplay(const char *fmt_head, const DeclToErrorMap &log) {
  for (const auto &kv : log) {
    const char *token   = kv.first->identifier->token.c_str();
    size_t token_length = kv.first->identifier->token.size();

    size_t num_uses = 0;
    for (const auto &file_and_locs : kv.second) {
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses += line_and_offsets.second.size();
      }
    }

    fprintf(stderr, fmt_head, token);
    if (num_uses == 1) {
      fprintf(stderr, ".\n");
    } else {
      fprintf(stderr, " used %s.\n", NumTimes(num_uses, nullptr, false).c_str());
    }

    for (const auto &file_and_locs : kv.second) {
      size_t num_uses_in_file = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        num_uses_in_file += line_and_offsets.second.size();
      }

      fprintf(stderr, "  %s in '%s':\n", NumTimes(num_uses_in_file, "Used ", true).c_str(), file_and_locs.first);

      size_t max_line_num = 0;
      for (const auto &line_and_offsets : file_and_locs.second) {
        if (max_line_num < line_and_offsets.first) { max_line_num = line_and_offsets.first; }
      }
      size_t line_num_width = NumDigits(max_line_num);

      for (const auto &line_and_offsets : file_and_locs.second) {
        pstr line = source_map AT(file_and_locs.first)
                        ->lines AT(line_and_offsets.first);

        size_t left_border_width                   = line_num_width + 4;
        size_t line_length                         = strlen(line) + 1;
        char *underline                            = new char[left_border_width + line_length + 1];
        underline[line_length + left_border_width] = '\0';
        memset(underline, ' ', left_border_width + line_length);

        for (const auto offset : line_and_offsets.second) {
          memset(underline + left_border_width + offset, '^', token_length);
        }

        fprintf(stderr, "    %*lu| %s\n"
                        "%s\n",
                (int)line_num_width, line_and_offsets.first, line.ptr, underline);
        delete[] underline;
      }
    }
  }
}

static void GatherAndDisplay(const char *fmt, const FileToLineNumMap &log) {
  if (global_non_decl.empty()) { return; }

  size_t num_instances = 0;
  for (const auto &kv : log) { num_instances += kv.second.size(); }

  fprintf(stderr, fmt, num_instances, num_instances ? "s" : "");

  for (const auto &kv : log) {
    fprintf(stderr, "  Found %lu instance%s in '%s':\n", kv.second.size(),
            kv.second.size() == 1 ? "s" : "", kv.first);

    int line_num_width   = (int)NumDigits(kv.second.back());
    size_t last_line_num = kv.second.front();
    for (auto line_num : kv.second) {
      if (line_num - last_line_num == 2) {
        pstr line = source_map AT(kv.first)->lines AT(line_num - 1);
        fprintf(stderr, "    %*lu| %s\n", line_num_width, line_num - 1,
                line.ptr);
      } else if (line_num - last_line_num == 3) {
        pstr line = source_map AT(kv.first)->lines AT(line_num - 1);
        fprintf(stderr, "    %*lu| %s\n", line_num_width, line_num - 1,
                line.ptr);

        line = source_map AT(kv.first)->lines AT(line_num - 2);
        fprintf(stderr, "    %*lu| %s\n", line_num_width, line_num - 2,
                line.ptr);
      } else if (line_num - last_line_num > 3) {
        fprintf(stderr, "%s...|\n",
                std::string((size_t)line_num_width + 1, ' ').c_str());
      }

      pstr line = source_map AT(kv.first)->lines AT(line_num);
      fprintf(stderr, ">   %*lu| %s\n", line_num_width, line_num, line.ptr);
      last_line_num = line_num;
    }
  }
}

void Error::Log::Dump() {
  GatherAndDisplay("Undeclared identifier '%s'", undeclared_identifiers);
  GatherAndDisplay("Ambiguous identifier '%s'", ambiguous_identifiers);
  GatherAndDisplay("Invalid capture of identifier '%s'", invalid_capture);
  GatherAndDisplay("Found %lu non-declaration statement%s at the top level. "
                   "All top-level statements must either be declarations or "
                   "imports.",
                   global_non_decl);

  for (const auto &file_log : log_) {
    // NOTE: No sense in robustifying this. It probably won't last.
    size_t num_eqs = 38 - file_log.first.size() / 2;
    std::string eqs(num_eqs, '=');
    std::cerr << eqs << ' ' << file_log.first << ' ' << eqs << std::endl;

    for (const auto &line_log : file_log.second) {
      std::cerr << "  (line " << line_log.first << "):\n";

      for (const auto &msg : line_log.second) {
        std::cerr << "    " << msg << "\n\n";
      }
    }
  }

  std::cerr << num_errs_ << " error";
  if (num_errs_ != 1) { std::cerr << "s"; }

  std::cerr << " found." << std::endl;
}

void Error::Log::Log(const Cursor &loc, const std::string &msg) {
  ++num_errs_;
  log_[(std::string)loc.file_name][loc.line_num].push_back(msg);
}

static void DisplayErrorMessage(const char *msg_head, const char *msg_foot,
                           const Cursor &loc, size_t underline_length) {
  assert(loc.file_name);
  pstr line = source_map AT(loc.file_name)->lines AT(loc.line_num);

  size_t left_border_width = NumDigits(loc.line_num) + 4;

  // Extra + 1 at the end because we might point after the end of the line.
  std::string underline(strlen(line.ptr) + left_border_width + 1, ' ');
  for (size_t i = left_border_width + loc.offset;
       i < left_border_width + loc.offset + underline_length; ++i) {
    underline[i] = '^';
  }

  fprintf(stderr, "%s\n\n"
                  "    %lu| %s\n"
                  "%s\n",
          msg_head, loc.line_num, line.ptr, underline.c_str());

  if (msg_foot) {
    fprintf(stderr, "%s\n\n", msg_foot);
  } else {
    fprintf(stderr, "\n");
  }
}

namespace Error {
namespace Log {
void NullCharInSrc(const Cursor &loc) {
  fprintf(stderr, "I found a null-character in your source file on line %lu. "
                  "I am ignoring it and moving on. Are you sure \"%s\" is a "
                  "source file?\n\n",
          loc.line_num, loc.file_name);
  ++num_errs_;
}

void NonGraphicCharInSrc(const Cursor &loc) {
  fprintf(stderr, "I found a non-graphic-character in your source file on line "
                  "%lu. I am ignoring it and moving on. Are you sure \"%s\" is "
                  "a source file?\n\n",
          loc.line_num, loc.file_name);
  ++num_errs_;
}

void TooManyDots(const Cursor &loc, size_t num_dots) {
  const char *msg_head = "There are too many consecutive '.' characters. I am "
                         "assuming you meant \"..\".";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, num_dots);
}

void NonWhitespaceAfterNewlineEscape(const Cursor &loc, size_t dist) {
  const char *msg_head =
      "I found a non-whitespace character following a '\\' on the same line.";
  const char *msg_foot = "Backslashes are used to ignore line-breaks and "
                         "therefore must be followed by a newline (whitespace "
                         "between the backslash and the newline is okay).";

  ++num_errs_;
  DisplayErrorMessage(msg_head, msg_foot, loc, dist);
}

void InvalidHashtag(const Cursor &loc) {
  const char *msg_head = "I found a '#' that wasn't followed by an alphabetic "
                         "character or underscore.";
  const char *msg_foot = "Hashtags must be a valid identifier.";

  ++num_errs_;
  DisplayErrorMessage(msg_head, msg_foot, loc, 1);
}

void NotInMultilineComment(const Cursor &loc) {
  const char *msg_head = "I found a token representing the end of a multi-line "
                         "comment (*/), but it was not part of a comment "
                         "block.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 2);
}

void RunawayMultilineComment() {
  fprintf(stderr, "Finished reading file during multi-line comment.\n\n");
  ++num_errs_;
}

void RunawayCharLit(const Cursor &loc) {
  const char *msg_head =
      "You are missing a quotation mark at the end of your character-literal.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
}

void GlobalNonDecl(const Cursor &loc) {
  ++num_errs_;
  global_non_decl[loc.file_name].push_back(loc.line_num);
}

void RunawayStringLit(const Cursor &loc) {
  const char *msg_head = "You are missing a quotation mark at the end of your "
                         "string-literal. I think it goes here:";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
}

void EscapedSingleQuoteInStringLit(const Cursor &loc) {
  const char *msg_head = "The single quotation mark character (') does not "
                         "need to be esacped in a string-literal.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 2);
}

void EscapedDoubleQuoteInCharLit(const Cursor &loc) {
  const char *msg_head = "The double quotation mark character (\") does not "
                         "need to be esacped in a character-literal.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 2);
}

void InvalidStringIndex(const Cursor &loc, Type *index_type) {
  std::string msg_head = "String indexed by an invalid type. Expected an int "
                         "or uint, but encountered a " +
                         index_type->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);

}

void InvalidEscapeCharInStringLit(const Cursor &loc) {
  const char *msg_head =
      "I encounterd an invalid escape sequence in your string-literal.";
  const char *msg_foot = "The valid escape sequences in a string-literal are "
                         "\\\\, \\\", \\a, \\b, \\f, \\n, \\r, \\t, and \\v.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, msg_foot, loc, 2);
}

void InvalidEscapeCharInCharLit(const Cursor &loc) {
  const char *msg_head =
      "I encounterd an invalid escape sequence in your character-literal.";
  const char *msg_foot = "The valid escape sequences in a character-literal "
                         "are \\\\, \\\', \\a, \\b, \\f, \\n, \\r, \\t, and "
                         "\\v.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, msg_foot, loc, 2);
}

void InvalidCharQuestionMark(const Cursor &loc) {
  const char *msg_head = "The character '?' is not used in Icarus. I am going "
                         "to ignore it and continue processing.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
}

void InvalidCharTilde(const Cursor &loc) {
  const char *msg_head = "The character '~' is not used in Icarus. I am going "
                         "to ignore it and continue processing.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
}

void TabInCharLit(const Cursor &loc) {
  const char *msg_head = "I found a tab '\t' in your character-literal. You "
                         "need to use '\\t' instead.";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
}

void MissingComma(const Cursor &loc) {
  const char *msg_head =
      "There are two consecutive expressions. Are you missing a comma?";
  ++num_errs_;
  DisplayErrorMessage(msg_head, nullptr, loc, 1);
}

void UnopTypeFail(const std::string &msg, const AST::Unop *unop) {
  ++num_errs_;
  DisplayErrorMessage(msg.c_str(), nullptr, unop->loc, 1);
}

void NonIntegralArrayIndex(const Cursor &loc, const Type *index_type) {
  std::string msg_head = "Array is being indexed by an expression of type " +
                         index_type->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(
      msg_head.c_str(),
      "Arrays must be indexed by integral types (either int or uint)", loc, 1);
}

void EmptyArrayLit(const Cursor &loc) {
  ++num_errs_;
  DisplayErrorMessage("Cannot infer the type of an empty array literal.",
                      nullptr, loc, 2);
}

void InvalidAddress(const Cursor &loc, Assign mode) {
  ++num_errs_;
  if (mode == Assign::Const) {
    DisplayErrorMessage(
        "Attempting to take the address of an identifier marked as #const",
        "Marking an identifier as #const means that it is a compile-time "
        "constant. The value may not actually be associated with a storage "
        "address. For this reason, it is illegal to take the address.",
        loc, 1);
  } else if (mode == Assign::RVal) {
    DisplayErrorMessage(
        "Attempting to take the address of a temporary expression.", nullptr, loc,
        1);
  } else {
    UNREACHABLE;
  }
}

void InvalidAssignment(const Cursor &loc, Assign mode) {
  ++num_errs_;
  if (mode == Assign::Const) {
    DisplayErrorMessage(
        "Attempting to assign to an identifier marked as #const",
        "Marking an identifier as #const means that it is a compile-time "
        "constant and cannot be modified at run-time.",
        loc, 1);
  } else if (mode == Assign::RVal) {
    DisplayErrorMessage(
        "Attempting to assign to a temporary expression.", nullptr, loc,
        1);
  } else {
    UNREACHABLE;
  }
}

void CaseLHSBool(const Cursor &case_loc, const Cursor &loc, const Type *t) {
  std::string msg_head = "In a case statement, the lefthand-side of a case "
                         "must have type bool. However, the expression has "
                         "type " +
                         t->to_string() + ".";
  ++num_errs_;
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void ResizingFixedArray(const Cursor &loc) {
  ++num_errs_;
  DisplayErrorMessage("Cannot resize a fixed-length array.", nullptr, loc, 1);
}

void MissingMember(const Cursor &loc, const std::string &member_name,
                   const Type *t) {
  ++num_errs_;
  std::string msg_head = "Expressions of type `" + t->to_string() + "` have no member named '" +
                         member_name + "'.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void IndexingNonArray(const Cursor &loc, const Type *t) {
  ++num_errs_;
  std::string msg_foot = "Indexed type is a `" + t->to_string() + "`.";
  DisplayErrorMessage("Cannot index into a non-array type.", msg_foot.c_str(),
                      loc, 1);
}

void SlicingNonArray(const Cursor &loc, const Type *t) {
  ++num_errs_;
  std::string msg_foot = "Sliced type is a `" + t->to_string() + "`.";
  DisplayErrorMessage("Cannot slice a non-array type.", msg_foot.c_str(), loc,
                      1);
}

void InvalidCast(const Cursor &loc, const Type *from, const Type *to) {
  ++num_errs_;
  std::string msg_head = "No valid cast from `" + from->to_string() + "` to `" +
                         to->to_string() + "`.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 2);
}

void NotAType(const Cursor &loc, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head = "In declaration of `" + id_tok +
                         "`, the declared type is not a actually a type.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void DeclaredVoidType(const Cursor &loc, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head =
      "Identifier `" + id_tok + "`is declared to have type `void`.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void DeclaredParametricType(const Cursor &loc, const std::string &id_tok) {
  ++num_errs_;
  std::string msg_head =
      "In declaration of `" + id_tok +
      "`, the declared type is parametric and has no parameters provided.";
  DisplayErrorMessage(msg_head.c_str(), nullptr, loc, 1);
}

void RepeatedEnumName(const Cursor &loc) {
  ++num_errs_; // TODO better error message here.
  DisplayErrorMessage("Repeated enum member.", nullptr, loc, 1);
}

void DoubleDeclAssignment(const Cursor &decl_loc, const Cursor &val_loc) {
  ++num_errs_;
  if (decl_loc.line_num == val_loc.line_num){
    DisplayErrorMessage("Attempting to initialize an identifier that already "
                        "has an initial value.",
                        nullptr, decl_loc, 1);
  } else {
    NOT_YET;
  }
}

void InvalidImport(const Cursor &loc) {
  ++num_errs_;

  DisplayErrorMessage("Import statements must take a string literal as the "
                      "name of the file to be imported.",
                      nullptr, loc, 1);
}
void InvalidReturnType(const Cursor &loc, Type *given, Type *correct) {
  ++num_errs_;
  std::string msg_head = "Invalid return type on line " +
                         std::to_string(loc.line_num) + " in \"" +
                         loc.file_name + "\".";
  std::string msg_foot = "Given return type:    " + given->to_string() +
                         "\n"
                         "Expected return type: " +
                         correct->to_string();
  DisplayErrorMessage(msg_head.c_str(), msg_foot.c_str(), loc,
                      strlen(loc.line.ptr) - loc.offset);
}


static void DisplayLines(const std::vector<Cursor> &lines) {
  size_t left_space     = NumDigits(lines.back().line_num) + 2;
  std::string space_fmt = std::string(left_space - 3, ' ') + "...|\n";

  size_t last_line_num = lines[0].line_num - 1;
  for (auto loc : lines) {
    if (loc.line_num != last_line_num + 1) { fputs(space_fmt.c_str(), stderr); }
    fprintf(stderr, "%*lu| %s\n", (int)left_space, loc.line_num, loc.line.ptr);
    last_line_num = loc.line_num;
  }

  fputc('\n', stderr);
}

void CaseTypeMismatch(AST::Case *case_ptr, Type *correct) {
  if (correct) {
    fprintf(stderr, "Type mismatch in case-expression on line %lu in \"%s\".\n",
            case_ptr->loc.line_num, case_ptr->loc.file_name);

    std::vector<Cursor> locs;
    for (auto kv : case_ptr->key_vals) {
      ++num_errs_;
      if (kv.second->type == Err || kv.second->type == correct) { continue; }
      locs.push_back(kv.second->loc);
    }

    num_errs_ += locs.size();
    DisplayLines(locs);
    fprintf(stderr, "Expected an expression of type %s.\n\n",
            correct->to_string().c_str());

  } else {
    ++num_errs_;
    fprintf(stderr, "Type mismatch in case-expression on line %lu in \"%s\".\n\n",
            case_ptr->loc.line_num, case_ptr->loc.file_name);
  }
}


void UnknownParserError(const std::string &file_name,
                        const std::vector<Cursor> &lines) {

  if (lines.empty()) {
    fprintf(stderr,
            "Parse errors found in \"%s\". Sorry I can't be more specific.",
            file_name.c_str());
    ++num_errs_;
    return;
  }

  num_errs_ += lines.size();
  fprintf(stderr, "Parse errors found in \"%s\" on the following lines:\n\n",
          file_name.c_str());

  DisplayLines(lines);
}

void UndeclaredIdentifier(const Cursor &loc, const char *token) {
  ++num_errs_;
  undeclared_identifiers[token][loc.file_name][loc.line_num].push_back(
      loc.offset);
}

void InvalidCapture(const Cursor &loc, const AST::Declaration *decl) {
  ++num_errs_;
  invalid_capture[decl][loc.file_name][loc.line_num].push_back(
      loc.offset);
}

void AmbiguousIdentifier(const Cursor &loc, const char *token) {
  ++num_errs_;
  ambiguous_identifiers[token][loc.file_name][loc.line_num].push_back(
      loc.offset);
}

} // namespace Log
} // namespace Error
