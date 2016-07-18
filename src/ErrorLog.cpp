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

static TokenToErrorMap undeclared_identifiers, ambiguous_identifiers;

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

static void GatherAndDisplay(const char *fmt, const TokenToErrorMap &log) {
  // TODO display file names too.
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
      fprintf(stderr, ".\n\n");
    } else if (num_uses == 2) {
      fprintf(stderr, " used twice.\n\n");
    } else {
      fprintf(stderr, " used %lu times.\n\n", num_uses);
    }

    for (const auto &file_and_locs : kv.second) {
      for (const auto &line_and_offsets : file_and_locs.second) {
        pstr line = source_map AT(file_and_locs.first)
                        ->lines AT(line_and_offsets.first);

        size_t left_border_width = NumDigits(line_and_offsets.first) + 4;
        size_t line_length       = strlen(line) + 1;
        char *underline          = new char[left_border_width + line_length + 1];
        underline[line_length + left_border_width] = '\0';
        memset(underline, ' ', left_border_width + line_length);

        for (const auto offset : line_and_offsets.second) {
          memset(underline + left_border_width + offset, '^', token_length);
        }

        fprintf(stderr, "  %lu| %s\n"
                        "%s\n",
                line_and_offsets.first, line.ptr, underline);
      }
    }
  }
}

void Error::Log::Dump() {
  GatherAndDisplay("Undeclared identifier '%s'", undeclared_identifiers);
  // TODO also log the declarations of the ambiguously declared identifiers and
  // display them.
  GatherAndDisplay("Ambiguous identifier '%s'", ambiguous_identifiers);

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
  pstr line = source_map AT(loc.file_name)->lines AT(loc.line_num);

  size_t left_border_width = NumDigits(loc.line_num) + 4;

  // Extra + 1 at the end because we might point after the end of the line.
  std::string underline(strlen(line.ptr) + left_border_width + 1, ' ');
  for (size_t i = left_border_width + loc.offset;
       i < left_border_width + loc.offset + underline_length; ++i) {
    underline[i] = '^';
  }

  fprintf(stderr, "%s\n\n"
                  "  %lu| %s\n"
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

void UndeclaredIdentifier(const Cursor &loc, const char *token) {
  ++num_errs_;
  undeclared_identifiers[token][loc.file_name][loc.line_num].push_back(
      loc.offset);
}

void AmbiguousIdentifier(const Cursor &loc, const char *token) {
  ++num_errs_;
  ambiguous_identifiers[token][loc.file_name][loc.line_num].push_back(
      loc.offset);
}

} // namespace Log
} // namespace Error
