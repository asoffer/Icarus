#include "frontend/tokenize.h"

#include <unordered_map>
#include <vector>

#include "base/debug.h"
#include "base/log.h"
#include "base/macros.h"
#include "frontend/numbers.h"
#include "frontend/source.h"

namespace frontend {
namespace {

constexpr bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
constexpr bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
constexpr bool IsNonZeroDigit(char c) { return ('1' <= c && c <= '9'); }
constexpr bool IsDigit(char c) { return ('0' <= c && c <= '9'); }

constexpr bool IsAlpha(char c) { return IsLower(c) || IsUpper(c); }
constexpr bool IsAlphaNumeric(char c) { return IsAlpha(c) || IsDigit(c); }
constexpr bool IsWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}
constexpr bool IsIdentifierStartChar(char c) { return IsAlpha(c) || c == '_'; }
constexpr bool IsIdentifierChar(char c) {
  return IsIdentifierStartChar(c) || c == '-';
}

// Returns a string_view prefix of the input consisting of the first contiguous
// collection of characters satisfying the predicate. These characters are
// removed `*line`. `line` must not be null.
template <typename Fn>
std::string_view ConsumeWhile(Fn&& predicate, std::string_view* line) {
  size_t pos = 0;
  for (char c : *line) {
    if (!predicate(c)) { break; }
    ++pos;
  }
  std::string_view result(line->data(), pos);
  line->remove_prefix(pos);
  return result;
}

}  // namespace

/*
std::unordered_map<std::string_view, int> ReservedKeywordExpressions{
    //    {"bool", ir::Val(type::Bool)},
    //    {"int8", ir::Val(type::Int8)},
    //    {"int16", ir::Val(type::Int16)},
    //    {"int32", ir::Val(type::Int32)},
    //    {"int64", ir::Val(type::Int64)},
    //    {"nat8", ir::Val(type::Nat8)},
    //    {"nat16", ir::Val(type::Nat16)},
    //    {"nat32", ir::Val(type::Nat32)},
    //    {"nat64", ir::Val(type::Nat64)},
    //    {"float32", ir::Val(type::Float32)},
    //    {"float64", ir::Val(type::Float64)},
    //    {"type", ir::Val(type::Type_)},
    //    {"module", ir::Val(type::Module)},
    //    {"true", ir::Val(true)},
    //    {"false", ir::Val(false)},
    //    {"null", ir::Val(nullptr)},
    //#ifdef DBG
    //    {"debug_ir", DebugIrFunc()},
    //#endif  // DBG
    //    {"foreign", ir::Val::BuiltinGeneric(ForeignFuncIndex)},
    //    {"opaque", ir::Val::BuiltinGeneric(OpaqueFuncIndex)},
    //    {"byte_view", ir::Val(type::ByteView)},
    //    {"bytes", BytesFunc()},
    //    {"alignment", AlignFunc()},
    //    // TODO these are terrible. Make them reasonable. In particular, this
    //    is
    //    // definitively UB.
    //    {"exit", ir::Val::Block(nullptr)},
    //    {"start", ir::Val::Block(reinterpret_cast<ast::BlockLiteral*>(0x1))}
};
*/


// TODO rename kw_struct to more clearly indicate that it's a scope block
// taking a an optional parenthesized list before the braces.
std::unordered_map<std::string_view, Tag> const ReservedKeywords = {
    {"which", op_l},         {"print", op_l},     {"ensure", op_l},
    {"needs", op_l},         {"import", op_l},    {"flags", kw_block_head},
    {"enum", kw_block_head}, {"generate", op_l},  {"struct", kw_struct},
    {"return", op_lt},       {"yield", op_lt},    {"switch", kw_struct},
    {"when", op_b},          {"as", op_b},        {"interface", kw_block},
    {"copy", op_l},          {"move", op_l},      {"stateful-scope", kw_block},
    {"scope", kw_block},     {"block", kw_block}, {"rep-block", kw_block},
    {"opt-block", kw_block}};

TaggedToken ConsumeWord(std::string_view* line) {
  std::string_view ident = ConsumeWhile(IsIdentifierChar, line);

  // TODO re-enable
  // if (auto iter = Reserved.find(id); iter != Reserved.end()) {
  //   return TaggedNode::TerminalExpression(span, iter->second);
  // }

  if (auto iter = ReservedKeywords.find(ident);
      iter != ReservedKeywords.end()) {
    return TaggedToken(iter->second, ident);
  }

  return {id, ident};
}

TaggedToken ConsumeNumber(std::string_view* line) {
  std::string_view number_part = ConsumeWhile(
      [](char c) {
        return c == 'b' || c == 'o' || c == 'd' || c == 'x' || c == '_' ||
               c == '.' || IsDigit(c);
      },
      line);

  ASSIGN_OR(
      [&]() {
        base::Log() << "Error: " << _.error().to_string();
        NOT_YET();
      }(),
      auto parsed_num, ParseNumber(number_part));
  return std::visit([](auto n) { return TaggedToken(expr, n); }, parsed_num);
}

// Single line comments start with "//" and end at the next \n character in the
// source, or the next explicit endline "\\" (whichever comes first).
void ConsumeSingleLineComment(std::string_view* line) {
  ConsumeWhile([](char c) { return c != '\\'; }, line);
  // If there are 0 or 1 characters left, we cannot possible have a "\\".
  if (line->size() < 2) { return; }

  if ((*line)[1] == '\\') { return; }
  line->remove_prefix(2);
  ConsumeSingleLineComment(line);
}

TaggedToken Tokenizer::Next() {
  if (line_.empty()) {
    if (more_to_read_) {
      auto chunk    = src_->ReadUntil('\n');
      line_         = chunk.view;
      more_to_read_ = chunk.more_to_read;
      return TaggedToken{newline, "\n"};
    } else {
      return TaggedToken{eof, ""};
    }
  }

  if (IsWhitespace(line_[0])) {
    line_.remove_prefix(1);
    return Next();
  } else if (IsIdentifierStartChar(line_[0])) {
    return ConsumeWord(&line_);
  } else if (IsDigit(line_[0])) {
    return ConsumeNumber(&line_);
  }

  switch (line_[0]) {
    case '/': {
      line_.remove_prefix(1);
      if (line_.empty() || IsWhitespace(line_[0])) {
        return TaggedToken{op_b, Operator::Div};
      } else if (line_[0] == '/') {
        ConsumeSingleLineComment(&line_);
        if (line_.empty()) { return Next(); }
        line_.remove_prefix(2);
        return TaggedToken{newline, R"(\\)"};
      }
      /*
          switch (line_[1]) {
            case '/': {
              line_.remove_prefix(1);
              // ConsumeSingleLineComment(line_);
              return Next();
            }
        } break;
      */
    } break;
  }
  UNREACHABLE(line_[0]);
}

}  // namespace frontend
