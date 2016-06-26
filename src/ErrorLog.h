#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

namespace Error {
enum class MsgId {
  RunawayMultilineComment,
  InvalidEscapeCharInStringLit,
  InvalidEscapeCharInCharLit,
  RunawayStringLit,
  NewlineInCharLit,
  EscapedDoubleQuoteInCharLit,
  EscapedSingleQuoteInStringLit,
  RunawayCharLit
};

struct Msg {
  Msg(MsgId msg_id, const TokenLocation &tok_loc, size_t rad, size_t under_len)
      : loc(tok_loc), context_radius(rad), underline_length(under_len),
        mid(msg_id) {}

  TokenLocation loc;
  size_t context_radius; // Number of lines surrounding this one to show.
  size_t underline_length;
  MsgId mid;
};

} // namespace Err

class ErrorLog {
public:
  static bool ImmediateMode;

  ErrorLog();
  friend std::ostream &operator<<(std::ostream &os, const ErrorLog &log);

  size_t num_errors() const;
  void log(TokenLocation loc, const std::string &msg);

  void log(const Error::Msg &msg);

  AST::Node *assignment_vs_equality(AST::Node *node);

private:
  size_t err_num_;
  std::map<std::string, std::map<size_t, std::vector<std::string>>> log_;
};

extern ErrorLog error_log;

#endif // ICARUS_ERROR_LOG_H
