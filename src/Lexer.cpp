#include "Lexer.h"
#include "Language.h"
#include "ErrorLog.h"

#include <map>

extern ErrorLog error_log;

// Local function for recognizing newlines a la std::isalpha, etc.
bool isnewline(int n) {
  return n == '\n' || n == '\r';
}

// Take a filename as a string or a C-string and opens the named file
Lexer::Lexer(const std::string& file_name) :
  file_name_(file_name.c_str()),
  file_(file_name, std::ifstream::in),
  line_num_(1) {
  }

// Lexer destructor:
//
// Closes the file opened by the constructor
Lexer::~Lexer() { file_.close(); }

// Get the next token
Lexer& operator>>(Lexer& lexer, AST::TokenNode& node) {
  int peek;

restart:
  // This label is used in the case that a space/tab character is encountered.
  // It allows us to repeat until we find a non-space/tab character.

  peek = lexer.file_.peek();

  // Delegate based on the next character in the file stream
  if (peek == EOF) {
    node = AST::TokenNode::eof(lexer.line_num_);

  } else if (isnewline(peek)) {
    node = AST::TokenNode::newline();
    ++lexer.line_num_;
    lexer.file_.get();

  } else if (std::isspace(peek)) {
    // Ignore space/tab characters by restarting
    lexer.file_.get();
    goto restart;

  } else if (std::isalpha(peek) || peek == '_') {
    node = lexer.next_word();

  } else if (std::isdigit(peek)) {
    node = lexer.next_number();

  } else if (std::ispunct(peek)) {
    node = lexer.next_operator();

  } else {
    assert(false && "Lexer found a control character.");
  }

  return lexer;
}

// The next token begins with an alpha character meaning that it is either a
// reserved word or an identifier.
AST::TokenNode Lexer::next_word() {
  assert((std::isalpha(file_.peek()) || file_.peek() == '_')
      && "Non-alpha character encountered as first character in next_word.");

  // Used to store the word
  std::string token;

  // Repeatedly add characters to the word so long as the word only uses
  // characters in the range a-z, A-Z, 0-9, and _ (and starts with an alpha
  // character).
  int peek;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isalnum(peek) || peek == '_');

  // Check if the word is reserved and if so, build the appropriate Node
  for (const auto& res : Language::reserved_words) {
    if (res.first == token) {
      return AST::TokenNode(line_num_, res.second, res.first);
    }
  }

  // Check if the word is a type primitive/literal and if so, build the
  // appropriate Node.
  for (const auto& type_lit : TypeSystem::Literals) {
    if (type_lit.first == token) {
      return AST::TokenNode(line_num_, Language::type_literal, token);
    }
  }

  // It's an identifier
  return AST::TokenNode(line_num_, Language::identifier, token);
}


AST::TokenNode Lexer::next_number() {
  assert(std::isdigit(file_.peek())
      && "Non-digit character encountered as first character in next_number.");

  // Used to store the number
  std::string token;

  // Add digits
  int peek;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  if (peek == 'u' || peek == 'U') {
    // If the next character is a 'u' or a 'U', it's an integer literal. We can
    // ignore the character and return.
    file_.get();
    return AST::TokenNode(line_num_, Language::uint_literal, token);

  } else if (peek != '.') {
    // If the next character is not a period, we're looking at an integer and
    // can return.
    return AST::TokenNode(line_num_, Language::int_literal, token);
  }

  // If the next character was a period, this is a non-integer. Add the period
  // and keep going with more digits.
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  return AST::TokenNode(line_num_, Language::real_literal, token);
}


AST::TokenNode Lexer::next_operator() {
  // Sanity check:
  // We only call this function if the top character is punctuation
  int peek = file_.peek();
  assert(
      std::ispunct(peek) &&
      "Non-punct character encountered as first character in next_operator.");

  // In general, we're going to take all punctuation characters, lump them
  // together, and call that an operator. However, some operators are just one
  // character and should be taken by themselves.
  //
  // For example, the characters '(', ')', '[', ']', '{', '}', '"', '\'', if
  // encountered should be considered on their own.
  switch (peek) {
    case '@':
      file_.get();
      return AST::TokenNode(line_num_, Language::dereference, "@");
    case ',':
      file_.get();
      return AST::TokenNode(line_num_, Language::comma, ",");
    case ';':
      file_.get();
      return AST::TokenNode(line_num_, Language::semicolon, ";");
    case '(':
      file_.get();
      return AST::TokenNode(line_num_, Language::left_paren, "(");
    case ')':
      file_.get();
      return AST::TokenNode(line_num_, Language::right_paren, ")");
    case '{':
      file_.get();
      return AST::TokenNode(line_num_, Language::left_brace, "{");
    case '}':
      file_.get();
      return AST::TokenNode(line_num_, Language::right_brace, "}");
    case '[':
      file_.get();
      return AST::TokenNode(line_num_, Language::left_bracket, "[");
    case ']':
      file_.get();
      return AST::TokenNode(line_num_, Language::right_bracket, "]");
    case '.':
      file_.get();
      return AST::TokenNode(line_num_, Language::dot, ".");
    case '"':
      file_.get();
      return next_string_literal();
    case '\'':
      file_.get();
      return next_char_literal();
  }

  if (peek == '/') {
    return next_given_slash();
  }

  // Cannot have '-' in this list because of '->'
  // The case '/' is missing because it has special the cases // and /* to deal
  // with
  char lead_char = 0;
  Language::NodeType node_type;
  switch (peek) {
    case '+':
    case '*':
    case '%':
      lead_char = static_cast<char>(peek);
      node_type = Language::generic_operator;
      break;
    case '<':
    case '>':
      lead_char = static_cast<char>(peek);
      node_type = Language::binary_boolean_operator;
      break;
    default:
      lead_char = 0;
  }

  if (lead_char != 0) {
    file_.get();
    peek = file_.peek();

    std::string tok(1, lead_char);
    if (peek == '=') {
      tok += "=";
      file_.get();
    }

    if (node_type == Language::generic_operator && peek == '=') {
      node_type = Language::assign_operator;
    }

   return AST::TokenNode(line_num_, node_type, tok);
  }

  if (peek == ':') {
    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      file_.get();
      return AST::TokenNode(line_num_, Language::decl_assign_operator, ":=");

    } else if (peek == '>') {
      file_.get();
      return AST::TokenNode(line_num_, Language::generic_operator, ":>");
 
    } else {
      return AST::TokenNode(line_num_, Language::decl_operator, ":");
    }
  }

  if (peek == '|' || peek == '^' || peek == '&') {
    char past_peek = static_cast<char>(peek);

    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      std::string tok = "_=";
      tok[0] = past_peek;
      file_.get();
      return AST::TokenNode(line_num_, Language::assign_operator, tok);

    } else {
      return AST::TokenNode(line_num_,
          (past_peek == '&' ? Language::indirection : Language::bool_operator),
          std::string(1, past_peek));
    }
  }

  if (peek == '!') {
    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      file_.get();
      return AST::TokenNode(line_num_, Language::binary_boolean_operator, "!=");
    } else {
      return AST::TokenNode(line_num_, Language::unary_operator, "!");
    }
  }


  if (peek == '-' || peek == '=') {
    char lead_char = static_cast<char>(peek);
    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      file_.get();

      Language::NodeType node_type = (lead_char == '-'
          ? Language::assign_operator
          : Language::binary_boolean_operator);
      return AST::TokenNode(line_num_, node_type, std::string(1, lead_char) + "=");
      
    } else if (peek == '>') {
      file_.get();
      if (lead_char == '-') {
        return AST::TokenNode(line_num_, Language::fn_arrow, "->");
      } else {
        return AST::TokenNode(line_num_, Language::rocket_operator, "=>");
      }
    } else {
      if (lead_char == '-') {
        return AST::TokenNode(line_num_, Language::negation, "-");
      } else {
        return AST::TokenNode(line_num_, Language::assign_operator, "=");
      }
    }
  }

  // If the first character isn't one of the specific ones mentioned above, read
  // in as many characters as possible.
  std::string token;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::ispunct(peek));

  return AST::TokenNode(line_num_, Language::generic_operator, token);
}

AST::TokenNode Lexer::next_string_literal() {
  int peek = file_.peek();
  std::string str_lit = "";

  // Repeat until you see a double-quote to end the string, or a newline
  // (which designates an error where they forgot to end the string)
  while (!(peek == '"' || isnewline(peek))) {
    // If you see a backslash, the next character is escaped
    if (peek == '\\') {
      file_.get();
      peek = file_.peek();
      switch (peek) {
        case '\\': str_lit += '\\'; break;
        case '"':  str_lit += '"';  break;
        case 'n':  str_lit += '\n'; break;
        case 'r':  str_lit += '\r'; break;
        case 't':  str_lit += '\t'; break;
        default:
          {
            error_log.log(line_num_,
                "The sequence `\\" + std::to_string(static_cast<char>(peek)) + "` is not an escape character.");

            str_lit += static_cast<char>(peek);
            break;
          }
      }
      file_.get();
    } else {
      str_lit += static_cast<char>(file_.get());
    }

    peek = file_.peek();
  }

  if (peek == '"') {
    // Ignore the last quotation mark if it exists
    file_.get();
  }
  else {
    error_log.log(line_num_,
        "String literal is not closed before the end of the line.");
  }

  return AST::TokenNode::string_literal(line_num_, str_lit);
}

AST::TokenNode Lexer::next_char_literal() {
  int peek = file_.peek();

  char output_char;

  // TODO 
  // 1. deal with the case of a tab character literally between single quotes.
  // 2. robust error handling
  switch(peek) {
    case '\n':
    case '\r':
      {
        error_log.log(line_num_, "Cannot use newline inside a character-literal.");
        return AST::TokenNode(line_num_, Language::newline);
      }
    case '\\':
      {
        file_.get();
        peek = file_.peek();
        switch (peek) {
          case '\'': output_char = '\''; break;
          case '\"': output_char = '"';
                     error_log.log(line_num_,
                         "Warning: the character '\"' does not need to be escaped.");
                     break;
          case '\\': output_char = '\\'; break;
          case 't':  output_char = '\t'; break;
          case 'n':  output_char = '\n'; break;
          case 'r':  output_char = '\r'; break;
          default:
                     error_log.log(line_num_,
                         "The specified character is not an escape character.");
          output_char = static_cast<char>(peek);
        }

        break;
      }
    default:
      {
        output_char = static_cast<char>(peek);
      }
  }

  file_.get();
  peek = file_.peek();

  if (peek == '\'') {
    file_.get();
  } else {
    error_log.log(line_num_, "Character literal must be followed by a single-quote.");
  }

  return AST::TokenNode(line_num_, Language::char_literal, std::string(1, output_char));
}

AST::TokenNode Lexer::next_given_slash() {
  int peek = file_.peek();
  assert(peek == '/' && "Non-slash character encountered as first character in next_operator.");

  file_.get();
  peek = file_.peek();

  // If the first two characters are '//', then it's a single-line comment
  if (peek == '/') {
    std::string token = "";

    // Add characters while we're not looking at a newline
    do {
      token += static_cast<char>(file_.get());
      peek = file_.peek();
    } while (!isnewline(peek));

    return AST::TokenNode(line_num_, Language::comment, token);
  }

  // If the first two characters are '/*' then start a multi-line comment.
  // Multi-line comments should nest
  if (peek == '*') {
    size_t comment_layer = 1;
    
    // This is intentionally not set to '/' (which must be the character
    // preceeding peek at this point because we don't want to count it in the
    // loop more than once). 
    int prepeek = 0;

    while (comment_layer != 0) {
      prepeek = peek;
      peek = file_.get();

      if (!*this) {  // If we're at the end of the stream
        error_log.log(line_num_, "File ended during multi-line comment.");
        return AST::TokenNode(line_num_, Language::comment, "");
      }

      if (prepeek == '/' && peek == '*') {
        ++comment_layer;

      } else if (prepeek == '*' && peek == '/') {
        --comment_layer;
      }
    }

    return AST::TokenNode(line_num_, Language::comment, "MULTILINE COMMENT");
  }

  if (peek == '=') {
    file_.get();
    return AST::TokenNode(line_num_, Language::assign_operator, "/=");
  }

  return AST::TokenNode(line_num_, Language::generic_operator, "/");
}
