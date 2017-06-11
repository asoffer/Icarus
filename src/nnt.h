#ifndef ICARUS_NNT
#define ICARUS_NNT

#include <memory>
#include "cursor.h"

struct NNT {
  NNT() = default;
  std::unique_ptr<AST::TokenNode> node = nullptr;
  Language::NodeType node_type = Language::bof;
  NNT(const Cursor &cursor, const std::string &token, Language::NodeType nt);

  NNT(std::unique_ptr<AST::TokenNode> n, Language::NodeType nt)
      : node(std::move(n)), node_type(nt) {}
  static NNT Invalid() {
    // Name of this function is clearer than just using default constructor
    return NNT();
  }
};

inline bool operator==(const NNT& lhs, const NNT& rhs) {
  return lhs.node.get() == rhs.node.get() && lhs.node_type == rhs.node_type;
}
inline bool operator!=(const NNT& lhs, const NNT& rhs) { return (lhs == rhs); }

#endif // ICARUS_NNT
