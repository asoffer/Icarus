#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/extend/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(extend x with (y) {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               Extension(ExtensionStart(), Identifier(),
                                         ExtendWith(), Identifier(),
                                         StatementSequence(ScopeStart()))))));
}

}  // namespace
}  // namespace ic
