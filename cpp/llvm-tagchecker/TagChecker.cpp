#include "clang/Tooling/Execution.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Signals.h"

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Refactoring/AtomicChange.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

namespace {

class TagChecker : public MatchFinder::MatchCallback {
public:
  explicit TagChecker(ExecutionContext &context) {}

  void run(const MatchFinder::MatchResult &Result) override {
    auto checkFunction = [](const Decl *fn, StringRef tag) {
      for (const auto *ann : fn->specific_attrs<AnnotateAttr>()) {
        const StringRef &ref = ann->getAnnotation();
        if (ref == tag) {
          return true;
        }
      }

      return false;
    };

    const auto *caller = Result.Nodes.getNodeAs<FunctionDecl>("caller");
    const auto *callee = Result.Nodes.getNodeAs<CallExpr>("callee");
    assert(parent);
    assert(callee);

    if (checkFunction(caller, "fast") &&
        checkFunction(callee->getDirectCallee(), "slow")) {
      clang::DiagnosticsEngine &engine = Result.Context->getDiagnostics();
      const unsigned ID =
          engine.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                 "fast function is calling a slow function");

      const clang::FixItHint FixIt =
          clang::FixItHint::CreateRemoval(callee->getExprLoc());

      engine.Report(caller->getLocation(), ID).AddFixItHint(FixIt);
    }
  }
};

} // end anonymous namespace

// Set up the command line options
// static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::OptionCategory ToolTemplateCategory("tool-template options");

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
      argc, argv, ToolTemplateCategory);

  if (!Executor) {
    llvm::errs() << llvm::toString(Executor.takeError()) << "\n";
    return EXIT_FAILURE;
  }

  TagChecker Callback(*Executor->get()->getExecutionContext());

  ast_matchers::MatchFinder Finder;
  // clang-format off
  auto matcher =
      callExpr(
          isExpansionInMainFile(),
          callee(functionDecl(hasAttr(attr::Annotate))),
          hasAncestor(functionDecl(hasAttr(attr::Annotate)).bind("caller"))
      ).bind("callee");
  // clang-format on
  Finder.addMatcher(matcher, &Callback);

  auto Err = Executor->get()->execute(newFrontendActionFactory(&Finder));
  if (Err) {
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
