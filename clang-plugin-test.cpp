/*
  The MIT License (MIT)

  Copyright (c) 2016 Stephen Kelly

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*/

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ParentMap.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/Config/llvm-config.h>

#include <iostream>
#include <sstream>
#include <stdio.h>

using namespace clang;
using namespace std;

class TestASTConsumer : public ASTConsumer,
                        public RecursiveASTVisitor<TestASTConsumer> {
  CompilerInstance &m_ci;

public:
  TestASTConsumer(CompilerInstance &ci) : m_ci(ci) {}

  void HandleTranslationUnit(ASTContext &ctx) override {
    TraverseDecl(ctx.getTranslationUnitDecl());
  }

  const clang::SourceManager &sm() const { return m_ci.getSourceManager(); }

  bool VisitStmt(Stmt *stm) {

    const SourceLocation start = stm->getLocStart();
    if (std::string(sm().getBufferName(start)).find(".cpp") ==
        std::string::npos)
      return true;

    DeclStmt *declStmt = dyn_cast<DeclStmt>(stm);
    if (!declStmt)
      return true;

    auto rng = declStmt->decls();

    for (auto child : rng) {
      VarDecl *varDecl = dyn_cast<VarDecl>(child);

      if (!varDecl)
        return true;

      if (!compatibleDecl(varDecl)) {
        return true;
      }
      std::string message = "auto can be used here";
      FullSourceLoc full(varDecl->getLocStart(), sm());
      unsigned id = m_ci.getDiagnostics().getDiagnosticIDs()->getCustomDiagID(
          DiagnosticIDs::Warning, message.c_str());
      m_ci.getDiagnostics().Report(full, id);
    }

    return true;
  }

  bool compatibleDecl(clang::VarDecl *varDecl) {

    if (varDecl->getInitStyle() != VarDecl::CInit)
      return false;

    if (varDecl->isExceptionVariable())
      return false;

    ParmVarDecl *parmVarDecl = dyn_cast<ParmVarDecl>(varDecl);
    if (parmVarDecl)
      return false;

    const Type *type = varDecl->getType().getTypePtrOrNull();

    if (dyn_cast<AutoType>(type))
      return false;

    if (varDecl->getType()->isAnyPointerType() &&
        dyn_cast<AutoType>(varDecl->getType()->getPointeeType()))
      return false;

    if (dyn_cast<ConstantArrayType>(type))
      return false;

    const Expr *exprInit = varDecl->getInit();
    if (!exprInit) {
      return false;
    }

    if (const auto *E = dyn_cast<ExprWithCleanups>(exprInit))
      exprInit = E->getSubExpr();

    // Drill down to the as-written initializer.

    const Expr *E = exprInit->IgnoreImplicit();

    const auto *Construct = dyn_cast<CXXConstructExpr>(exprInit);
    if (Construct) {

      // Ensure that the constructor receives a single argument.
      if (Construct->getNumArgs() != 1)
        return false;

      // Drill down to the as-written initializer.
      E = (*Construct->arg_begin())->IgnoreParenImpCasts();
    }
    if (E != E->IgnoreConversionOperator()) {
      // We hit a conversion operator. Early-out now as they imply an implicit
      // conversion from a different type. Could also mean an explicit
      // conversion from the same type but that's pretty rare.
      return false;
    }

    if (auto T = dyn_cast<CXXBindTemporaryExpr>(E)) {
      E = T->getSubExpr();
    }

    if (dyn_cast<ImplicitCastExpr>(E)) {
      return false;
    }
    if (dyn_cast<InitListExpr>(E)) {
      return false;
    }

    if (const auto *NestedConstruct = dyn_cast<CXXConstructExpr>(E)) {
      // If we ran into an implicit conversion contructor, can't convert.
      //
      // FIXME: The following only checks if the constructor can be used
      // implicitly, not if it actually was. Cases where the converting
      // constructor was used explicitly won't get converted.
      if (NestedConstruct->getConstructor()->isConvertingConstructor(false))
        return false;
    }

    auto declType = varDecl->getType();

    if (declType->isPointerType() &&
        declType->getPointeeType()->isFunctionType()) {
      return false;
    }

    if ((varDecl->getType()->getUnqualifiedDesugaredType() ==
         E->getType()->getUnqualifiedDesugaredType()) ||
        (varDecl->getType()->isPointerType() &&
         E->getType()->isPointerType())) {

      return true;
    }

    return false;
  }
};

class TestASTAction : public PluginASTAction {

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string> &arg) override {
    return true;
  }

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(CompilerInstance &ci, llvm::StringRef) override {
    return llvm::make_unique<TestASTConsumer>(ci);
  }
};

static FrontendPluginRegistry::Add<TestASTAction> X("clang-plugin-test",
                                                    "test plugin");
