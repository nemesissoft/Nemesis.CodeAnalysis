using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SF = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Nemesis.CodeAnalysis
{
    public class TriviaReducer : CSharpSyntaxRewriter
    {
        public override SyntaxTriviaList VisitList(SyntaxTriviaList list)
        {
            list = SF.TriviaList(
                list.Where(t => !t.IsKind(SyntaxKind.EndOfLineTrivia))
            );

            return list.Count > 0 ?
                SF.TriviaList(SF.Space) :
                base.VisitList(list);
        }

        private TriviaReducer(){}

        public static TNode Reduce<TNode>(TNode node) where TNode : SyntaxNode => (TNode) new TriviaReducer().Visit(node);
    }

    public class TriviaFlattener : CSharpSyntaxRewriter
    {
        public override SyntaxTriviaList VisitList(SyntaxTriviaList list)
        {
            if (list.Count == 0) return list;

            Debug.Assert(list.Count == 0 || list.All(st => st.Token == list[0].Token)); //all trivia should belong to same token

            bool isMethodBaseTrivia = list[0].Token.Parent is BaseMethodDeclarationSyntax;

            var originalTriviaWithNoWhitespace = (
                isMethodBaseTrivia ? list : list.Where(t => t.Kind() != SyntaxKind.WhitespaceTrivia && t.Kind() != SyntaxKind.EndOfLineTrivia)
            ).ToList();

            if (!originalTriviaWithNoWhitespace.Any())
                originalTriviaWithNoWhitespace.Add(SF.Space);

            return SF.TriviaList(originalTriviaWithNoWhitespace);
        }

        private TriviaFlattener() { }

        public static TNode Flatten<TNode>(TNode node) where TNode : SyntaxNode => (TNode)new TriviaFlattener().Visit(node);
    }

    public class TriviaPrefixer : CSharpSyntaxRewriter
    {
        private readonly IEnumerable<SyntaxTrivia> _leading;

        private TriviaPrefixer(IEnumerable<SyntaxTrivia> leading, bool visitIntoStructuredTrivia = false) : base(visitIntoStructuredTrivia)
            => _leading = leading?.ToArray() ?? new SyntaxTrivia[0];

        public static TNode Prepend<TNode>(TNode node, IEnumerable<SyntaxTrivia> leading) where TNode : SyntaxNode => 
            (TNode)new TriviaPrefixer(leading).Visit(node);

        #region Declarations

        public override SyntaxNode VisitConstructorDeclaration(ConstructorDeclarationSyntax node) =>
            base.VisitConstructorDeclaration(PrependDeclaration(node));

        public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node) =>
            base.VisitMethodDeclaration(PrependDeclaration(node));

        public override SyntaxNode VisitConversionOperatorDeclaration(ConversionOperatorDeclarationSyntax node) =>
            base.VisitConversionOperatorDeclaration(PrependDeclaration(node));

        public override SyntaxNode VisitDestructorDeclaration(DestructorDeclarationSyntax node) =>
            base.VisitDestructorDeclaration(PrependDeclaration(node));

        public override SyntaxNode VisitOperatorDeclaration(OperatorDeclarationSyntax node) =>
            base.VisitOperatorDeclaration(PrependDeclaration(node));

        public override SyntaxNode VisitPropertyDeclaration(PropertyDeclarationSyntax node) =>
            base.VisitPropertyDeclaration(PrependDeclaration(node));

        public override SyntaxNode VisitFieldDeclaration(FieldDeclarationSyntax node) =>
            base.VisitFieldDeclaration(PrependDeclaration(node));

        public override SyntaxNode VisitEventFieldDeclaration(EventFieldDeclarationSyntax node) =>
            base.VisitEventFieldDeclaration(PrependDeclaration(node));
        
        private TNode PrependDeclaration<TNode>(TNode node) where TNode : SyntaxNode
        {
            var originalTrivia = node.GetLeadingTrivia();
            var leadingTriviaWithSpecialWhitespace = originalTrivia.SelectMany(trivia =>
                trivia.Kind() == SyntaxKind.WhitespaceTrivia ? _leading : new[] { trivia }).ToList();

            node = node.WithLeadingTrivia(SF.TriviaList(leadingTriviaWithSpecialWhitespace));

            return node;
        }

        #endregion

        #region Statements and clauses

        public override SyntaxNode VisitBreakStatement(BreakStatementSyntax node) => base.VisitBreakStatement(PrependNode(node));

        public override SyntaxNode VisitCheckedExpression(CheckedExpressionSyntax node) => base.VisitCheckedExpression(PrependNode(node));

        public override SyntaxNode VisitCheckedStatement(CheckedStatementSyntax node) => base.VisitCheckedStatement(PrependNode(node));

        public override SyntaxNode VisitContinueStatement(ContinueStatementSyntax node) => base.VisitContinueStatement(PrependNode(node));

        public override SyntaxNode VisitDoStatement(DoStatementSyntax node) => base.VisitDoStatement(PrependNode(node));

        public override SyntaxNode VisitExpressionStatement(ExpressionStatementSyntax node) => base.VisitExpressionStatement(PrependNode(node));

        public override SyntaxNode VisitFixedStatement(FixedStatementSyntax node) => base.VisitFixedStatement(PrependNode(node));

        public override SyntaxNode VisitForStatement(ForStatementSyntax node) => base.VisitForStatement(PrependNode(node));

        public override SyntaxNode VisitForEachStatement(ForEachStatementSyntax node) => base.VisitForEachStatement(PrependNode(node));

        public override SyntaxNode VisitGotoStatement(GotoStatementSyntax node) => base.VisitGotoStatement(PrependNode(node));

        public override SyntaxNode VisitIfStatement(IfStatementSyntax node) => base.VisitIfStatement(PrependNode(node));

        public override SyntaxNode VisitLabeledStatement(LabeledStatementSyntax node) => base.VisitLabeledStatement(PrependNode(node));

        public override SyntaxNode VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node) => base.VisitLocalDeclarationStatement(PrependNode(node));

        public override SyntaxNode VisitLocalFunctionStatement(LocalFunctionStatementSyntax node) => base.VisitLocalFunctionStatement(PrependNode(node));

        public override SyntaxNode VisitLockStatement(LockStatementSyntax node) => base.VisitLockStatement(PrependNode(node));

        public override SyntaxNode VisitReturnStatement(ReturnStatementSyntax node) => base.VisitReturnStatement(PrependNode(node));

        public override SyntaxNode VisitSwitchStatement(SwitchStatementSyntax node) => base.VisitSwitchStatement(PrependNode(node));

        public override SyntaxNode VisitThrowStatement(ThrowStatementSyntax node) => base.VisitThrowStatement(PrependNode(node));

        public override SyntaxNode VisitTryStatement(TryStatementSyntax node) => base.VisitTryStatement(PrependNode(node));

        public override SyntaxNode VisitUnsafeStatement(UnsafeStatementSyntax node) => base.VisitUnsafeStatement(PrependNode(node));

        public override SyntaxNode VisitUsingStatement(UsingStatementSyntax node) => base.VisitUsingStatement(PrependNode(node));

        public override SyntaxNode VisitWhileStatement(WhileStatementSyntax node) => base.VisitWhileStatement(PrependNode(node));

        public override SyntaxNode VisitYieldStatement(YieldStatementSyntax node) => base.VisitYieldStatement(PrependNode(node));


        public override SyntaxNode VisitElseClause(ElseClauseSyntax node) => base.VisitElseClause(PrependNode(node));

        public override SyntaxNode VisitCatchClause(CatchClauseSyntax node) => base.VisitCatchClause(PrependNode(node));

        public override SyntaxNode VisitFinallyClause(FinallyClauseSyntax node) => base.VisitFinallyClause(PrependNode(node));

        public override SyntaxNode VisitBlock(BlockSyntax node)
        {
            node = node.WithOpenBraceToken(
                node.OpenBraceToken.WithLeadingTrivia(_leading.Concat(node.OpenBraceToken.LeadingTrivia))
            );
            node = node.WithCloseBraceToken(
                node.CloseBraceToken.WithLeadingTrivia(_leading.Concat(node.CloseBraceToken.LeadingTrivia))
            );
            return base.VisitBlock(node);
        }

        public override SyntaxNode VisitAccessorList(AccessorListSyntax node)
        {
            node = node.WithOpenBraceToken(
                node.OpenBraceToken.WithLeadingTrivia(_leading.Concat(node.OpenBraceToken.LeadingTrivia))
            );
            node = node.WithCloseBraceToken(
                node.CloseBraceToken.WithLeadingTrivia(_leading.Concat(node.CloseBraceToken.LeadingTrivia))
            );
            return base.VisitAccessorList(node);
        }

        public override SyntaxNode VisitAccessorDeclaration(AccessorDeclarationSyntax node) => base.VisitAccessorDeclaration(PrependNode(node));

        private TNode PrependNode<TNode>(TNode node) where TNode : SyntaxNode
            => node.WithLeadingTrivia(_leading.Concat(node.GetLeadingTrivia()));

        #endregion
    }
}
