using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using SF = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Nemesis.CodeAnalysis
{
    public static class TriviaUtils
    {
        #region Comments

        public static bool IsSingleOrMultiLineComment(this SyntaxTrivia trivia) => trivia.IsKind(SyntaxKind.MultiLineCommentTrivia) || trivia.IsKind(SyntaxKind.SingleLineCommentTrivia);

        public static string GetCommentsFromNode(SyntaxNode node)
        {
            //TODO take also trivia from separators in SeparatedTriviaList

            var lang = node.SyntaxTree.Options.Language;
            string FormatTrivia(SyntaxTrivia st)
            {
                if (lang == LanguageNames.CSharp)
                    switch (st.Kind())
                    {
                        case SyntaxKind.MultiLineCommentTrivia:
                            var mlComment = st.ToString();
                            if (mlComment.StartsWith("/*")) mlComment = mlComment.Substring(2);
                            if (mlComment.EndsWith("*/")) mlComment = mlComment.Substring(0, mlComment.Length - 2);
                            return mlComment.Trim();
                        case SyntaxKind.SingleLineCommentTrivia:
                            var slComment = st.ToString();
                            if (slComment.StartsWith("//")) slComment = slComment.Substring(2);
                            return slComment.Trim();
                        case SyntaxKind.XmlComment:
                            return st.ToString().Trim();
                        default:
                            return st.ToString().Trim();
                    }
                else
                    throw new NotSupportedException("Implement other languages");
            }

            var ignoredKinds = new[]
            {
                SyntaxKind.WhitespaceTrivia, SyntaxKind.EndOfLineTrivia, SyntaxKind.RegionDirectiveTrivia,
                SyntaxKind.EndRegionDirectiveTrivia
            };
            var trivia = node.GetLeadingTrivia().Concat(node.GetTrailingTrivia())
                .Where(t => !ignoredKinds.Contains(t.Kind()))
                .Select(FormatTrivia)
                .Where(s => !string.IsNullOrWhiteSpace(s))
                .ToList();

            return string.Join(Environment.NewLine, trivia);
        }

        public static SyntaxNode CreateCommentNode(SyntaxNode node, string prefix = "CHECK: ")
            => SF.EmptyStatement().WithLeadingTrivia(
                SF.Comment
                (
                    $"/*//{prefix}{node.ToFullString().Replace("*/", "*❤/")}*/"
                )
            ).WithTrailingTrivia(SF.CarriageReturnLineFeed);

        public static TNode RemoveExteriorNonCommentTrivia<TNode>(TNode syntaxNode) where TNode : SyntaxNode
        {
            var newLead = SF.TriviaList(syntaxNode.GetLeadingTrivia().Where(t => t.IsKind(SyntaxKind.MultiLineCommentTrivia) || t.IsKind(SyntaxKind.SingleLineCommentTrivia)));
            var newTrail = SF.TriviaList(syntaxNode.GetTrailingTrivia().Where(t => t.IsKind(SyntaxKind.MultiLineCommentTrivia) || t.IsKind(SyntaxKind.SingleLineCommentTrivia)));

            return syntaxNode.WithLeadingTrivia(newLead).WithTrailingTrivia(newTrail);
        }
        #endregion

        #region Formatting
        public static byte GetElementIndentationLevel(SyntaxNode node)
        {
            static IEnumerable<SyntaxNode> GetHierarchy(SyntaxNode startNode)
            {
                while (startNode.Parent != null)
                {
                    var @base = startNode.Parent;
                    yield return @base;
                    startNode = @base;
                }
            }

            var hierarchy = GetHierarchy(node).Where(n => !(n is CompilationUnitSyntax));
            return (byte)hierarchy.Count();
        }

        public static TNode NormalizeWhitespace<TNode>(TNode baseMethod, SyntaxNode parent, bool indentWithTabs = false) where TNode : SyntaxNode
        {
            parent ??= baseMethod.Parent ?? throw new ArgumentNullException(nameof(parent), $@"{nameof(parent)} has to be not null or {nameof(baseMethod)}.Parent needs to point to not null reference");

            var parentLeadingTrivia = parent is TypeDeclarationSyntax tds && tds.Identifier.TrailingTrivia.Any(SyntaxKind.EndOfLineTrivia) ?
                tds.OpenBraceToken.LeadingTrivia :
                parent.GetLeadingTrivia();

            var lastWhitespaceTrivia = parentLeadingTrivia.LastOrDefault(t => t.Kind() == SyntaxKind.WhitespaceTrivia);
            var singleIndentation = indentWithTabs ? new[] { SF.Tab } : new[] { SF.Space, SF.Space, SF.Space, SF.Space };
            var baseIndentation = lastWhitespaceTrivia == default
                ? singleIndentation
                : new[] { lastWhitespaceTrivia }.Concat(singleIndentation);

            baseMethod = baseMethod.NormalizeWhitespace(indentWithTabs ? "\t" : "    ").WithTriviaFrom(baseMethod);

            return TriviaPrefixer.Prepend(baseMethod, baseIndentation);
        }

        public static TNode NormalizeWhitespace<TNode>(TNode baseMethod, int baseIndentationLevel, bool indentWithTabs = false) where TNode : SyntaxNode
        {
            var singleIndentation = indentWithTabs ? new[] { SF.Tab } : new[] { SF.Space, SF.Space, SF.Space, SF.Space };
            var baseIndentation = Enumerable.Range(1, baseIndentationLevel).SelectMany(i => singleIndentation).ToList().AsReadOnly();

            baseMethod = baseMethod.NormalizeWhitespace(indentWithTabs ? "\t" : "    ").WithTriviaFrom(baseMethod);

            return TriviaPrefixer.Prepend(baseMethod, baseIndentation);
        }
        #endregion

        #region Trivia

        public static T WithPrependedLeadingTrivia<T>(this T node, SyntaxTrivia prependTrivia) where T : SyntaxNode =>
            node.WithLeadingTrivia(node.GetLeadingTrivia().Insert(0, prependTrivia));

        public static T WithPrependedLeadingTrivia<T>(this T node, SyntaxTriviaList prependTriviaList) where T : SyntaxNode
        {
            if (prependTriviaList.Count == 0) return node;
            return node.WithLeadingTrivia(prependTriviaList.Concat(node.GetLeadingTrivia()));
        }

        public static T WithAppendedTrailingTrivia<T>(this T node, SyntaxTriviaList trivia) where T : SyntaxNode => 
            trivia.Count == 0 ? node : node.WithTrailingTrivia(node.GetTrailingTrivia().Concat(trivia));

        [SuppressMessage("ReSharper", "PossibleMultipleEnumeration")]
        public static SyntaxToken WithAppendedTrailingTrivia(this SyntaxToken token, IEnumerable<SyntaxTrivia> trivia) => 
            !trivia.Any() ? token : token.WithTrailingTrivia(token.TrailingTrivia.Concat(trivia));

        #endregion
    }
}
