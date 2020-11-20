using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System.Collections.Generic;

namespace Nemesis.CodeAnalysis
{
    public static class SourceUtils
    {
        public static bool IsOnSingleLine(SyntaxNode node, bool fullSpan)
        {
            // Use an actual Stack so we can write out deeply recursive structures without overflowing.
            // Note: algorithm is taken from GreenNode.WriteTo.
            //
            // General approach is that we recurse down the nodes, using a real stack object to
            // keep track of what node we're on.  If full-span is true we'll examine all tokens
            // and all the trivia on each token.  If full-span is false we'll examine all tokens
            // but we'll ignore the leading trivia on the very first trivia and the trailing trivia
            // on the very last token.
            var stack = new Stack<(SyntaxNodeOrToken nodeOrToken, bool leading, bool trailing)>();
            stack.Push((node, leading: fullSpan, trailing: fullSpan));

            var result = IsOnSingleLine(stack);


            return result;
        }

        private static bool IsOnSingleLine(Stack<(SyntaxNodeOrToken nodeOrToken, bool leading, bool trailing)> stack)
        {
            while (stack.Count > 0)
            {
                var (currentNodeOrToken, currentLeading, currentTrailing) = stack.Pop();

                if (currentNodeOrToken.IsToken)
                {
                    // If this token isn't on a single line, then the original node definitely isn't on a single line.
                    if (!IsOnSingleLine(currentNodeOrToken.AsToken(), currentLeading, currentTrailing))
                    {
                        return false;
                    }
                }
                else
                {
                    var currentNode = currentNodeOrToken.AsNode();

                    var childNodesAndTokens = currentNode.ChildNodesAndTokens();
                    var childCount = childNodesAndTokens.Count;

                    // Walk the children of this node in reverse, putting on the stack to process. This way we process the children in the actual child-order they are in for this node.
                    var index = 0;
                    foreach (var child in childNodesAndTokens.Reverse())
                    {
                        var first = index == 0;
                        var last = index == childCount - 1;

                        // We want the leading trivia if we've asked for it, or if we're not the first token being processed.  We want the trailing trivia if we've asked for it,
                        // or if we're not the last token being processed.
                        stack.Push((child, currentLeading | !first, currentTrailing | !last));
                        index++;
                    }
                }
            }
            // All tokens were on a single line.  This node is on a single line.
            return true;
        }

        private static bool IsOnSingleLine(SyntaxToken token, bool leading, bool trailing)
        {
            // If any of our trivia is not on a single line, then we're not on a single line.
            if (!IsOnSingleLine(token.LeadingTrivia, leading) || !IsOnSingleLine(token.TrailingTrivia, trailing))
                return false;


            // Only string literals can span multiple lines.  Only need to check those.
            if (token.IsKind(SyntaxKind.StringLiteralToken) || token.IsKind(SyntaxKind.InterpolatedStringTextToken))
            {
                // This allocated.  But we only do it in the string case. For all other tokens we don't need any allocations.
                if (!IsOnSingleLine(token.ToString()))
                {
                    return false;
                }
            }
            // Any other type of token is on a single line.
            return true;
        }

        private static bool IsOnSingleLine(SyntaxTriviaList triviaList, bool checkTrivia)
        {
            if (checkTrivia)
            {
                foreach (var trivia in triviaList)
                {
                    if (trivia.HasStructure)
                    {
                        // For structured trivia, we recurse into the trivia to see if it is on a single line or not.  If it isn't, then we're definitely not on a single line.
                        if (!IsOnSingleLine(trivia.GetStructure(), fullSpan: true))
                        {
                            return false;
                        }
                    }
                    else if (trivia.IsKind(SyntaxKind.EndOfLineTrivia))
                    {
                        // Contained an end-of-line trivia.  Definitely not on a single line.
                        return false;
                    }
                    else if (!trivia.IsKind(SyntaxKind.WhitespaceTrivia))
                    {
                        // Was some other form of trivia (like a comment).  Easiest thing
                        // to do is just stringify this and count the number of newlines.
                        // these should be rare.  So the allocation here is ok.
                        if (!IsOnSingleLine(trivia.ToString()))
                        {
                            return false;
                        }
                    }
                }
            }

            return true;
        }

        private static bool IsOnSingleLine(string value) => GetNumberOfLineBreaks(value) == 0;

        private static int GetNumberOfLineBreaks(string text)
        {
            int lineBreaks = 0;
            for (int i = 0; i < text.Length; i++)
            {
                if (text[i] == '\n')
                {
                    lineBreaks++;
                }
                else if (text[i] == '\r')
                {
                    if (i + 1 == text.Length || text[i + 1] != '\n')
                    {
                        lineBreaks++;
                    }
                }
            }

            return lineBreaks;
        }

        //TODO: use IsKind(SyntaxKind.None) ??
        public static bool AreOnSameLine(this SourceText text, SyntaxToken token1, SyntaxToken token2) => 
            token1.RawKind != 0 && token2.RawKind != 0 && text.Lines.IndexOf(token1.Span.End) == text.Lines.IndexOf(token2.SpanStart);

        public static bool AreTwoTokensOnSameLine(SyntaxToken token1, SyntaxToken token2)
        {
            bool ContainsLineBreak(string s)
            {
                foreach (char ch in s)
                    if (ch == '\n' || ch == '\r')
                        return true;

                return false;
            }

            if (token1.SyntaxTree is { } tree && tree.TryGetText(out var text))
                return text.AreOnSameLine(token1, token2);

            return ContainsLineBreak(GetTextBetween(token1, token2));
        }

        public static string GetTextBetween(SyntaxToken token1, SyntaxToken token2)
        {
            var builder = new StringBuilder();
            AppendTextBetween(token1, token2, builder);
            return builder.ToString();
        }

        private static void AppendTextBetween(SyntaxToken token1, SyntaxToken token2, StringBuilder builder)
        {
            //Contract.ThrowIfTrue(token1.RawKind == 0 && token2.RawKind == 0);
            //Contract.ThrowIfTrue(token1.Equals(token2));

            if (token1.RawKind == 0)
            {
                AppendLeadingTriviaText(token2, builder);
                return;
            }

            if (token2.RawKind == 0)
            {
                AppendTrailingTriviaText(token1, builder);
                return;
            }

            var token1PartOftoken2LeadingTrivia = token1.FullSpan.Start > token2.FullSpan.Start;

            if (token1.FullSpan.End == token2.FullSpan.Start)
            {
                AppendTextBetweenTwoAdjacentTokens(token1, token2, builder);
                return;
            }

            AppendTrailingTriviaText(token1, builder);

            for (var token = token1.GetNextToken(includeZeroWidth: true); token.FullSpan.End <= token2.FullSpan.Start; token = token.GetNextToken(includeZeroWidth: true))
            {
                builder.Append(token.ToFullString());
            }

            AppendPartialLeadingTriviaText(token2, builder, token1.TrailingTrivia.FullSpan.End);
        }

        private static void AppendTextBetweenTwoAdjacentTokens(SyntaxToken token1, SyntaxToken token2, StringBuilder builder)
        {
            AppendTrailingTriviaText(token1, builder);
            AppendLeadingTriviaText(token2, builder);
        }

        private static void AppendLeadingTriviaText(SyntaxToken token, StringBuilder builder)
        {
            if (!token.HasLeadingTrivia)
                return;

            foreach (var trivia in token.LeadingTrivia)
                builder.Append(trivia.ToFullString());
        }

        private static void AppendTrailingTriviaText(SyntaxToken token, StringBuilder builder)
        {
            if (!token.HasTrailingTrivia)
                return;

            foreach (var trivia in token.TrailingTrivia)
                builder.Append(trivia.ToFullString());
        }

        /// <summary>If the token1 is expected to be part of the leading trivia of the token2 then the trivia before the token1FullSpanEnd, which the fullspan end of the token1 should be ignored </summary>
        private static void AppendPartialLeadingTriviaText(SyntaxToken token, StringBuilder builder, int token1FullSpanEnd)
        {
            if (!token.HasLeadingTrivia)
                return;

            foreach (var trivia in token.LeadingTrivia)
            {
                if (trivia.FullSpan.End <= token1FullSpanEnd) continue;
                builder.Append(trivia.ToFullString());
            }
        }

        public static bool InBetweenTwoMembers(SyntaxToken previousToken, SyntaxToken currentToken)
        {
            if (previousToken.Kind() != SyntaxKind.SemicolonToken && previousToken.Kind() != SyntaxKind.CloseBraceToken)
                return false;

            if (currentToken.Kind() == SyntaxKind.CloseBraceToken)
                return false;

            var previousMember = GetEnclosingMember(previousToken);
            var nextMember = GetEnclosingMember(currentToken);

            return previousMember != null
                   && nextMember != null
                   && previousMember != nextMember;
        }

        public static MemberDeclarationSyntax GetEnclosingMember(SyntaxToken token)
        {
            if (token.Kind() == SyntaxKind.CloseBraceToken && (token.Parent.Kind() == SyntaxKind.Block || token.Parent.Kind() == SyntaxKind.AccessorList))
                return token.Parent.Parent as MemberDeclarationSyntax;
            return token.Parent.FirstAncestorOrSelf<MemberDeclarationSyntax>();
        }
    }
}
