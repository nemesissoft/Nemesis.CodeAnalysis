namespace Nemesis.CodeAnalysis;

internal static class BlockSyntaxExtensions
{
    public static bool TryConvertToExpressionBody(this BlockSyntax block, SyntaxKind declarationKind, LanguageVersion languageVersion, ExpressionBodyPreference preference, out ArrowExpressionClauseSyntax arrowExpression, out SyntaxToken semicolonToken)
    {
        bool IsSupportedInCSharp6() =>
            !(declarationKind is
               SyntaxKind.ConstructorDeclaration or SyntaxKind.DestructorDeclaration or
               SyntaxKind.AddAccessorDeclaration or SyntaxKind.RemoveAccessorDeclaration or
               SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration
            );

        static bool IsWhitespaceOrEndOfLine(SyntaxTrivia trivia) => trivia.IsKind(SyntaxKind.WhitespaceTrivia) || trivia.IsKind(SyntaxKind.EndOfLineTrivia);

        if (preference != ExpressionBodyPreference.Never && block != null && block.Statements.Count == 1)
        {
            var acceptableVersion =
                languageVersion >= LanguageVersion.CSharp7 ||
                languageVersion >= LanguageVersion.CSharp6 && IsSupportedInCSharp6();

            if (acceptableVersion)
            {
                var firstStatement = block.Statements[0];

                if (TryGetExpression(languageVersion, firstStatement, out var expression, out semicolonToken) &&
                    (preference == ExpressionBodyPreference.WhenPossible
                     ||
                     preference == ExpressionBodyPreference.WhenOnSingleLine && SourceUtils.IsOnSingleLine(expression, fullSpan: false)
                    )
                   )
                {
                    arrowExpression = SyntaxFactory.ArrowExpressionClause(expression);

                    // The close brace of the block may have important trivia on it (like comments or directives).  Preserve them on the semicolon when we convert to an expression body.
                    var closeBraceLeadTrivia = block.CloseBraceToken.LeadingTrivia.Where(t => !IsWhitespaceOrEndOfLine(t));
                    semicolonToken = semicolonToken.WithAppendedTrailingTrivia(closeBraceLeadTrivia);
                    return true;
                }
            }
        }

        arrowExpression = null;
        semicolonToken = default;
        return false;
    }

    private static bool TryGetExpression(LanguageVersion version, StatementSyntax firstStatement, out ExpressionSyntax expression, out SyntaxToken semicolonToken)
    {
        if (firstStatement is ExpressionStatementSyntax exprStatement)
        {
            expression = exprStatement.Expression;
            semicolonToken = exprStatement.SemicolonToken;
            return true;
        }
        else if (firstStatement is ReturnStatementSyntax returnStatement)
        {
            if (returnStatement.Expression != null)
            {
                // If there are any comments or directives on the return keyword, move them to
                // the expression.
                expression = firstStatement.GetLeadingTrivia().Any(t => t.IsDirective || t.IsSingleOrMultiLineComment())
                    ? returnStatement.Expression.WithLeadingTrivia(returnStatement.GetLeadingTrivia())
                    : returnStatement.Expression;
                semicolonToken = returnStatement.SemicolonToken;
                return true;
            }
        }
        else if (firstStatement is ThrowStatementSyntax throwStatement)
        {
            if (version >= LanguageVersion.CSharp7 && throwStatement.Expression != null)
            {
                expression = SyntaxFactory.ThrowExpression(throwStatement.ThrowKeyword, throwStatement.Expression);
                semicolonToken = throwStatement.SemicolonToken;
                return true;
            }
        }

        expression = null;
        semicolonToken = default;
        return false;
    }


    public static bool TryConvertToBlock(this ArrowExpressionClauseSyntax arrowExpression, SyntaxToken semicolonToken, bool createReturnStatementForExpression, out BlockSyntax block)
    {
        if (!arrowExpression.TryConvertToStatement(semicolonToken, createReturnStatementForExpression, out var statement))
        {
            block = null;
            return false;
        }

        block = SyntaxFactory.Block(statement);
        return true;
    }

    public static bool TryConvertToStatement(this ArrowExpressionClauseSyntax arrowExpression, SyntaxToken semicolonToken, bool createReturnStatementForExpression, out StatementSyntax statement)
    {
        // It's tricky to convert an arrow expression with directives over to a block. We'd need to find and remove the directives *after* the arrow expression and move them accordingly.  So, for now, we just disallow this.
        if (arrowExpression.Expression.GetLeadingTrivia().Any(t => t.IsDirective))
        {
            statement = null;
            return false;
        }

        statement = ConvertToStatement(arrowExpression.Expression, semicolonToken, createReturnStatementForExpression);
        if (arrowExpression.ArrowToken.TrailingTrivia.Any(t => t.IsSingleOrMultiLineComment()))
        {
            statement = statement.WithPrependedLeadingTrivia(arrowExpression.ArrowToken.TrailingTrivia);
        }

        return true;
    }

    private static StatementSyntax ConvertToStatement(ExpressionSyntax expression, SyntaxToken semicolonToken, bool createReturnStatementForExpression)
    {
        if (expression.IsKind(SyntaxKind.ThrowExpression))
        {
            var throwExpression = (ThrowExpressionSyntax)expression;
            return SyntaxFactory.ThrowStatement(throwExpression.ThrowKeyword, throwExpression.Expression, semicolonToken);
        }
        else if (createReturnStatementForExpression)
        {
            if (expression.GetLeadingTrivia().Any(t => t.IsSingleOrMultiLineComment()))
            {
                return SyntaxFactory.ReturnStatement(expression.WithLeadingTrivia(SyntaxFactory.ElasticSpace))
                                    .WithSemicolonToken(semicolonToken)
                                    .WithLeadingTrivia(expression.GetLeadingTrivia())
                                    .WithPrependedLeadingTrivia(SyntaxFactory.ElasticMarker);
            }
            else
            {
                return SyntaxFactory.ReturnStatement(expression)
                                    .WithSemicolonToken(semicolonToken);
            }
        }
        else
        {
            return SyntaxFactory.ExpressionStatement(expression)
                                .WithSemicolonToken(semicolonToken);
        }
    }
}
