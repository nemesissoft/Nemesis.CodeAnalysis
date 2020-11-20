using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SF = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Nemesis.CodeAnalysis
{
    internal class ExceptionFilterRewriter : CSharpSyntaxRewriter
    {
        private readonly string _loggerName;//TODO use symbol browser
        private readonly string[] _logMethodNames;

        public ExceptionFilterRewriter(string loggerName, string[] logMethodNames, bool visitIntoStructuredTrivia = false) : base(visitIntoStructuredTrivia)
        {
            _loggerName = loggerName;
            _logMethodNames = logMethodNames;
        }
        
        public override SyntaxNode VisitCatchClause(CatchClauseSyntax @catch)
        {
            if (@catch.Filter is null)
            {
                var loggerInvocationStatement = @catch.Block.Statements
                    .OfType<ExpressionStatementSyntax>().FirstOrDefault(e =>
                        e.Expression is InvocationExpressionSyntax i && //filter by expression not statement

                        i.Expression is MemberAccessExpressionSyntax mae && //only take instance call access

                        mae.Expression is IdentifierNameSyntax ins &&
                        ins.Identifier.ValueText == _loggerName && // == "_logger" <- better use symbol finder here 

                        mae.Name.Identifier.ValueText is { } methodName &&
                        _logMethodNames.Contains(methodName) //_logMethodNames == new[] { "LogError" }
                    );
                if (loggerInvocationStatement != null)
                {
                    //exact: _logger.LogError(e, "Unexpected error.")
                    var filterText = loggerInvocationStatement.NormalizeWhitespace().ToString();

                    //remove
                    var newBlock = @catch.Block.RemoveNode(loggerInvocationStatement, SyntaxRemoveOptions.KeepNoTrivia);
                    //immutability !!!
                    @catch = @catch.WithBlock(newBlock);


                    var filterInvocation = SF.ParseExpression(filterText);
                    //Handle(()=>_logger.LogError(e, "Unexpected error."))
                    var filterBody = SF.InvocationExpression(
                            SF.IdentifierName("Handle"))
                        .WithArgumentList(
                            SF.ArgumentList(
                                SF.SingletonSeparatedList(
                                    SF.Argument(
                                        SF.ParenthesizedLambdaExpression(
                                            filterInvocation)
                                        ))));
                    //when(Handle(()=>_logger.LogError(e, "Unexpected error.")))
                    var filter = SF.CatchFilterClause(filterBody);

                    //immutability !!!
                    @catch = @catch.WithFilter(filter);
                }
            }
            return base.VisitCatchClause(@catch);
        }
    }
}
