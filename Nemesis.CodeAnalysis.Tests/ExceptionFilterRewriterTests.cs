using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SF = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using NUnit.Framework;

namespace Nemesis.CodeAnalysis.Tests
{
    [TestFixture]
    public class ExceptionFilterRewriterTests
    {
        [Test]
        public void ParseTest()
        {
            const string CODE = @"
class Program { 
    static void Main()
    {
        try
        {
            
        }
        catch (Exception e) 
        {
            _logger.LogError(e, ""Unexpected error."")
            return null;
        }
    }
}
";
            var tree = CSharpSyntaxTree.ParseText(CODE);

            var root = tree.GetRoot();

            var @try = root.DescendantNodes().OfType<TryStatementSyntax>().Single();

            var newTry = new ExceptionFilterRewriter("_logger", new[] { "LogError" }).Visit(@try).NormalizeWhitespace().ToFullString();
            /* RESULT:
            try
            {
            }
            catch (Exception e)when (Handle(() => _logger.LogError(e, "Unexpected error.")))
            {
                return null;
            }
                         */
        }
    }
}
