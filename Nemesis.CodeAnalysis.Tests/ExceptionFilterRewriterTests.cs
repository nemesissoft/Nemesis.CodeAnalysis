namespace Nemesis.CodeAnalysis.Tests;

[TestFixture]
public class ExceptionFilterRewriterTests
{
    [Test]
    public void ShouldRewriteToExceptionFilter()
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
        const string EXPECTED_CODE = """
            try
            {
            }
            catch (Exception e)when (Handle(() => _logger.LogError(e, "Unexpected error.")))
            {
                return null;
            }
            """;
        var tree = CSharpSyntaxTree.ParseText(CODE);

        var root = tree.GetRoot();

        var @try = root.DescendantNodes().OfType<TryStatementSyntax>().Single();

        var actual = new ExceptionFilterRewriter("_logger", new[] { "LogError" }).Visit(@try).NormalizeWhitespace().ToFullString();

        Assert.That(actual, Is.EqualTo(EXPECTED_CODE));
    }
}
