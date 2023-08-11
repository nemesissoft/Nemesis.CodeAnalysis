namespace Nemesis.CodeAnalysis.Tests;

[TestFixture]
public class CompilationUtilsTests
{
    private static IEnumerable<TestCaseData> GetDataFor_TryEvaluateExpression()
    {
        var data = new (string In, string Out, bool ExpectedSuccess)[]
        {
            ("1 + 2", "3", true),
            ("null + 5", null, true),
            ("null + \"5\"", "5", true),
            ("\"1\" + \"2\"", "12", true),
            ("\"1\" + 2", "12", true),
            ("London + 5", "THROW", false),
        };
        return data.Select((d, i) => new TestCaseData(d.In, d.Out).Returns(d.ExpectedSuccess)
            //.SetName($"{i + 1:00}. {d.In} => {d.Out ?? "∅"} {(d.ExpectedSuccess ? "positive" : "negative")}")
            ).ToList();
    }
    [TestCaseSource(nameof(GetDataFor_TryEvaluateExpression))]
    public bool TryEvaluateExpressionTests(string code, string expectedResult)
    {
        bool success = CompilationUtils.TryEvaluateExpression(code, out var result);

        if (success)
            Assert.That(result, Is.EqualTo(expectedResult));
        else
            Assert.That(result, Does.StartWith("<error in"));

        return success;
    }

    private static IEnumerable<TestCaseData> GetDataFor_GetTypeFromCSharpName()
    {
        var data = new (string In, Type Out)[]
        {
            ("bool", typeof(bool)),
            ("System.Boolean", typeof(bool)),
            ("int", typeof(int)),
            ("string", typeof(string)),
            ("void", typeof(void)),
            ("decimal", typeof(decimal)),
            ("System.Activator", typeof(Activator)),
            ("int[]", typeof(int[])),
            ("int[,]", typeof(int[,])),
            ("int[,][,][][,][][]", typeof(int[,][,][][,][][]))
        };
        return data.Select(d => new TestCaseData(d.In).Returns(d.Out)
        //.SetName($"{d.In} => {d.Out}")
        ).ToList();
    }

    [TestCaseSource(nameof(GetDataFor_GetTypeFromCSharpName))]
    public static Type TypeFromStringUsingScripts(string typeName) => CompilationUtils.GetTypeFromCSharpName(typeName);
}
