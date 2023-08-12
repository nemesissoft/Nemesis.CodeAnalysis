using TCD = NUnit.Framework.TestCaseData;

namespace Nemesis.CodeAnalysis.Tests;

[TestFixture]
class SymbolUtilsTests
{
    private const string CODE = @"using System;
    class TestClass
    {
        private readonly string _readOnly = ""qwe"", _ro2 = ""ASD"";
        private readonly string _eval = string.Concat(""123"" + new TimeSpan(1, 2, 3).ToString(""hhmm""), 666.5);
        private string _roRef = _readOnly;
        //private string _evalRef = _eval + false;
        private long empty;
        private ulong cast = unchecked((ulong)-15);
        private const long NOT_SET = -1;
        private const string strConst = ""Ala has a cat"";
        private long _mPreOpenSliceTimeEndTimeOffsetSecs = NOT_SET;

        private long fromMethod = new TimeSpan(1, 2, 3).TotalHours + TimeSpan.FromHours(5.5).TotalHours;
        

        private int _multiDecl1 = 11, _multiDecl2 = 22, _multiDecl3 = 33, _multiDecl4;
        private string _partMultiDecl1 = null, _partMultiDecl2, _partMultiDecl3 = ""AAA"", _partMultiDecl4 = ""null"";
        private float _standardNumber;
        private float _standardNumberWithInit = 15;
        private const double _d = 3.14;
        
        private readonly string _noInit; 

        public TestClass(){}
        private TestClass(int i, float f){}        
        static TestClass(){}

        public void MethodWithVariableDeclarations()
        {
            var integerInferred = 15;
            var floatInferred = 15.5f;
            int i = 60;
            string s1, s2 = ""ABC"", s3 = null;
            bool noInit;
        }
    }";

    private ClassDeclarationSyntax _class;
    private SemanticModel _model;

    [OneTimeSetUp]
    public void OneTimeSetup()
    {
        var (_, tree, model) = CompilationUtils.CreateTestCompilation(CODE);

        _class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First();

        _model = model;
    }

    private static readonly IEnumerable<TCD> _getFieldMetaTestData = new[]
    {
        new TCD("_multiDecl1", "int", "11"),
        new TCD("_multiDecl2", "int", "22"),
        new TCD("_multiDecl3", "int", "33"),
        new TCD("_multiDecl4", "int", null),

        new TCD("_partMultiDecl1", "string", "null"),
        new TCD("_partMultiDecl2", "string", null),
        new TCD("_partMultiDecl3", "string", "\"AAA\""),
        new TCD("_partMultiDecl4", "string", "\"null\""),

        new TCD("_standardNumber", "float", null),
        new TCD("_standardNumberWithInit", "float", "15"),
        new TCD("_d", "double", "3.14"),
        new TCD("_mPreOpenSliceTimeEndTimeOffsetSecs", "long", "-1"),
        new TCD("NOT_SET", "long", "-1"),
        new TCD("strConst", "string", @"""Ala has a cat"""),
        new TCD("_readOnly", "string", @"""qwe"""),
        new TCD("_roRef", "string", @"""qwe"""),
        new TCD("empty", "long", null),
        new TCD("cast", "ulong", "unchecked((ulong)-15)"),
        new TCD("fromMethod", "long",
            "new TimeSpan(1, 2, 3).TotalHours + TimeSpan.FromHours(5.5).TotalHours"),
        new TCD("_noInit", "string", null),
        /*TODO [TestCase("fromMethod", "6.5341666666666667")]
               [TestCase("_eval", @"""1230102666.5""")]
               [TestCase("_evalRef", @"""1230102666.5false""")]*/
    };
    [TestCaseSource(nameof(_getFieldMetaTestData))]
    public void GetFieldsMeta_ShouldHandleAllFieldTypesAndInitializers(string fieldName, string expectedType, string expectedInit)
    {
        var metas = FieldMeta.GetFieldsMeta(_class, _model).ToList().ToDictionary(m => m.FieldName);

        Assert.That(metas, Does.ContainKey(fieldName), "BAD TEST DATA");

        var meta = metas[fieldName];
        Assert.Multiple(() =>
        {
            Assert.That(meta.Type.Name, Is.EqualTo(expectedType));
            Assert.That(meta.Initializer, Is.EqualTo(expectedInit));
        });
    }

    private static IEnumerable<TCD> GetLocalVariables_ShouldHandleTypes_TestCases() => new (string varName, string expectedType)[]
        {
            ("integerInferred", "int"),
            ("floatInferred", "float"),
            ("i", "int"),
            ("s1", "string"),
            ("s2", "string"),
            ("s3", "string"),
            ("noInit", "bool")
        }.Select(
        (tuple, _) => new TCD(tuple.varName, tuple.expectedType)
    //.SetName($"{tuple.varName} of type {tuple.expectedType}")
    );
    [TestCaseSource(nameof(GetLocalVariables_ShouldHandleTypes_TestCases))]
    public void GetLocalVariables_ShouldHandleTypes(string varName, string expectedType)
    {
        var method = _class.DescendantNodes().OfType<MethodDeclarationSyntax>().First(m => m.Identifier.ValueText == "MethodWithVariableDeclarations");
        var metas = LocalVariableMeta.GetLocalVariables(method, _model).ToList().ToDictionary(m => m.Name);

        Assert.That(metas[varName].Type.Name, Is.EqualTo(expectedType));
    }

    private static IEnumerable<TCD> GetLocalVariables_ShouldHandleInitializers_TestCases()
        => new (string varName, string? expectedInit)[]
        {
            ("integerInferred", "15"),
            ("floatInferred", "15.5f"),
            ("i", "60"),
            ("s1", null),
            ("s2", "\"ABC\""),
            ("s3", "null"),
            ("noInit", null)
        }.Select(
            tuple => new TCD(tuple.varName, tuple.expectedInit)
            //{ TestName = $"{tuple.varName} init with {tuple.expectedInit}" }
            //.SetName($"{tuple.varName} init with {tuple.expectedInit}")
        );
    [TestCaseSource(nameof(GetLocalVariables_ShouldHandleInitializers_TestCases))]
    public void GetLocalVariables_ShouldHandleInitializers(string varName, string? expectedInit)
    {
        var method = _class.DescendantNodes().OfType<MethodDeclarationSyntax>().First(m => m.Identifier.ValueText == "MethodWithVariableDeclarations");
        var metas = LocalVariableMeta.GetLocalVariables(method, _model).ToList().ToDictionary(m => m.Name);

        Assert.That(metas[varName].Initializer, Is.EqualTo(expectedInit));
    }

    [Test]
    public void GetConstructors_ShouldYieldAllConstructors()
    {
        var classSymbol = _model.GetDeclaredSymbol(_class);

        var constructors = classSymbol.GetInstanceConstructors();

        var @params = constructors.Select(c => string.Join(" ", c.Parameters.Select(p => p.Type.Name)));

        Assert.That(@params, Is.EquivalentTo(new[] { "", "Int32 Single" }));
    }



    private static IEnumerable<TCD> GetHierarchyTestCases() => new[]
    {
        new TCD("A", ""),
        new TCD("B", "A"),
        new TCD("C", "B->A"),
        new TCD("Object", "C->B->A"),
        new TCD("Der", "Object->C->B->A"),
        new TCD("Super", ""),
        new TCD("Object,1", "System.Super"),
    };

    private const string HIERARCHY_CODE = @"
                class A { }
                class B:A { }
                class C:B { }
                class Object : C { }
                class Der : Object {}

                namespace System
                {
                    class Super { }
                    class Object : Super { }
                    class Der2 : Object {}
                }";

    private static readonly IEnumerable<TCD> _simpleHierarchyTestData = GetHierarchyTestCases().Concat(new[]
        {
            new TCD("Der2", ""), //no compilation data == no info about suspicious symbols 
        });

    [TestCaseSource(nameof(_simpleHierarchyTestData))]
    public void GetSymbolHierarchy_ShouldReturnHierarchies(string testClass, string expectedHierarchy)
    {
        var (_, tree, model) = CompilationUtils.CreateTestCompilation(HIERARCHY_CODE);

        if (testClass.IndexOf(',') is { } index && int.TryParse(testClass.Substring(index + 1), out var skip))
            testClass = testClass.Substring(0, index);

        var classes = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Where(c => c.Identifier.Text == testClass)
            .Skip(skip);

        var @class = classes.FirstOrDefault() ?? throw new InvalidOperationException("Bad test data in: " + testClass);


        var hierarchy = model.GetDeclaredSymbol(@class).GetSymbolHierarchy();
        var hierarchyText = string.Join("->", hierarchy.Select(h => h.ToDisplayString()));


        Assert.That(hierarchyText, Is.EqualTo(expectedHierarchy));
    }

    private static readonly IEnumerable<TCD> _compilationHierarchyTestData = GetHierarchyTestCases().Concat(new[]
    {
        new TCD("Der2", "System.Object->System.Super"),
    });

    [TestCaseSource(nameof(_compilationHierarchyTestData))]
    public void GetSymbolHierarchy_ShouldReturnHierarchies_BasedOnCompilation(string testClass, string expectedHierarchy)
    {
        var (compilation, tree, model) = CompilationUtils.CreateTestCompilation(HIERARCHY_CODE);

        if (testClass.IndexOf(',') is { } index && int.TryParse(testClass.Substring(index + 1), out var skip))
            testClass = testClass.Substring(0, index);

        var classes = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Where(c => c.Identifier.Text == testClass)
            .Skip(skip);

        var @class = classes.FirstOrDefault() ?? throw new InvalidOperationException("Bad test data in: " + testClass);


        var hierarchy = model.GetDeclaredSymbol(@class).GetSymbolHierarchy(compilation);
        var hierarchyText = string.Join("->", hierarchy.Select(h => h.ToDisplayString()));


        Assert.That(hierarchyText, Is.EqualTo(expectedHierarchy));
    }
}