namespace Nemesis.CodeAnalysis.Tests;

[TestFixture]
public class SyntaxNodeUtilsTests
{
    [Test]
    public static void IsNameOfExpressionTest()
    {
        string code = @"
class Class
{
    void Method()
    {
        int argument = 15;
        Console.WriteLine($@""{ nameof(Console)}.{ nameof(Console.WriteLine)} ({ nameof(argument)})"");
        Console.WriteLine(Sub.nameof(argument));
    }

    //string nameof(object o) => o?.GetType().Name; //This destroys everything - nameof is contextual keyword 
    class Sub
    {
        public static string nameof(object o) => o?.GetType().Name;
    }
}";
        var (_, sourceTree, semanticModel) = CompilationUtils.CreateTestCompilation(code);

        var nameOfs = sourceTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Where(invocation => invocation.IsNameOfExpression(semanticModel))
            .Select(inv => inv.ToString()).ToList();

        Assert.That(nameOfs, Is.EquivalentTo(new[] { "nameof(Console)", "nameof(Console.WriteLine)", "nameof(argument)" }));
    }

    #region Code
    const string REMOVE_FIELD_CODE = @"
class TestClass //class comment
/*class comment 2*/
{
    #region Fields
    
    #region Region to save
    private int we, all/*trail comment1*/, be, deleted/*trail comment2*/;
    #endregion save

    private int they, will, take=15, two, OfUs;
    private int i, am, super, safe=16;
    private int only=1, one=2, 
    #region Region to save 2
                shall=3, 
    #endregion save2           
                stay=4;

    #endregion fields

    public void Method()
    {
        int localVariables, are, similarTo, fieldVariables;
    }

    class InnerClass
    {
        private int innerFieldIsSafe; 
    }
}";
    #endregion
    [TestCase("we,all,be,deleted,two,OfUs,only,shall,stay", ExpectedResult = "they,will,take,i,am,super,safe,one,innerFieldIsSafe")]
    public static string RemoveFieldVariables_ShouldRemoveVariablesProperly_ReturnRemaining(string toRemove)
    {
        var tree = CSharpSyntaxTree.ParseText(REMOVE_FIELD_CODE);
        var syntaxRoot = tree.GetRoot();
        var @class = syntaxRoot.DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.ValueText == "TestClass");

        @class = SyntaxNodeUtils.RemoveFieldVariables(@class, toRemove.Split(','));

        var localVariables = @class.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().SelectMany(local => local.Declaration.Variables.Select(v => v.Identifier.ValueText)).ToList();

        Assert.That(localVariables, Is.EquivalentTo(new[] { "localVariables", "are", "similarTo", "fieldVariables" }));

        return string.Join(",",
            @class.DescendantNodes().OfType<FieldDeclarationSyntax>().SelectMany(local => local.Declaration.Variables.Select(v => v.Identifier.ValueText))
        );
    }

    [Test]
    public static void RemoveFieldVariables_Failures()
    {
        Assert.Throws<ArgumentException>(() => RemoveFieldVariables_ShouldRemoveVariablesProperly_ReturnRemaining("ABC"));
        Assert.Throws<ArgumentException>(() => RemoveFieldVariables_ShouldRemoveVariablesProperly_ReturnRemaining("DEF,we"));
        Assert.Throws<ArgumentException>(() => RemoveFieldVariables_ShouldRemoveVariablesProperly_ReturnRemaining("We"));
        Assert.Throws<ArgumentException>(() => RemoveFieldVariables_ShouldRemoveVariablesProperly_ReturnRemaining(""));
    }

    [Test]
    public static void RemoveFieldVariables_CommentTest()
    {
        var tree = CSharpSyntaxTree.ParseText(REMOVE_FIELD_CODE);
        var syntaxRoot = tree.GetRoot();
        var @class = syntaxRoot.DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.ValueText == "TestClass");

        @class = SyntaxNodeUtils.RemoveFieldVariables(@class, "we,all,be,deleted,two,OfUs,only,shall,stay".Split(','), true);

        var commentTrivia = @class.GetLeadingTrivia().ToFullString().Trim();
        Assert.That(commentTrivia, Is.EqualTo(@"/*we
all/*trail comment1*❤/
be
deleted/*trail comment2*❤/
two
OfUs
only=1
shall=3
stay=4*/"));
    }

    [TestCase("C1,S1,E1,ROS1,RROS1,RS1,RRS1,R1", "First.Second")]
    [TestCase("C2,S2,E2,ROS2,RROS2,RS2,RRS2,R2", "First.Second.Third.Fourth")]
    [TestCase("C3,S3,E3,ROS3,RROS3,RS3,RRS3,R3", "First.Second.Third.Fourth.Fifth")]
    public static void GetNamespace_ShouldReturnValidNamespace_StandardNamespaces(string typeNamesText, string expectedNamespace)
    {
        const string code = """
            namespace First.Second
            {
                class C1 {}
                struct S1 {}
                enum E1 {}
                readonly struct ROS1 {}
                readonly ref struct RROS1 {}
                record struct RS1 {}
                readonly record struct RRS1 {}
                record R1 {}

                namespace Third.Fourth 
                {
                    class C2 {}
                    struct S2 {}
                    enum E2 {}
                    readonly struct ROS2 {}
                    readonly ref struct RROS2 {}
                    record struct RS2 {}
                    readonly record struct RRS2 {}
                    record R2 {}

                    namespace Fifth 
                    {
                        class C3 {}
                        struct S3 {}
                        enum E3 {}
                        readonly struct ROS3 {}
                        readonly ref struct RROS3 {}
                        record struct RS3 {}
                        readonly record struct RRS3 {}
                        record R3 {}                    
                    }
                }
            }
            """;
        GetNamespace_ShouldReturnValidNamespace_Helper(typeNamesText, expectedNamespace, code);
    }

    [TestCase("C1,S1,E1,ROS1,RROS1,RS1,RRS1,R1", "FileScoped")]
    public static void GetNamespace_ShouldReturnValidNamespace_FileScopedNamespaces(string typeNamesText, string expectedNamespace)
    {
        const string code = """
            namespace FileScoped;
            
            class C1 {}
            struct S1 {}
            enum E1 {}
            readonly struct ROS1 {}
            readonly ref struct RROS1 {}
            record struct RS1 {}
            readonly record struct RRS1 {}
            record R1 {}
            """;
        GetNamespace_ShouldReturnValidNamespace_Helper(typeNamesText, expectedNamespace, code);
    }

    [TestCase("C1,S1,E1,ROS1,RROS1,RS1,RRS1,R1", "")]
    public static void GetNamespace_ShouldReturnValidNamespace_DefaultNamespaces(string typeNamesText, string expectedNamespace)
    {
        const string code = """
            class C1 {}
            struct S1 {}
            enum E1 {}
            readonly struct ROS1 {}
            readonly ref struct RROS1 {}
            record struct RS1 {}
            readonly record struct RRS1 {}
            record R1 {}
            """;
        GetNamespace_ShouldReturnValidNamespace_Helper(typeNamesText, expectedNamespace, code);
    }

    private static void GetNamespace_ShouldReturnValidNamespace_Helper(string typeNamesText, string expectedNamespace, string code)
    {
        var (_, sourceTree, _) = CompilationUtils.CreateTestCompilation(code);

        var typeNames = typeNamesText.Split(',');

        var types = sourceTree.GetRoot().DescendantNodes().OfType<BaseTypeDeclarationSyntax>()
            .Where(t => typeNames.Contains(t.Identifier.Text))
            .ToList();
        var namespaces = types.Select(t => t.GetNamespace()).ToList();

        Assert.That(namespaces, Is.All.EqualTo(expectedNamespace));
    }
}
