using System;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using NUnit.Framework;

namespace Nemesis.CodeAnalysis.Tests
{
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
    }
}
