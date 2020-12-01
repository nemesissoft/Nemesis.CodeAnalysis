using System;
using System.Collections.Generic;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using NUnit.Framework;

namespace Nemesis.CodeAnalysis.Tests
{
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
        

        private int _multiDecl1 = 11, _multiDecl2 = 22, _multiDecl3 = 33;
        private string _partMultiDecl1 = null, _partMultiDecl2, _partMultiDecl3 = ""AAA"";
        private float _standardNumber;
        private float _standardNumberWithInit = 15;
        private const double _d = 3.14;

        public TestClass(){}
        private TestClass(int i, float f){}        
        static TestClass(){}

        public void MethodWithVariableDeclarations()
        {
            var integerInferred = 15;
            var floatInferred = 15.5f;
            int i = 60;
            string s1, s2 = ""ABC"", s3 = null;
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

        [TestCase("_multiDecl1", "int")]
        [TestCase("_multiDecl2", "int")]
        [TestCase("_partMultiDecl1", "string")]
        [TestCase("_partMultiDecl2", "string")]
        [TestCase("_standardNumberWithInit", "float")]
        [TestCase("_d", "double")]
        [TestCase("_mPreOpenSliceTimeEndTimeOffsetSecs", "long")]
        [TestCase("NOT_SET", "long")]
        [TestCase("_readOnly", "string")]
        [TestCase("_roRef", "string")]
        [TestCase("empty", "long")]
        [TestCase("cast", "ulong")]
        [TestCase("fromMethod", "long")]
        public void GetFieldMeta_ShouldHandleAllFieldTypes(string fieldName, string expectedType)
        {
            var metas = FieldMeta.GetFieldMeta(_class, _model).ToList().ToDictionary(m => m.FieldName);
            Assert.That(metas[fieldName].Type.Name, Is.EqualTo(expectedType));
        }

        [TestCase("_multiDecl1", "11")]
        [TestCase("_multiDecl2", "22")]
        [TestCase("_multiDecl3", "33")]
        [TestCase("_partMultiDecl1", "null")]
        [TestCase("_partMultiDecl2", null)]
        [TestCase("_partMultiDecl3", "\"AAA\"")]
        [TestCase("_standardNumber", null)]
        [TestCase("_standardNumberWithInit", "15")]
        [TestCase("_d", "3.14")]
        [TestCase("_mPreOpenSliceTimeEndTimeOffsetSecs", "-1")]
        [TestCase("NOT_SET", "-1")]
        [TestCase("strConst", @"""Ala has a cat""")]
        [TestCase("_readOnly", @"""qwe""")]
        //[TestCase("_eval", @"""1230102666.5""")]
        //[TestCase("_evalRef", @"""1230102666.5false""")]
        [TestCase("_roRef", @"""qwe""")]
        [TestCase("empty", null)]
        [TestCase("cast", "unchecked((ulong)-15)")]
        //[TestCase("fromMethod", "6.5341666666666667")]
        public void GetFieldMeta_ShouldHandleInitializers(string fieldName, string expectedInit)
        {
            var metas = FieldMeta.GetFieldMeta(_class, _model).ToList().ToDictionary(m => m.FieldName);

            Assert.That(metas[fieldName].Initializer, Is.EqualTo(expectedInit));
        }

        /*return (from d in data
                         select new TestCaseData(d.type, d.generic).Returns(d.expectedResult)*/

        private static IEnumerable<TestCaseData> GetLocalVariables_ShouldHandleTypes_TestCases() => new (string varName, string expectedType)[]
            {
                ("integerInferred", "int"),
                ("floatInferred", "float"),
                ("i", "int"),
                ("s1", "string"),
                ("s2", "string"),
                ("s3", "string")
            }.Select(
            (tuple, _) => new TestCaseData(tuple.varName, tuple.expectedType)
        //.SetName($"{tuple.varName} of type {tuple.expectedType}")
        );
        [TestCaseSource(nameof(GetLocalVariables_ShouldHandleTypes_TestCases))]
        public void GetLocalVariables_ShouldHandleTypes(string varName, string expectedType)
        {
            var method = _class.DescendantNodes().OfType<MethodDeclarationSyntax>().First(m => m.Identifier.ValueText == "MethodWithVariableDeclarations");
            var metas = LocalVariableMeta.GetLocalVariables(method, _model).ToList().ToDictionary(m => m.Name);

            Assert.That(metas[varName].Type.Name, Is.EqualTo(expectedType));
        }

        private static IEnumerable<TestCaseData> GetLocalVariables_ShouldHandleInitializers_TestCases() => new (string varName, string expectedInit)[]
        {
            ("integerInferred", "15"),
            ("floatInferred", "15.5f"),
            ("i", "60"),
            ("s1", null),
            ("s2", "\"ABC\""),
            ("s3", "null")
        }.Select(
            tuple => new TestCaseData(tuple.varName, tuple.expectedInit)
            //{ TestName = $"{tuple.varName} init with {tuple.expectedInit}" }
            //.SetName($"{tuple.varName} init with {tuple.expectedInit}")
        );
        [TestCaseSource(nameof(GetLocalVariables_ShouldHandleInitializers_TestCases))]
        public void GetLocalVariables_ShouldHandleInitializers(string varName, string expectedInit)
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

        private static IEnumerable<TestCaseData> GetHierarchyTestCases() => new[]
        {
            new TestCaseData("A", ""),
            new TestCaseData("B", "A"),
            new TestCaseData("C", "B->A"),
            new TestCaseData("Object", "C->B->A"),
            new TestCaseData("Der", "Object->C->B->A"),
            new TestCaseData("Super", ""),
            new TestCaseData("Object,1", "System.Super"),
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

        private static readonly IEnumerable<TestCaseData> _simpleHierarchyTestData = GetHierarchyTestCases().Concat(new[]
            {
                new TestCaseData("Der2", ""), //no compilation data == no info about suspicious symbols 
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

        private static readonly IEnumerable<TestCaseData> _compilationHierarchyTestData = GetHierarchyTestCases().Concat(new[]
        {
            new TestCaseData("Der2", "System.Object->System.Super"),
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
}