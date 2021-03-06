﻿using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using SF = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using NUnit.Framework;

namespace Nemesis.CodeAnalysis.Tests
{
    [TestFixture]
    public class TriviaUtilsTests
    {
        private const string CODE = @"using System;
class BareClass
{
    public BareClass() { Console.WriteLine(123);Console.WriteLine(456);while(false)Console.WriteLine(789); }
    
    private int j;
    public int I1 { get; set; }
    public int I2 { get { return j; } set { j = value; } }
    public int I3 => j;
}

namespace Blah
{
	    	class ClassInNamespace  {
        public ClassInNamespace() {Console.WriteLine(123); }

        #region MyRegion
        public ClassInNamespace(int i) { if(true)_i = i; else {_i=16;} }

        private readonly int _i;
        #endregion

        class InnerClass
        {
            public InnerClass() 
            { 
                try { Console.WriteLine('A'); }
                catch (Exception e)
                {
				            Console.WriteLine(e);
          throw;
                }
            }

            //expression style
            public InnerClass(int j) =>               /*body comment*/    _j = j;

            private readonly int _j;
        }

#pragma warning disable 414, CS3021
        public ClassInNamespace(float f) { if(true)_i = (int)f; else {_i=16;} }
#pragma warning restore CS3021

    }
}";
        private IReadOnlyList<ConstructorDeclarationSyntax> _ctors;
        private IReadOnlyList<PropertyDeclarationSyntax> _props;
        //private SemanticModel _model;

        [OneTimeSetUp]
        public void OneTimeSetup()
        {
            var tree = CSharpSyntaxTree.ParseText(CODE);
            var syntaxRoot = tree.GetRoot();
            _ctors = syntaxRoot.DescendantNodes().OfType<ConstructorDeclarationSyntax>().ToList().AsReadOnly();
            _props = syntaxRoot.DescendantNodes().OfType<PropertyDeclarationSyntax>().ToList().AsReadOnly();
        }

        private const string NEW_LINE = "\r\n";
        private const string TAB1 = "\t";
        private const string TAB2 = "\t\t";
        private const string TAB3 = "\t\t\t";
        private const string TAB4 = "\t\t\t\t";
        private const string TAB5 = "\t\t\t\t\t";
        private const string THIN_SPACE = "\u202F";

        [TestCase(0, 1)]
        [TestCase(1, 2)]
        [TestCase(2, 2)]
        [TestCase(3, 3)]
        [TestCase(4, 3)]
        [TestCase(5, 2)]
        public void GetElementIndentationLevel_ShouldReturnIndentation(int ctorNumber, int expectedIndentation)
        {
            var ctor = _ctors[ctorNumber];

            var result = TriviaUtils.GetElementIndentationLevel(ctor);

            Assert.That(result, Is.EqualTo((byte)expectedIndentation));
        }

        private static IEnumerable<TestCaseData> NormalizeWhitespaceDataCtors()
        {
            var expectedResults = new[]
            {
                  $"{TAB1}public BareClass()" + NEW_LINE
                + $"{TAB1}{{" + NEW_LINE
                + $"{TAB2}Console.WriteLine(123);" + NEW_LINE
                + $"{TAB2}Console.WriteLine(456);" + NEW_LINE
                + $"{TAB2}while (false)" + NEW_LINE
                + $"{TAB3}Console.WriteLine(789);" + NEW_LINE
                + $"{TAB1}}}"
                ,
                  $"{TAB2}public ClassInNamespace()" + NEW_LINE
                + $"{TAB2}{{" + NEW_LINE
                + $"{TAB3}Console.WriteLine(123);" + NEW_LINE
                + $"{TAB2}}}"
                ,
                NEW_LINE + $"{TAB2}#region MyRegion" + NEW_LINE
                + $"{TAB2}public ClassInNamespace(int i)" + NEW_LINE
                + $"{TAB2}{{" + NEW_LINE
                + $"{TAB3}if (true)" + NEW_LINE
                + $"{TAB4}_i = i;" + NEW_LINE
                + $"{TAB3}else" + NEW_LINE
                + $"{TAB3}{{" + NEW_LINE
                + $"{TAB4}_i = 16;" + NEW_LINE
                + $"{TAB3}}}" + NEW_LINE
                + $"{TAB2}}}"
                ,
                  $"{TAB3}public InnerClass()" + NEW_LINE
                + $"{TAB3}{{" + NEW_LINE
                + $"{TAB4}try" + NEW_LINE
                + $"{TAB4}{{" + NEW_LINE
                + $"{TAB5}Console.WriteLine('A');" + NEW_LINE
                + $"{TAB4}}}" + NEW_LINE
                + $"{TAB4}catch (Exception e)" + NEW_LINE
                + $"{TAB4}{{" + NEW_LINE
                + $"{TAB5}Console.WriteLine(e);" + NEW_LINE
                + $"{TAB5}throw;" + NEW_LINE
                + $"{TAB4}}}" + NEW_LINE
                + $"{TAB3}}}"
                ,
                NEW_LINE + $"{TAB3}//expression style" + NEW_LINE
                + $"{TAB3}public InnerClass(int j) => /*body comment*/ _j = j;"
            };

            return
                (from i in Enumerable.Range(0, expectedResults.Length)
                 let result = expectedResults[i]
                 let root = CSharpSyntaxTree.ParseText(result).GetRoot()
                 let flattenedRootText = TriviaFlattener.Flatten(root).ToFullString()
                 let name = flattenedRootText
                    .Replace("\r\n", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\r", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\n", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\t", "↦") // NL = \u23CE  TAB = \u21A6    THIN_SPACE = 8239
                 select new TestCaseData(i).Returns(result)
                 //.SetName($"{i}. {name})")
                ).ToList();
        }

        [TestCaseSource(nameof(NormalizeWhitespaceDataCtors))]
        public string NormalizeWhitespace_ShouldNormalizeDependingOnBaseIndentationCtors(int ctorNumber)
        {
            var ctor = _ctors[ctorNumber];

            var indentationLevel = TriviaUtils.GetElementIndentationLevel(ctor);

            var result = TriviaUtils.NormalizeWhitespace(ctor, indentationLevel, true).ToFullString();

            Assert.That(result, Does.EndWith("\r\n"));

            return result.Substring(0, result.Length - 2);
        }


        private static IEnumerable<TestCaseData> NormalizeWhitespaceDataProps()
        {
            var expectedResults = new[]
            {
                  $"{TAB1}public int I1" + NEW_LINE
                + $"{TAB1}{{" + NEW_LINE
                + $"{TAB2}get;" + NEW_LINE
                + $"{TAB2}set;" + NEW_LINE
                + $"{TAB1}}}"
                ,
                  $"{TAB1}public int I2" + NEW_LINE
                + $"{TAB1}{{" + NEW_LINE
                + $"{TAB2}get" + NEW_LINE
                + $"{TAB2}{{" + NEW_LINE
                + $"{TAB3}return j;" + NEW_LINE
                + $"{TAB2}}}" + NEW_LINE
                + NEW_LINE
                + $"{TAB2}set" + NEW_LINE
                + $"{TAB2}{{" + NEW_LINE
                + $"{TAB3}j = value;" + NEW_LINE
                + $"{TAB2}}}" + NEW_LINE
                + $"{TAB1}}}"
                ,
                  $"{TAB1}public int I3 => j;"
            };

            return
                (from i in Enumerable.Range(0, expectedResults.Length)
                 let result = expectedResults[i]
                 let root = CSharpSyntaxTree.ParseText(result).GetRoot()
                 let flattenedRootText = TriviaFlattener.Flatten(root).ToFullString()
                 let name = flattenedRootText
                    .Replace("\r\n", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\r", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\n", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\t", "↦") // NL = \u23CE  TAB = \u21A6    THIN_SPACE = 8239
                 select new TestCaseData(i).Returns(result)
                 //.SetName($"Normalize properties {i}. {name})")
                ).ToList();
        }

        [TestCaseSource(nameof(NormalizeWhitespaceDataProps))]
        public string NormalizeWhitespace_ShouldNormalizeDependingOnBaseIndentationProps(int propNumber)
        {
            var prop = _props[propNumber];

            var indentationLevel = TriviaUtils.GetElementIndentationLevel(prop);

            var result = TriviaUtils.NormalizeWhitespace(prop, indentationLevel, true).ToFullString();

            Assert.That(result, Does.EndWith("\r\n"));

            return result.Substring(0, result.Length - 2);
        }

        [Test]
        public void NormalizeWhitespace_ShouldNormalizeDependingOriginalParentIndentation()
        {
            var ctor = _ctors[2];

            var result = TriviaUtils.NormalizeWhitespace(ctor, null/*ctor.Parent*/, true).ToFullString();

            var expected =
                  NEW_LINE
              + $"{TAB1}    {TAB2}#region MyRegion" + NEW_LINE
              + $"{TAB1}    {TAB2}public ClassInNamespace(int i)" + NEW_LINE
              + $"{TAB1}    {TAB2}{{" + NEW_LINE
              + $"{TAB1}    {TAB3}if (true)" + NEW_LINE
              + $"{TAB1}    {TAB4}_i = i;" + NEW_LINE
              + $"{TAB1}    {TAB3}else" + NEW_LINE
              + $"{TAB1}    {TAB3}{{" + NEW_LINE
              + $"{TAB1}    {TAB4}_i = 16;" + NEW_LINE
              + $"{TAB1}    {TAB3}}}" + NEW_LINE
              + $"{TAB1}    {TAB2}}}"
              + NEW_LINE;

            Assert.That(result, Is.EqualTo(expected));
        }

        [Test]
        public void NormalizeWhitespace_ShouldNotNormalizePragmas()
        {
            var ctor = _ctors[5];

            var result = TriviaUtils.NormalizeWhitespace(ctor, null/*ctor.Parent*/, true).ToFullString();

            /*"\r\n#pragma warning disable 414, CS3021\r\n\t    \t\tpublic */

            var expected =
                  NEW_LINE
              + "#pragma warning disable 414, CS3021" + NEW_LINE

              + $"{TAB1}    {TAB2}public ClassInNamespace(float f)" + NEW_LINE
              + $"{TAB1}    {TAB2}{{" + NEW_LINE
              + $"{TAB1}    {TAB3}if (true)" + NEW_LINE
              + $"{TAB1}    {TAB4}_i = (int)f;" + NEW_LINE
              + $"{TAB1}    {TAB3}else" + NEW_LINE
              + $"{TAB1}    {TAB3}{{" + NEW_LINE
              + $"{TAB1}    {TAB4}_i = 16;" + NEW_LINE
              + $"{TAB1}    {TAB3}}}" + NEW_LINE
              + $"{TAB1}    {TAB2}}}"
              + NEW_LINE;

            Assert.That(result, Is.EqualTo(expected));
        }
    }
}
