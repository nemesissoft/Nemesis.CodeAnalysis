using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using NUnit.Framework;

#nullable enable

namespace Nemesis.CodeAnalysis.Tests
{
    [TestFixture]
    public class SimpleTypeTests
    {
        [Test]
        public static void ShouldHandleVariousTypeNames()
        {
            const string SOURCE = @"
using System;
using System.Collections.Generic;

namespace SimpleNamespace
{
    class Types
    {
        void Method()
        {
            dynamic d = 15;

            int i1 = default;
            Int32 i2 = default;

            string s1 = default;
            String s2 = default;

            List<string> l1 = default;
            List<String> l2 = default;
            List<int?> l3 = default;
            List<Int32?> l4 = default;

            IList<string> il1 = default;
            System.Collections.Generic.IList<string> il2 = default;
            System.Collections.Specialized.BitVector32 vector = default;

            byte[][] jagged = default;
            byte[][][,][] jagged2 = default;
            string[] singleDim = default;
            int[,] twoDim = default;
            float[,,] multiDim = default;

            var anon = new {Number = 15};
            (int, int, int) tuple = (1, 2, 3);
            Function<int, string> func=default;
            Func<int, string> func2=default;

            IList<Dictionary<Int64, System.Collections.Specialized.BitVector32[]>> compound = default;

            Console.WriteLine($@""dynamic { d}, { nameof(i1)}: { i1}, { nameof(i2)}: { i2}, { nameof(s1)}: { s1}, { nameof(s2)}: { s2}, { nameof(l1)}: { l1}, { nameof(l2)}: { l2}, { nameof(l3)}: { l3}, { nameof(l4)}: { l4}, { nameof(il1)}: { il1}, { nameof(il2)}: { il2}, { nameof(vect)}: { vect}, { nameof(jagged)}: { jagged}, { nameof(jagged2)}: { jagged2}, { nameof(singleDim)}: { singleDim}, { nameof(twoDim)}: { twoDim}, { nameof(multiDim)}: { multiDim}, { nameof(compound)}: { compound}"");
        }

        delegate TResult Function<in T, out TResult>(T arg);
    }
}
";
            var (_, sourceTree, semanticModel) = CompilationUtils.CreateTestCompilation(SOURCE, new[] { typeof(SyntaxTree).Assembly, typeof(CSharpSyntaxTree).Assembly, typeof(System.Collections.Specialized.BitVector32).Assembly });

            var variables =
                sourceTree.GetRoot().DescendantNodes().OfType<VariableDeclarationSyntax>()
                    .Select(declaration =>
                        (Name: declaration.Variables.Single().Identifier.ValueText, TypeSymbol: semanticModel.GetTypeInfo(declaration.Type).Type)
                    ).ToList();

            var simpleTypes = variables.Select(v => v.TypeSymbol is { } ts ? SimpleType.FromTypeSymbol(ts) : default).ToList();

            var simpleTypesTexts = simpleTypes.Select(st => st.ToString()).ToList();

            Assert.That(simpleTypesTexts, Is.EquivalentTo(new[]
            {
                "dynamic",
                "System.int", "System.int", "System.string", "System.string",
                "System.Collections.Generic.List<string>", "System.Collections.Generic.List<string>", "System.Collections.Generic.List<int?>", "System.Collections.Generic.List<int?>", "System.Collections.Generic.IList<string>", "System.Collections.Generic.IList<string>",
                "System.Collections.Specialized.BitVector32",
                "System.byte[][]", "System.byte[][][,][]", "System.string[]", "System.int[,]", "System.float[,,]",
                "<global namespace>.<anonymous type: int Number>", "System.(int, int, int)",
                "SimpleNamespace.Function<int, string>", "System.Func<int, string>",
                "System.Collections.Generic.IList<Dictionary<long, BitVector32[]>>"
            }));
        }

        private const string NAMESPACE_EXTRACT_CODE = @"
namespace NumberNamespace
{
    readonly struct Number { }
}

namespace CollectionNamespace
{
    class NumberCollection: System.Collections.ObjectModel.Collection<NumberNamespace.Number> { }
}

namespace NullableNamespace
{
    readonly struct MyNullable<T> where T:struct { }
}

namespace TestNamespace
{
    class Test
    {
        public System.Numerics.BigInteger Bi;
        public NumberNamespace.Number N1;
        public NumberNamespace.Number[] A1;
        public CollectionNamespace.NumberCollection Nc1;
        public System.Collections.ObjectModel.Collection<NumberNamespace.Number?> C_N_N;
        public System.Collections.ObjectModel.Collection<NullableNamespace.MyNullable<NumberNamespace.Number>[]> C_N_M_N;
    }
}";

        private IReadOnlyCollection<(string Name, ITypeSymbol TypeSymbol)> _namespaceFields = Array.Empty<(string, ITypeSymbol)>();

        [OneTimeSetUp]
        public void BeforeAnyTests()
        {
            var (_, tree, semanticModel) = CompilationUtils.CreateTestCompilation(NAMESPACE_EXTRACT_CODE, new[] { typeof(BigInteger).Assembly });

            var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First(cds => cds.Identifier.ValueText == "Test");
            _namespaceFields = @class.ChildNodes().OfType<FieldDeclarationSyntax>()
                .Select(f => f.Declaration)
                .SelectMany(decl => decl.Variables.Select(v =>
                    (v.Identifier.Text, semanticModel.GetTypeInfo(decl.Type).Type ?? throw new NotSupportedException("No type info")))
                )
                .ToList();
        }

        [TestCase("Bi", "System.Numerics")]
        [TestCase("N1", "NumberNamespace")]
        [TestCase("A1", "NumberNamespace;System")]
        [TestCase("Nc1", "CollectionNamespace")]
        [TestCase("C_N_N", "NumberNamespace;System;System.Collections.ObjectModel")]
        [TestCase("C_N_M_N", "NumberNamespace;System;System.Collections.ObjectModel;NullableNamespace")]
        public void ExtractNamespaces_ShouldExtractNestedNamespaces(string symbolName, string expectedNamespacesText)
        {
            var expectedNamespaces = new SortedSet<string>(expectedNamespacesText.Split(';'));
            var symbolMeta = _namespaceFields.SingleOrDefault(p => p.Name == symbolName);
            Assert.That(symbolName, Is.Not.Null, "Initialization error");
            var namespaces = new SortedSet<string>();


            SimpleType.ExtractNamespaces(symbolMeta.TypeSymbol, namespaces);


            Assert.That(namespaces, Is.EquivalentTo(expectedNamespaces));
        }
    }
}
