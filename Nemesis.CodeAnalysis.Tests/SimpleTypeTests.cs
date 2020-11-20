using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using NUnit.Framework;

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
            System.Collections.Specialized.BitVector32 vect = default;

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

            var simpleTypes = variables.Select(v => SimpleType.FromTypeSymbol(v.TypeSymbol)).ToList();

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
    }
}
