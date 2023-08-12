using System.Diagnostics;
using SF = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Nemesis.CodeAnalysis.Sample;

partial class Roslyn
{
    /*[SuppressMessage("ReSharper", "MemberCanBePrivate.Local")]
    [SuppressMessage("ReSharper", "UnusedAutoPropertyAccessor.Local")]
    [SuppressMessage("ReSharper", "AutoPropertyCanBeMadeGetOnly.Local")]
    [SuppressMessage("ReSharper", "UnusedMember.Local")]
    class ConfigModelWithPublicSetters
    {
        public string Text { get; set; }
        public int Number { get; set; }
        public TimeSpan Duration { get; set; }
        public TimeSpan NoSetter => TimeSpan.MaxValue;
        public TimeSpan GetOnlyProp { get; } = TimeSpan.MaxValue;

        public ConfigModelWithPublicSetters(string text, int number, TimeSpan duration)
        {
            Text = text;
            Number = number;
            Duration = duration;
        }
    }

    // ReSharper disable once UnusedMember.Local
    [SuppressMessage("ReSharper", "UnusedMember.Local")]
    class ConfigModelWithPublicSettersUnproperUse
    {
        public static void UnproperUse()
        {
            var t = new ConfigModelWithPublicSetters("123", 15, TimeSpan.Zero) { Number = 16 };
        }

        public static void ProperUseThroughReflection()
        {
            var t = new ConfigModelWithPublicSetters("123", 15, TimeSpan.Zero);
            (t.GetType().GetProperty(nameof(ConfigModelWithPublicSetters.Number)) ?? throw new InvalidOperationException($"Property {nameof(ConfigModelWithPublicSetters.Number)} does not exist"))
                .SetValue(t, 17);
        }
    }

    [Test, Ignore("TODO")]
    public static void UnproperUseOfSettersHunter()
    {
        var projPath = Path.Combine(TestContext.CurrentContext.TestDirectory, @"..\..\Nemesis_Education.csproj");
        
        var msbws = MSBuildWorkspace.Create();
        var project = msbws.OpenProjectAsync(projPath).Result;
        var compilation = project.GetCompilationAsync().Result;

        var configModelSymbol = compilation.GetTypeByMetadataName(typeof(ConfigModelWithPublicSetters).FullName);

        var semModels = compilation.SyntaxTrees.Select(synTree => compilation.GetSemanticModel(synTree, true));

        var classes = semModels.SelectMany(
            sm => sm.SyntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>(),
            (sm, @class) => (Class: @class, Symbol: sm.GetDeclaredSymbol(@class), SemanticModel: sm)
        );

        var testClass = classes.First(pair => Equals(pair.Symbol, configModelSymbol));

        var setters = testClass.Class.DescendantNodes().OfType<PropertyDeclarationSyntax>()
            .Select(prop => prop.AccessorList?.Accessors)
            .Where(a => a.HasValue)
            .SelectMany(ads => ads.Value.Where(ad => ad.Kind() == SyntaxKind.SetAccessorDeclaration))
            .Select(ads => (Setter: ads, Symbol: testClass.SemanticModel.GetDeclaredSymbol(ads)))
            .ToList();

        var setters2 = testClass.Class.DescendantNodes().OfType<PropertyDeclarationSyntax>()
            .Select(prop => testClass.SemanticModel.GetDeclaredSymbol(prop).SetMethod)
            .Where(setter => setter != null)
            .ToList();

        //var setterReferences = setters.Select(setter => SymbolFinder.FindReferencesAsync(setter.Symbol, project.Solution).Result.ToList()).ToList();
        var setterReferences = setters.Select(setter => SymbolFinder.FindCallersAsync(setter.Symbol, project.Solution).Result.ToList()).ToList();


        var setterReferences2 = setters2.Select(setter => SymbolFinder.FindCallersAsync(setter, project.Solution).Result.ToList()).ToList();
    }*/


    public static void CtorExpressionAssignments()
    {
        string code = @"class Base
        {
            protected int _iB = 15;

            public Base()
            {
                _iB = 16;
                Console.WriteLine('1'+_iB);
            }
        }

        class Der : Base
        {
            private float _fD = 3.14f;

            public Der():base()
            {
                _iB = 18;
                _fD = GetF();
                Console.WriteLine('1' + _fD);
            }

            private static float GetF() => 15;

            public Der(int i)
            {
                _iB = i; int j; j = i;
            }
        }";

        var (_, tree, model) = CompilationUtils.CreateTestCompilation(code);

        var syntaxRoot = tree.GetRoot();
        var classes = syntaxRoot.DescendantNodes().OfType<ClassDeclarationSyntax>();
        var ctors = classes.SelectMany(c => c.DescendantNodes().OfType<ConstructorDeclarationSyntax>()
            .Where(ctor => ctor.ParameterList.Parameters.Count == 0));

        var stats = ctors.Where(c => c.Body is not null)
            .SelectMany(c => c.Body!.Statements)
            .OfType<ExpressionStatementSyntax>()
            .Select(stat => stat.Expression)
            .OfType<AssignmentExpressionSyntax>();

        var fieldsToAss =
            stats.Select(s => (Statement: s, Symbol: model.GetSymbolInfo(s.Left).Symbol as IFieldSymbol))
                .Where(ss => ss.Symbol is not null)
                .Select(ss => (Field: ss.Symbol!.Name, Assignment: ss.Statement.Right.ToString()))
                .ToList();

        /*Assert.That(fieldsToAss, Is.EquivalentTo(new[]
        {
            ("_iB", "16"),
            ("_iB", "18"),
            ("_fD", "GetF()")
        }.ToList()
        ));*/
    }


    public static void TreeVisitorTests()
    {
        var tree = CSharpSyntaxTree.ParseText(@"public class MyClass { public void MyMethod() {  }  public void MyMethod(int n) {  } }");

        var walker = new CustomWalker();
        walker.Visit(tree.GetRoot());

        Console.WriteLine();

        DeeperWalker.VisitTree(tree);

        Console.WriteLine();

        var twoClassesTree = CSharpSyntaxTree.ParseText(@"public class MyClass { public void MyMethod() { } }   public class MyOtherClass { public void MyMethod(int n) { } }");

        var classWalker = new ClassMethodWalker();
        classWalker.Visit(twoClassesTree.GetRoot());
    }

    public class CustomWalker : CSharpSyntaxWalker
    {
        static int _tabs;
        public override void Visit(SyntaxNode? node)
        {
            _tabs++;
            if (node is not null)
            {
                var indents = new string('\t', _tabs);
                Console.WriteLine(indents + node.Kind());
            }
            base.Visit(node);
            _tabs--;
        }
    }

    public class DeeperWalker : CSharpSyntaxWalker
    {
        static int _tabs;
        private readonly ConsoleColor _initColor;

        private DeeperWalker() : base(SyntaxWalkerDepth.Token) => _initColor = Console.ForegroundColor;

        public static void VisitTree(SyntaxTree tree)
        {
            var deepWalker = new DeeperWalker();
            deepWalker.Visit(tree.GetRoot());
            Console.ForegroundColor = deepWalker._initColor;
        }

        public override void Visit(SyntaxNode? node)
        {
            _tabs++;
            if (node is not null)
            {
                var indents = new string('\t', _tabs);
                Console.ForegroundColor = ConsoleColor.Green;
                Console.WriteLine(indents + node.Kind());
            }
            base.Visit(node);
            _tabs--;
        }

        public override void VisitToken(SyntaxToken token)
        {
            var indents = new string('\t', _tabs);
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine(indents + token);
            base.VisitToken(token);
        }
    }

    public class ClassMethodWalker : CSharpSyntaxWalker
    {
        string _className = string.Empty;
        public override void VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            _className = node.Identifier.ToString();
            base.VisitClassDeclaration(node);
        }

        public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            string methodName = node.Identifier.ToString();
            Console.WriteLine($@"{_className}{'.'}{methodName}");
            base.VisitMethodDeclaration(node);
        }
    }


    public static void TreeVisitorFindAllSymbols()
    {
        var (compilation, _, _) = CompilationUtils.CreateTestCompilation(@"class MyClass{ class Nested { } void M() { } }");

        var symbols = FindAllSymbolsVisitor.GetAllSymbols(compilation);

        foreach (var symbol in symbols)
            Console.WriteLine(symbol);
    }

    public class FindAllSymbolsVisitor : SymbolVisitor
    {
        public static List<INamedTypeSymbol> GetAllSymbols(Compilation compilation)
        {
            var visitor = new FindAllSymbolsVisitor();
            visitor.Visit(compilation.GlobalNamespace);
            return visitor.AllTypeSymbols;
        }

        private FindAllSymbolsVisitor() { }

        public List<INamedTypeSymbol> AllTypeSymbols { get; } = new List<INamedTypeSymbol>();

        public override void VisitNamespace(INamespaceSymbol symbol) => Parallel.ForEach(symbol.GetMembers(), s => s.Accept(this));

        public override void VisitNamedType(INamedTypeSymbol symbol)
        {
            AllTypeSymbols.Add(symbol);
            foreach (var childSymbol in symbol.GetTypeMembers())
                Visit(childSymbol);
        }
    }

    /*[DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class Analyzer1Analyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.NamedType);
        }

        private static void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            // Replace the following code with your own analysis, generating Diagnostic objects for any issues you find
            var namedTypeSymbol = (INamedTypeSymbol)context.Symbol;

            // Find just those named type symbols with names containing lowercase letters.
            if (namedTypeSymbol.Name.ToCharArray().Any(char.IsLower))
            {

                DiagnosticDescriptor Rule = new("42", "Title", "MessageFormat", "Category", DiagnosticSeverity.Warning, isEnabledByDefault: true, description: "Description");
                // For all such symbols, produce a diagnostic.
                var diagnostic = Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name);

                context.ReportDiagnostic(diagnostic);
            }
        }

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(new DiagnosticDescriptor("42", "Title", "MessageFormat", "Category", DiagnosticSeverity.Warning, isEnabledByDefault: true, description: "Description"));
    }*/


    public void DumpToConsoleTest()
    {
        const string CODE = @"
using System;

class SimpleClass 
{ 
    public void SimpleMethod()
    {
        var list = new List<int>();
        list.Add(20);
        list.Add(40);
        var result = from item in list
                     where item > 20
                     select item;
    }
}";
        var tree = CSharpSyntaxTree.ParseText(CODE);
        DumpToConsole(tree.GetRoot());
    }


    public void DumpNamesToConsoleTest()
    {
        const string CODE = @"using System;
using System.Linq;

namespace Abc
{
    using System.Collections.Generic;

    enum Enumek : byte { None = 0, One = 1 }

    class SimpleClass
    {
        public int I { get; set; }

        private float f1 = 15.5f, f2 = 30;

        public void SimpleMethod()
        {
            var list = new List<int>();
            list.Add(20);
            list.Add(40);
            if (true)
                Console.WriteLine(123);
            var result = from item in list
                         where item > 20
                         select item;
        }
    }
}";
        var tree = CSharpSyntaxTree.ParseText(CODE);

        new HierarchyTextWalker(Console.Out).Visit(tree.GetRoot());
    }

    private static void DumpToConsole(SyntaxNode node, bool useFullTypeName = false)
        => new HierarchyTypeWalker(Console.Out, useFullTypeName).Visit(node);

    /*private static string DumpToString(SyntaxNode node, bool useFullTypeName = false)
    {
        using var writer = new StringWriter();
        new HierarchyTypeWalker(writer, useFullTypeName).Visit(node);
        return writer.ToString();
    }*/

    class HierarchyTypeWalker : SyntaxWalker
    {
        private readonly TextWriter _writer;
        private readonly bool _useFullTypeName;

        public HierarchyTypeWalker(TextWriter writer, bool useFullTypeName = false, SyntaxWalkerDepth depth = SyntaxWalkerDepth.Node) : base(depth)
        {
            _writer = writer;
            _useFullTypeName = useFullTypeName;
        }

        public override void Visit(SyntaxNode node)
        {
            string prepend = node.ChildNodes().Any() ? "[-]" : "[.]"; //leaf nodes vs parents

            int padding = node.Ancestors().Count();

            string line = $"{new string(' ', padding * 3)}{prepend} ";
            _writer.Write(line);

            _writer.WriteLine(_useFullTypeName ? node.GetType().ToString() : node.GetType().Name);

            base.Visit(node);
        }
    }

    class HierarchyTextWalker : CSharpSyntaxWalker
    {
        private readonly TextWriter _writer;

        public HierarchyTextWalker(TextWriter writer, SyntaxWalkerDepth depth = SyntaxWalkerDepth.StructuredTrivia) : base(depth) => _writer = writer;

        private static SyntaxList<MemberDeclarationSyntax> EmptyMemberList() => SF.List<MemberDeclarationSyntax>();
        private static SeparatedSyntaxList<EnumMemberDeclarationSyntax> EnumMemberList() => SF.SeparatedList<EnumMemberDeclarationSyntax>();

        private void WriteNode(SyntaxNode nodeToWrite, SyntaxNode originalNode)
        {
            //TODO: prepend with custom rule if needed 
            string prepend = originalNode.ChildNodes().Any() ? "[-]" : "[.]"; //leaf nodes vs parents
            int padding = originalNode.Ancestors().Count();

            string line = $"{new string(' ', padding * 3)}{prepend} ";
            _writer.Write(line);

            const string THIN_SPACE = "\u202F";
            var noTrivia = nodeToWrite.WithoutAnnotations().WithoutTrivia();
            string text = noTrivia.ToString()
                    .Replace("\r\n", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\r", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\n", $"{THIN_SPACE}⏎{THIN_SPACE}")
                    .Replace("\t", "↦")
                    .Replace(" ", THIN_SPACE)
                ;
            _writer.WriteLine(text);
        }

        public override void VisitCompilationUnit(CompilationUnitSyntax compUnit)
        {
            var noChildren = compUnit.WithMembers(HierarchyTextWalker.EmptyMemberList());
            WriteNode(noChildren, compUnit);
            base.VisitCompilationUnit(compUnit);
        }

        public override void VisitNamespaceDeclaration(NamespaceDeclarationSyntax @namespace)
        {
            var noChildren = @namespace.WithOpenBraceToken(SF.MissingToken(SyntaxKind.OpenBraceToken)).WithCloseBraceToken(SF.MissingToken(SyntaxKind.CloseBraceToken)).WithMembers(HierarchyTextWalker.EmptyMemberList());
            WriteNode(noChildren, @namespace);
            base.VisitNamespaceDeclaration(@namespace);
        }

        public override void VisitGlobalStatement(GlobalStatementSyntax node)
        {
            WriteNode(node, node);//base.VisitGlobalStatement(node);
        }

        public override void VisitClassDeclaration(ClassDeclarationSyntax @class)
        {
            var noChildren = @class.WithOpenBraceToken(SF.MissingToken(SyntaxKind.OpenBraceToken)).WithCloseBraceToken(SF.MissingToken(SyntaxKind.CloseBraceToken)).WithMembers(HierarchyTextWalker.EmptyMemberList());
            WriteNode(noChildren, @class);
            base.VisitClassDeclaration(@class);
        }

        public override void VisitStructDeclaration(StructDeclarationSyntax @struct)
        {
            var noChildren = @struct.WithOpenBraceToken(SF.MissingToken(SyntaxKind.OpenBraceToken)).WithCloseBraceToken(SF.MissingToken(SyntaxKind.CloseBraceToken)).WithMembers(HierarchyTextWalker.EmptyMemberList());
            WriteNode(noChildren, @struct);
            base.VisitStructDeclaration(@struct);
        }

        public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax @interface)
        {
            var noChildren = @interface.WithOpenBraceToken(SF.MissingToken(SyntaxKind.OpenBraceToken)).WithCloseBraceToken(SF.MissingToken(SyntaxKind.CloseBraceToken)).WithMembers(HierarchyTextWalker.EmptyMemberList());
            WriteNode(noChildren, @interface);
            base.VisitInterfaceDeclaration(@interface);
        }

        public override void VisitEnumDeclaration(EnumDeclarationSyntax @enum)
        {
            var noChildren = @enum.WithOpenBraceToken(SF.MissingToken(SyntaxKind.OpenBraceToken)).WithCloseBraceToken(SF.MissingToken(SyntaxKind.CloseBraceToken)).WithMembers(HierarchyTextWalker.EnumMemberList());
            WriteNode(noChildren, @enum);
            base.VisitEnumDeclaration(@enum);
        }

        public override void VisitDelegateDeclaration(DelegateDeclarationSyntax node)
        {
            WriteNode(node, node);//base.VisitDelegateDeclaration(node);
        }


        public override void VisitEnumMemberDeclaration(EnumMemberDeclarationSyntax node)
        {
            WriteNode(node, node);//base.VisitEnumMemberDeclaration(node);
        }

        public override void VisitFieldDeclaration(FieldDeclarationSyntax node)
        {
            WriteNode(node, node);//base.VisitFieldDeclaration(node);
        }

        public override void VisitEventFieldDeclaration(EventFieldDeclarationSyntax node)
        {
            WriteNode(node, node);//base.VisitEventFieldDeclaration(node);
        }


        public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            WriteNode(node, node);//base.VisitPropertyDeclaration(node);
        }

        public override void VisitEventDeclaration(EventDeclarationSyntax node)
        {
            WriteNode(node, node);//base.VisitEventDeclaration(node);
        }

        public override void VisitIndexerDeclaration(IndexerDeclarationSyntax node)
        {
            WriteNode(node, node);//base.VisitIndexerDeclaration(node);
        }


        public override void VisitMethodDeclaration(MethodDeclarationSyntax method)
        {
            var noChildren = method.WithBody(null);
            WriteNode(noChildren, method);
            base.VisitMethodDeclaration(method);
        }

        public override void VisitOperatorDeclaration(OperatorDeclarationSyntax @operator)
        {
            var noChildren = @operator.WithBody(null);
            WriteNode(noChildren, @operator);
            base.VisitOperatorDeclaration(@operator);
        }

        public override void VisitConversionOperatorDeclaration(ConversionOperatorDeclarationSyntax conversion)
        {
            var noChildren = conversion.WithBody(null);
            WriteNode(noChildren, conversion);
            base.VisitConversionOperatorDeclaration(conversion);
        }

        public override void VisitConstructorDeclaration(ConstructorDeclarationSyntax ctor)
        {
            var noChildren = ctor.WithBody(null);
            WriteNode(noChildren, ctor);
            base.VisitConstructorDeclaration(ctor);
        }

        public override void VisitDestructorDeclaration(DestructorDeclarationSyntax destr)
        {
            var noChildren = destr.WithBody(null);
            WriteNode(noChildren, destr);
            base.VisitDestructorDeclaration(destr);
        }

        /*public override void VisitBlock(BlockSyntax block)
        {
            //var noBrackets = block.WithOpenBraceToken(SF.MissingToken(SyntaxKind.OpenBraceToken)).WithCloseBraceToken(SF.MissingToken(SyntaxKind.CloseBraceToken));
            base.VisitBlock(block);
        }*/

        #region Statements and clauses

        public override void VisitBreakStatement(BreakStatementSyntax node) { WriteNode(node, node); /* base.VisitBreakStatement(node); */}

        public override void VisitCheckedExpression(CheckedExpressionSyntax node) { WriteNode(node, node); /* base.VisitCheckedExpression(node); */ }

        public override void VisitCheckedStatement(CheckedStatementSyntax node) { WriteNode(node, node); /* base.VisitCheckedStatement(node); */ }

        public override void VisitContinueStatement(ContinueStatementSyntax node) { WriteNode(node, node); /* base.VisitContinueStatement(node); */ }

        public override void VisitDoStatement(DoStatementSyntax node) { WriteNode(node, node); /* base.VisitDoStatement(node); */ }

        public override void VisitExpressionStatement(ExpressionStatementSyntax node) { WriteNode(node, node); /* base.VisitExpressionStatement(node); */ }

        public override void VisitFixedStatement(FixedStatementSyntax node) { WriteNode(node, node); /* base.VisitFixedStatement(node); */ }

        public override void VisitForStatement(ForStatementSyntax node) { WriteNode(node, node); /* base.VisitForStatement(node); */ }

        public override void VisitForEachStatement(ForEachStatementSyntax node) { WriteNode(node, node); /* base.VisitForEachStatement(node); */ }

        public override void VisitGotoStatement(GotoStatementSyntax node) { WriteNode(node, node); /* base.VisitGotoStatement(node); */ }

        public override void VisitIfStatement(IfStatementSyntax node) { WriteNode(node, node); /* base.VisitIfStatement(node); */ }

        public override void VisitLabeledStatement(LabeledStatementSyntax node) { WriteNode(node, node); /* base.VisitLabeledStatement(node); */ }

        public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node) { WriteNode(node, node); /* base.VisitLocalDeclarationStatement(node); */ }

        public override void VisitLocalFunctionStatement(LocalFunctionStatementSyntax node) { WriteNode(node, node); /* base.VisitLocalFunctionStatement(node); */ }

        public override void VisitLockStatement(LockStatementSyntax node) { WriteNode(node, node); /* base.VisitLockStatement(node); */ }

        public override void VisitReturnStatement(ReturnStatementSyntax node) { WriteNode(node, node); /* base.VisitReturnStatement(node); */ }

        public override void VisitSwitchStatement(SwitchStatementSyntax node) { WriteNode(node, node); /* base.VisitSwitchStatement(node); */ }

        public override void VisitThrowStatement(ThrowStatementSyntax node) { WriteNode(node, node); /* base.VisitThrowStatement(node); */ }

        public override void VisitTryStatement(TryStatementSyntax node) { WriteNode(node, node); /* base.VisitTryStatement(node); */ }

        public override void VisitUnsafeStatement(UnsafeStatementSyntax node) { WriteNode(node, node); /* base.VisitUnsafeStatement(node); */ }

        public override void VisitUsingStatement(UsingStatementSyntax node) { WriteNode(node, node); /* base.VisitUsingStatement(node); */ }

        public override void VisitWhileStatement(WhileStatementSyntax node) { WriteNode(node, node); /* base.VisitWhileStatement(node); */ }

        public override void VisitYieldStatement(YieldStatementSyntax node) { WriteNode(node, node); /* base.VisitYieldStatement(node); */ }


        public override void VisitElseClause(ElseClauseSyntax node) { WriteNode(node, node); /* base.VisitElseClause(node); */ }

        public override void VisitCatchClause(CatchClauseSyntax node) { WriteNode(node, node); /* base.VisitCatchClause(node); */ }

        public override void VisitFinallyClause(FinallyClauseSyntax node) { WriteNode(node, node); /* base.VisitFinallyClause(node); */ }

        #endregion
    }

    /// <summary>Rewrites string and character literals which contain non ascii characters to instead use the \uXXXX or \UXXXXXXXX syntax.</summary>
    internal class UnicodeCharacterEscapingSyntaxRewriter : CSharpSyntaxRewriter
    {
        public static readonly CSharpSyntaxRewriter Instance = new UnicodeCharacterEscapingSyntaxRewriter();

        private UnicodeCharacterEscapingSyntaxRewriter() { }

        public override SyntaxNode VisitLiteralExpression(LiteralExpressionSyntax node)
            => node.Kind() switch
            {
                SyntaxKind.StringLiteralExpression => RewriteStringLiteralExpression(node),
                SyntaxKind.CharacterLiteralExpression => RewriteCharacterLiteralExpression(node),
                _ => node,
            };

        private static SyntaxNode RewriteStringLiteralExpression(LiteralExpressionSyntax node)
        {
            Debug.Assert(node.Kind() == SyntaxKind.StringLiteralExpression);

            if (node.Token.IsVerbatimStringLiteral()) return node;

            if (HasNonAsciiCharacters(node.Token.Text))
            {
                string convertedText = EscapeNonAsciiCharacters(node.Token.Text);

                SyntaxToken t = SF.Literal(node.Token.LeadingTrivia, convertedText, node.Token.ValueText, node.Token.TrailingTrivia);

                node = node.WithToken(t);
            }

            return node;
        }

        private static SyntaxNode RewriteCharacterLiteralExpression(LiteralExpressionSyntax node)
        {
            Debug.Assert(node.Kind() == SyntaxKind.CharacterLiteralExpression);

            if (HasNonAsciiCharacters(node.Token.Text))
            {
                string convertedText = EscapeNonAsciiCharacters(node.Token.Text);

                SyntaxToken t = SF.Literal(node.Token.LeadingTrivia, convertedText, node.Token.ValueText, node.Token.TrailingTrivia);

                node = node.WithToken(t);
            }

            return node;
        }

        private static bool HasNonAsciiCharacters(string value)
        {
            // ReSharper disable once ForCanBeConvertedToForeach
            for (int i = 0; i < value.Length; i++)
                if (value[i] >= 0x80)
                    return true;
            return false;
        }

        private static string EscapeNonAsciiCharacters(string oldValue)
        {
            var sb = new StringBuilder(oldValue.Length);

            for (int i = 0; i < oldValue.Length; i++)
            {
                if (oldValue[i] < 0x80)
                    sb.Append(oldValue[i]);
                else if (char.IsHighSurrogate(oldValue[i]) && i + 1 < oldValue.Length &&
                         char.IsLowSurrogate(oldValue[i + 1]))
                {
                    sb.Append($@"\U{char.ConvertToUtf32(oldValue[i], oldValue[i + 1]):X8}");
                    i++; // move past the low surogate we consumed above.
                }
                else
                    sb.Append($@"\u{(ushort)oldValue[i]:X4}");
            }

            return sb.ToString();
        }
    }
}
