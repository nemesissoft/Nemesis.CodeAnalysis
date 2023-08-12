using System.Diagnostics;
using System.Globalization;
using System.Reflection;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.Scripting;
using Microsoft.CodeAnalysis.Text;

namespace Nemesis.CodeAnalysis.Sample;

partial class Roslyn
{
    public static void CompileSample()
    {
        var sourceCode = @"
using System;
using System.IO;
namespace MainNamespace
{
    class Program
    {
        public string Print()
        {
            var s = 42 + "" is the answer"";
            System.Console.WriteLine(s);
            return s;
        }
    }
}";
        /*var compiledAssembly = CompileSourceCodeDom(sourceCode);
        var resultCodeDom = ExecuteFromAssembly(compiledAssembly);*/

        var roslynAssembly = CompileSourceRoslyn(sourceCode);
        var resultRoslyn = ExecuteFromAssembly(roslynAssembly);

        //Assert.That(resultCodeDom, Is.TypeOf<string>());
        //Assert.That(resultRoslyn, Is.TypeOf<string>());

        //Assert.That((string)resultRoslyn, Is.EqualTo("42 is the answer"));
    }

    /*private static Assembly CompileSourceCodeDom(string sourceCode)
    {
        using (var csharpCodeProvider = new CSharpCodeProvider())
        {
            var cp = new CompilerParameters { GenerateExecutable = false, ReferencedAssemblies = { "System.dll" } };

            CompilerResults cr = csharpCodeProvider.CompileAssemblyFromSource(cp, sourceCode);

            if (cr.Errors.Count > 0) // Display compilation errors
            {
                Console.WriteLine(@"Errors building source into target file '{0}'", cr.PathToAssembly);
                foreach (CompilerError ce in cr.Errors)
                    Console.WriteLine(@"ERROR: {0}{1}", ce, Environment.NewLine);
            }

            return cr.Errors.Count == 0 ? cr.CompiledAssembly : null;
        }
    }*/

    private static Assembly CompileSourceRoslyn(string sourceCode)
    {
        string assemblyFileName = string.Concat(Guid.NewGuid().ToString(), ".dll");

        var compilationOptions = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithOptimizationLevel(OptimizationLevel.Debug)
                .WithAllowUnsafe(true)
                .WithModuleName("CompiledModule")
                .WithPlatform(Platform.AnyCpu)
                .WithMetadataReferenceResolver(ScriptMetadataResolver.Default)
            ;


        var neededRefs = new[] { typeof(object), typeof(Enumerable), typeof(List<>), typeof(decimal), typeof(Console), typeof(Action<>) }
            .Select(t => t.Assembly.Location).Distinct().ToList();

        var compilation = CSharpCompilation.Create(assemblyFileName,
            new[] { CSharpSyntaxTree.ParseText(sourceCode) },
            neededRefs.Select(ass => MetadataReference.CreateFromFile(ass)),
            compilationOptions);

        using var assemblyStream = new MemoryStream();
        using var symbolStream = new MemoryStream();

        var emitOptions = new EmitOptions(false, DebugInformationFormat.PortablePdb, null, "ScriptFromTests");
        var result = compilation.Emit(assemblyStream, symbolStream, options: emitOptions);
        if (result.Success)
        {
            File.WriteAllBytes(Path.Combine(Path.GetTempPath(), assemblyFileName), assemblyStream.ToArray());
            return Assembly.Load(assemblyStream.ToArray(), symbolStream.ToArray());
        }
        else
        {
            var failures = result.Diagnostics
                .Where(diagnostic => diagnostic.IsWarningAsError || diagnostic.Severity == DiagnosticSeverity.Error)
                .Select(diagnostic => $"{diagnostic.Id}: {diagnostic.GetMessage()}");
            throw new InvalidOperationException(
                $"Compilation error: {string.Join(Environment.NewLine, failures)}");
        }
    }

    private static object? ExecuteFromAssembly(Assembly assembly)
    {
        var program = assembly.GetType("MainNamespace.Program");
        MethodInfo printMethod = program?.GetMethod("Print") ?? throw new InvalidOperationException("No Print method");
        var programInstance = Activator.CreateInstance(program) ?? throw new InvalidOperationException("Program class cannot be instantiated");
        return printMethod.Invoke(programInstance, BindingFlags.InvokeMethod, null, null, CultureInfo.CurrentCulture);
    }

    public static void MetadataSeeker()
    {
        var tree = CSharpSyntaxTree.ParseText(@"public class MyClass { public void MyMethod() {  }  public void MyMethod(int n) {  } }");

        var syntaxRoot = tree.GetRoot();
        MethodDeclarationSyntax myMethodWithAtLeastOneParameter = syntaxRoot.DescendantNodes().OfType<MethodDeclarationSyntax>().First(n => n.ParameterList.Parameters.Any());

        //Assert.AreEqual("MyMethod", myMethodWithAtLeastOneParameter.Identifier.Text);
        //Assert.AreEqual(1, myMethodWithAtLeastOneParameter.ParameterList.Parameters.Count);


        TypeDeclarationSyntax containingType = myMethodWithAtLeastOneParameter.Ancestors().OfType<TypeDeclarationSyntax>().First();
        //Assert.AreEqual("MyClass", containingType.Identifier.Text);
    }

    public static void DataFlowTest()
    {
        var code = @"
public class Sample
{
   public void Foo()
   {
        int[] outerArray = new int[10] { 0, 1, 2, 3, 4, 0, 1, 2, 3, 4};
        for (int index = 0; index < 10; index++)
        {
             int[] innerArray = new int[10] { 0, 1, 2, 3, 4, 0, 1, 2, 3, 4 };
             index = index + 2;
             outerArray[index - 1] = 5;
        }
   }
}";
        var (_, tree, model) = CompilationUtils.CreateTestCompilation(code);

        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();
        var result = model.AnalyzeDataFlow(forStatement) ?? throw new("NULL");

        Console.WriteLine(@"AlwaysAssigned");
        foreach (ISymbol symbol in result.AlwaysAssigned)
            Console.WriteLine(symbol.Name);
        Console.WriteLine();

        Console.WriteLine(@"WrittenInside");
        foreach (ISymbol symbol in result.WrittenInside)
            Console.WriteLine(symbol.Name);
        Console.WriteLine();

        Console.WriteLine(@"WrittenOutside");
        foreach (ISymbol symbol in result.WrittenOutside)
            Console.WriteLine(symbol.Name);
        Console.WriteLine();

        Console.WriteLine(@"ReadInside");
        foreach (ISymbol symbol in result.ReadInside)
            Console.WriteLine(symbol.Name);
        Console.WriteLine();

        Console.WriteLine(@"VariablesDeclared");
        foreach (ISymbol symbol in result.VariablesDeclared)
            Console.WriteLine(symbol.Name);
        Console.WriteLine();
    }

    public static void ControlFlowTestIf()
    {
        var code = @"
class C
{
    void M()
    {
        for (int i = 0; i < 10; i++)
        {
            if (i == 3)
                continue;
            if (i == 8)
                break;
            if (i == 11)
                return;
            if (i == 12)
                goto Finish;
        }
        Finish:
            Console.WriteLine(""End"");
        }
    }
}";
        var (_, tree, model) = CompilationUtils.CreateTestCompilation(code);

        var firstFor = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();
        var result = model.AnalyzeControlFlow(firstFor.Statement);

        //Assert.That(result.Succeeded, Is.True);
        //Assert.That(result.ExitPoints.Select(ep => ep.Kind()).ToList(), Is.EquivalentTo(new[] { SyntaxKind.ContinueStatement, SyntaxKind.BreakStatement, SyntaxKind.ReturnStatement, SyntaxKind.GotoStatement }));
    }

    public static void ControlFlowTestLabels()
    {
        var code = @"
class C
{
    void M(int x)
    {
        L1: ; // 1
        if (x == 0) goto L1;    //firstIf
        if (x == 1) goto L2;
        if (x == 3) goto L3;
        L3: ;                   //label3
        L2: ; // 2
        if(x == 4) goto L3;
    }
}
";
        var (_, tree, model) = CompilationUtils.CreateTestCompilation(code);

        //Choose first and last statements
        var firstIf = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var label3 = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Skip(1).Take(1).Single();

        var result = model.AnalyzeControlFlow(firstIf, label3);
        //Assert.AreEqual(1, result.EntryPoints.Length);      //1 - Label 3 is a candidate entry point within these statements
        //Assert.AreEqual(2, result.ExitPoints.Length);       //2 - goto L1 and goto L2 and candidate exit points
    }

    public static void ControlFlowTestReachable()
    {
        var code = @"
class C
{
    void M(int x)
    {
        return;
        if(x == 0)                                  //-+     Start is unreachable
            System.Console.WriteLine(""Hello"");    // |
        L1:                                            //-+    End is unreachable
    }
}";
        var (_, tree, model) = CompilationUtils.CreateTestCompilation(code);

        //Choose first and last statements
        var firstIf = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().Single();
        var label1 = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();

        var result = model.AnalyzeControlFlow(firstIf, label1);
        //Assert.That(!result.StartPointIsReachable);    //False
        //Assert.That(!result.EndPointIsReachable);      //False
    }

    public static void CSharpScriptEvaluateTests()
    {
        int result = CSharpScript.EvaluateAsync<int>("48-6").Result;
        //Assert.That(result, Is.EqualTo(42));


        var globals = new Globals { X = 1, Y = 2 };
        int resultWithGlobals = CSharpScript.EvaluateAsync<int>("X+Y", globals: globals).Result;
        //Assert.That(resultWithGlobals, Is.EqualTo(3));


        var script = CSharpScript.Create<int>("X*Y", globalsType: typeof(Globals));
        script.Compile();
        //for (int i = 0; i < 10; i++)
        //    Assert.That
        //    (
        //        script.RunAsync(new Globals { X = i, Y = i }).Result.ReturnValue,
        //        Is.EqualTo(i * i)
        //    );


        var script2 = CSharpScript.Create<int>("X=X+Y*2; X+5", globalsType: typeof(Globals));
        ScriptRunner<int> runner = script2.CreateDelegate();
        //for (int i = 0; i < 10; i++)
        //    Assert.That
        //    (
        //        runner(new Globals { X = i, Y = i }).Result,
        //        Is.EqualTo(i + i * 2 + 5)
        //    );



        /*var strongTypeFromDictionary = FromDictToAnonymousObj(new Dictionary<string, object> { ["AnimalList"] = new List<string> { "Dog", "Lion" } });
        int listCount = CSharpScript.EvaluateAsync<int>("AnimalList.Count",
            globals: strongTypeFromDictionary,
            globalsType: strongTypeFromDictionary.GetType(),
            options: ScriptOptions.Default.WithReferences(strongTypeFromDictionary.GetType().Assembly)

            ).Result;
        Assert.That(listCount, Is.EqualTo(2));*/
    }

    public static void CustomDebuggableScriptCompileTests()
    {
        #region Code

        const string BREAKPOINT = "breakpoint";
        const string PRINT = "print";

        string content = @"var i = 15;
//print $""{X}, {Y}""
//X = 15;
//Y = 20;
print i
print i*10
i++;
print $""ABC{i}""+""XYZ""
i++;
System.Console.WriteLine(i);
System.Diagnostics.Debugger.Break();

unsafe
{
    int number = 0xABCDEF;
    int* intPtr = &number;
    {
        byte* bytePtr = (byte*)intPtr;
        byte b;
        do
        {
            b = *bytePtr;
            Console.WriteLine(""{0:X2}"", b);
            bytePtr++;
        } while (b > 0);
    }
}

var numbers = new[] { 0x12345678, 0xABCDEF };
print numbers[0].ToString(""X8"")

var result = new List<string>{ i.ToString() };

breakpoint
unsafe
{
    fixed (int* p1 = numbers)
        for (byte* b1 = (byte*)p1; b1 < p1 + 2; b1++)
            result.Add($""0x{*b1:X2}"");
}
result
";

        #endregion


        string scriptPath = Path.Combine(Path.GetTempPath(), "CustomRoslynScript.txt");
        string assemblyPath = Path.Combine(Path.GetTempPath(), "CustomRoslynScript.dll");
        File.WriteAllText(scriptPath, content);

        var script = File.ReadAllText(scriptPath);

        static string ProcessLine(string line) => line.StartsWith(PRINT) ?
            $"System.Console.WriteLine({line.Substring(PRINT.Length)});" :
            (line.StartsWith(BREAKPOINT) ? "System.Diagnostics.Debugger.Break();" : line);

        string newCode =
            script.Split(new[] { Environment.NewLine, "\n", "\r" }, StringSplitOptions.None)
                .Aggregate(
                    new StringBuilder($"#line 1 \"{scriptPath}\"{Environment.NewLine}"),
                    (sb, line) => sb.AppendLine(ProcessLine(line)),
                    sb => sb.ToString());

        var neededTypes = new[] { typeof(object), typeof(Enumerable), typeof(List<>), typeof(Debug), typeof(Console) };

        var scriptOptions = ScriptOptions.Default
            .AddReferences(neededTypes.Select(t => t.Assembly).Distinct())
            .AddImports(neededTypes.Select(t => t.Namespace).Distinct());

        var roslynScript = CSharpScript.Create<object>(newCode, scriptOptions);
        var compilation = roslynScript.GetCompilation();


        var compilationOptions = (CSharpCompilationOptions)compilation.Options;

        const string SCRIPT_CLASS = "ScriptClass";

        compilationOptions = compilationOptions
            .WithOptimizationLevel(OptimizationLevel.Debug)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithAllowUnsafe(true)
            .WithModuleName("ScriptingModule")
            .WithScriptClassName(SCRIPT_CLASS)
            .WithGeneralDiagnosticOption(ReportDiagnostic.Error)
            ;

        compilation = compilation.WithOptions(compilationOptions);

        using var assemblyStream = new MemoryStream();
        using var symbolStream = new MemoryStream();

        var emitOptions = new EmitOptions(false, DebugInformationFormat.PortablePdb, null, "ScriptFromTests");

        var result = compilation.Emit(assemblyStream, symbolStream, options: emitOptions);
        if (result.Success)
        {
            File.WriteAllBytes(assemblyPath, assemblyStream.ToArray());
            //AssemblyDefinition assembly = AssemblyDefinition.ReadAssembly(stream);
            var assembly = Assembly.Load(assemblyStream.ToArray(), symbolStream.ToArray());
            var type = assembly.GetType(SCRIPT_CLASS) ?? throw new InvalidOperationException($"'{SCRIPT_CLASS}' type cannot be found");
            var method = type.GetMethod("<Factory>", BindingFlags.Static | BindingFlags.Public) ?? throw new MissingMethodException($"{SCRIPT_CLASS}.<Factory> method does not exist");

            var methodResult = method.Invoke(null, new object[] { new object[2] });

            /*Assert.That(methodResult.Result, Is.EquivalentTo(
                new[] { "17", "0x78", "0x56", "0x34", "0x12", "0xEF", "0xCD", "0xAB", "0x00" })
            );*/
        }
        else
        {
            var failures = result.Diagnostics
                .Where(diagnostic => diagnostic.IsWarningAsError || diagnostic.Severity == DiagnosticSeverity.Error)
                .Select(diagnostic => $"{diagnostic.Id}: {diagnostic.GetMessage()}");
            throw new InvalidOperationException(
                $"Compilation error: {string.Join(Environment.NewLine, failures)}");
        }
    }

    public static void CSharpScriptAnalyzeTests()
    {
        var script = CSharpScript.Create<float>("Single sin = (float)Math.Sin(30); System.Console.WriteLine(sin); float f = 15.5f; sin+f");
        Compilation compilation = script.GetCompilation();
        var symModels = compilation.SyntaxTrees.Select(synTree => compilation.GetSemanticModel(synTree, true)).Single();

        var allIdentifiers = symModels.SyntaxTree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Select(v => v.Identifier.ValueText).ToList();

        //Assert.That(allIdentifiers, Is.EquivalentTo(new[] { "Single", "Math", "Sin", "System", "Console", "WriteLine", "sin", "sin", "f" }));
    }

    public static void CSharpScriptTests()
    {
        var result = CSharpScript.EvaluateAsync("5 + 5").Result;
        //Assert.AreEqual(10, result); // 10

        result = CSharpScript.EvaluateAsync(@"""sample""").Result;
        //Assert.AreEqual("sample", result); // sample

        result = CSharpScript.EvaluateAsync(@"""sample"" + "" string""").Result;
        //Assert.AreEqual("sample string", result); // sample string

        result = CSharpScript.EvaluateAsync("int x = 5; int y = 5; x").Result; //Note the last x is not contained in a proper statement
        //Assert.AreEqual(5, result); // 5
    }

    public static void CSharpScriptStateTests()
    {
        var state = CSharpScript.RunAsync(@"int x = 5; int y = 3; int z = x + y;z").Result;
        state = state.ContinueWithAsync("x++; y = 1;").Result;
        state = state.ContinueWithAsync("x = x + y;").Result;

        ScriptVariable x = state.Variables.Single(sv => sv.Name == "x");
        ScriptVariable y = state.Variables.Single(sv => sv.Name == "y");

        //Assert.That(x.Value is int i && i == 7);
        //Assert.That(y.Value is int i1 && i1 == 1);
    }

    public static void CSharpScriptOptions()
    {
        ScriptOptions scriptOptions = ScriptOptions.Default;

        //Add reference to mscorlib
        var mscorlib = typeof(object).Assembly;
        var systemCore = typeof(Enumerable).Assembly;
        scriptOptions = scriptOptions.AddReferences(mscorlib, systemCore);
        //Add namespaces
        scriptOptions = scriptOptions.AddImports("System", "System.Linq", "System.Collections.Generic");

        var state = CSharpScript.RunAsync(@"var x = new List<int>(){1,2,3,4,5};", scriptOptions).Result;
        state = state.ContinueWithAsync("var y = x.Skip(1).Take(2).ToList();").Result;

        var y = state.Variables.Single(sv => sv.Name == "y");
        var yList = (IList<int>)y.Value;
        //Assert.That(yList.Count == 2);
        //Assert.That(yList[0] == 2);
        //Assert.That(yList[1] == 3);
    }

    /*internal class RoslynSampleClass
    {
        internal static long Ticks = DateTime.Today.Ticks;
    }

    [Test, Ignore("TODO")]
    public static void ResolveMemberAccessTest()
    {
        var msWorkspace = MSBuildWorkspace.Create();
        var path = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) ?? "";
        var project = msWorkspace.OpenProjectAsync(Path.Combine(path, @"..\..\Nemesis_Education.csproj")).Result;
        var compilation = project.GetCompilationAsync().Result;
        var document = project.Documents.First(d => d.Name == "Roslyn.cs");
        document.TryGetSyntaxTree(out SyntaxTree syntaxTree);

        ClassDeclarationSyntax classDeclaration = syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First(c => c.Identifier.ToString() == nameof(RoslynSampleClass));
        var attr = classDeclaration.AttributeLists.SelectMany(a => a.Attributes).FirstOrDefault(a => a.Name.ToString() == "Option");

        //SemanticModel semanticModel = compilation.GetSemanticModel(syntaxTree);
        //semanticModel.Compilation.GetSemanticModel()
        //compilation.SyntaxTrees.Select(synTree=>compilation.GetSemanticModel(synTree, true))

        //TODO:resolve member access
        /*var exp = attr.ArgumentList.Arguments.First().Expression;
        string value = null;
        var mem = exp as MemberAccessExpressionSyntax;
        if (mem != null)
        {
            value = ResolveMemberAccess(mem, solution, compilation)?.ToString();
        }
        else
        {
            var lit = exp as LiteralExpressionSyntax;
            if (lit != null)
            {
                value = semanticModel.GetConstantValue(lit).Value?.ToString();
            }
        }#1#
    }

    private static object ResolveMemberAccess(MemberAccessExpressionSyntax memberSyntax, Solution solution, Compilation compilation)
    {
        var model = compilation.GetSemanticModel(memberSyntax.SyntaxTree);
        var memberSymbol = model.GetSymbolInfo(memberSyntax).Symbol;
        var refs = SymbolFinder.FindReferencesAsync(memberSymbol, solution).Result.FirstOrDefault();

        if (refs != null)
        {
            var defSyntax = refs.Definition.DeclaringSyntaxReferences.First();
            var parent = compilation.GetSemanticModel(defSyntax.SyntaxTree);
            var syn = defSyntax.GetSyntax();

            var literal = syn.DescendantNodes().OfType<LiteralExpressionSyntax>().FirstOrDefault();
            if (literal != null)
            {
                var val = parent.GetConstantValue(literal);
                return val.Value;
            }
            else
            {
                var memberAccess = syn.DescendantNodes().OfType<MemberAccessExpressionSyntax>().FirstOrDefault();
                if (memberAccess != null)
                {
                    return ResolveMemberAccess(memberAccess, solution, compilation);
                }
            }
        }
        return null;
    }*/

    public static void RegionReplacer()
    {
        var tree = CSharpSyntaxTree.ParseText(@"
class Class
{
    #region CFG.REMOVE whole method

    public void MethodToBeRemoved()
    {

    }

    #endregion


    public void MethodWithParameterToBeRemoved(
    #region CFG.REMOVE parameter
        string param
    #endregion
        )
    {
    #region DO NOT REMOVE
        Console.WriteLine();
    #endregion
    }

    public void MethodWithContentToBeRemoved()
    {
        #region CFG.REMOVE invocation
        Console.WriteLine(123);
        #endregion

        #region  CFG.REMOVE
        //no removal comment with additional leading space 
        #endregion
    }
}");

        var root = tree.GetRoot();
        var regionNodes = root.DescendantNodes(descendIntoChildren: null, descendIntoTrivia: true).OfType<RegionDirectiveTriviaSyntax>().OrderBy(end => end.FullSpan.Start).ToList();
        var endRegionNodes = root.DescendantNodes(descendIntoChildren: null, descendIntoTrivia: true).OfType<EndRegionDirectiveTriviaSyntax>().OrderBy(end => end.FullSpan.Start).ToList();

        if (regionNodes.Count != endRegionNodes.Count) throw new InvalidDataException("Unbalanced #region directives");

        var regionRemovalPattern = new Regex(@"^CFG\.REMOVE\s*(?<RemovalComment>.*)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant | RegexOptions.IgnorePatternWhitespace | RegexOptions.Compiled);

        var removeRegions =
                from region in regionNodes
                let name = region.EndOfDirectiveToken.GetAllTrivia().Single(st => st.IsKind(SyntaxKind.PreprocessingMessageTrivia)).ToString()
                let match = regionRemovalPattern.Match(name)
                where match.Success
                let matchingEndRegion = endRegionNodes.First(end => region.FullSpan.End < end.FullSpan.Start)
                select (Span: new TextSpan(region.FullSpan.Start, matchingEndRegion.Span.End - region.FullSpan.Start), Comment: match.Groups["RemovalComment"].Value);

        var textChanges = removeRegions.Select(pair => new TextChange(pair.Span, string.IsNullOrEmpty(pair.Comment) ? "" : $"/*removed {pair.Comment}*/"));
        tree = tree.WithChangedText(tree.GetText().WithChanges(textChanges));
        var newSource = tree.GetText().ToString();

        var newSourceWithNoWhites = Regex.Replace(newSource, @"\s+", "");
        //Assert.That(newSourceWithNoWhites,
        //    Is.EqualTo(
        //        "classClass{/*removedwholemethod*/publicvoidMethodWithParameterToBeRemoved(/*removedparameter*/){#regionDONOTREMOVEConsole.WriteLine();#endregion}publicvoidMethodWithContentToBeRemoved(){/*removedinvocation*/}}"));
    }
}

public class Globals
{
#pragma warning disable 414
    public int X;
    public int Y;
#pragma warning restore 414
}
