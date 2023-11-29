#nullable enable

using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

namespace Nemesis.CodeAnalysis;

[PublicAPI]
public static class CompilationUtils
{
    #region Evaluation

    public static async Task<(bool Success, string? Result)> TryEvaluateExpression(string code)
    {
        try
        {
            var scriptOptions = ScriptOptions.Default;
            var result = await CSharpScript.EvaluateAsync(code, scriptOptions);
            return (true, result?.ToString());
        }
        catch (Exception e)
        {
            return (false, $"<error in '{code}' {e.Message}>");
        }
    }

    public static Type GetTypeFromCSharpName(string typeName, IEnumerable<string>? additionalNamespaces = null, params Assembly[] additionalAssemblies)
    {
        var standardNamespaces = new[] { "System", "System.Linq", "System.Collections.Generic" };

        var scriptOptions = ScriptOptions.Default
            .AddReferences(new[] { typeof(object).Assembly, typeof(Enumerable).Assembly }.Concat(additionalAssemblies))
            .AddImports(additionalNamespaces is null ? standardNamespaces : standardNamespaces.Concat(additionalNamespaces));

        var state = CSharpScript.RunAsync<Type>($"typeof({typeName})", scriptOptions).Result;
        return state.ReturnValue;
    }

    private static readonly ConcurrentDictionary<string, Type> _nameToTypeMap = new();
    public static Type GetTypeFromCSharpNameCached(string csharpName, IEnumerable<string>? additionalNamespaces = null, params Assembly[] additionalAssemblies)
        => _nameToTypeMap.GetOrAdd(csharpName,
            name => GetTypeFromCSharpName(name, additionalNamespaces, additionalAssemblies)
        );

    #endregion

    #region Compilation

    /// <example><![CDATA[
    /// var ws = MSBuildWorkspace.Create();
    /// var project = ws.OpenProjectAsync(@"..\..\ConsoleApp1.csproj").Result;
    /// var issues = GetCompilationIssues(project.GetCompilationAsync().Result);
    /// ]]></example>
    public static IEnumerable<string> GetCompilationIssues(Compilation compilation)
    {
        using var ms = new MemoryStream();
        var result = compilation.Emit(ms);
        return result.Diagnostics
            .Where(d => d.Severity == DiagnosticSeverity.Error || d.Severity == DiagnosticSeverity.Warning)
            .Select(d => d.ToString()).ToList();
    }


    public static (Compilation compilation, SyntaxTree sourceTree, SemanticModel semanticModel) CreateTestCompilation(
        string source, Assembly[]? additionalAssemblies = null, OutputKind outputKind = OutputKind.DynamicallyLinkedLibrary, [CallerMemberName] string? memberName = null)
    {

        var assemblyPath = Path.GetDirectoryName(typeof(object).Assembly.Location) ?? throw new InvalidOperationException("The location of the .NET assemblies cannot be retrieved");

        var tree = CSharpSyntaxTree.ParseText(source, CSharpParseOptions.Default.WithLanguageVersion(LanguageVersion.Latest))
            //.WithFilePath(path)
            ;

        var references = new List<PortableExecutableReference>(8)
        {
            MetadataReference.CreateFromFile(Path.Combine(assemblyPath, "System.Runtime.dll")),
            MetadataReference.CreateFromFile(typeof(Binder).GetTypeInfo().Assembly.Location)
        };


        if (additionalAssemblies is not null)
            foreach (var ass in additionalAssemblies)
                references.Add(MetadataReference.CreateFromFile(ass.Location));


        var compilation = CSharpCompilation.Create($"{memberName}_Compilation", new[] { tree }, references, new CSharpCompilationOptions(outputKind));
        var sourceTree = compilation.SyntaxTrees.Single();
        return (compilation, sourceTree, compilation.GetSemanticModel(sourceTree));
    }

    public static IEnumerable<MetadataReference> GetCurrentReferences() =>
        AppDomain.CurrentDomain.GetAssemblies()
            .Where(assembly => !assembly.IsDynamic && !string.IsNullOrWhiteSpace(assembly.Location))
            .Select(assembly => MetadataReference.CreateFromFile(assembly.Location))
            .ToList().AsReadOnly();

    public static GeneratorDriver CreateDriver(Compilation c, params ISourceGenerator[] generators)
        => CSharpGeneratorDriver.Create(generators, parseOptions: (CSharpParseOptions)c.SyntaxTrees.First().Options);

    public static Compilation RunGenerators(Compilation c, out IReadOnlyList<Diagnostic> diagnostics, params ISourceGenerator[] generators)
    {
        CreateDriver(c, generators).RunGeneratorsAndUpdateCompilation(c, out var compilation, out var diagnosticsArray);
        diagnostics = diagnosticsArray;
        return compilation;
    }

    public static Accessibility Minimum(Accessibility accessibility1, Accessibility accessibility2)
    {
        if (accessibility1 == Accessibility.Private || accessibility2 == Accessibility.Private)
            return Accessibility.Private;

        if (accessibility1 == Accessibility.ProtectedAndInternal || accessibility2 == Accessibility.ProtectedAndInternal)
            return Accessibility.ProtectedAndInternal;

        if (accessibility1 == Accessibility.Protected || accessibility2 == Accessibility.Protected)
            return Accessibility.Protected;

        if (accessibility1 == Accessibility.Internal || accessibility2 == Accessibility.Internal)
            return Accessibility.Internal;

        if (accessibility1 == Accessibility.ProtectedOrInternal || accessibility2 == Accessibility.ProtectedOrInternal)
            return Accessibility.ProtectedOrInternal;

        return Accessibility.Public;
    }

    #endregion
}