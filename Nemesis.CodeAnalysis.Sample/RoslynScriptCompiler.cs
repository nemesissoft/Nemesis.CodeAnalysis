﻿using System.Collections.Concurrent;
using System.Reflection;
using Microsoft.CodeAnalysis.Emit;

namespace Nemesis.CodeAnalysis.Sample;

class RoslynScriptCompiler
{
    public static void SampleCompilation()
    {
        //var compiler1 = new CodeDomCompiler();
        var compiler2 = new RoslynCompiler();
        //var compiler3 = new CachedCompiler(compiler1);
        var compiler4 = new CachedCompiler(compiler2);

        string className = $"Class_{Guid.NewGuid():N}";
        string classDeclaration = $"public class {className}: {typeof(IFunction<>).Namespace}.IFunction<string>";

        string code = "using System;" + classDeclaration + @"
{
    public string Run() 
    {
        return ""Hello world from "" + GetType().Assembly.FullName;
    }
}";
        var instruction = new CompilerInstructions(code, className, new[] { typeof(IFunction<string>).Assembly.Location, typeof(object).Assembly.Location });

        foreach (var compiler in new ICompiler[] { /*compiler1,*/ compiler2, /*compiler3,*/ compiler4 })
        {
            string result = compiler.RunProducer<string>(instruction);
            Console.WriteLine($@"Result from {compiler}: {result}");
        }
    }
}

public interface IFunction<out TResult> { TResult Run(); }

public interface IScript { void Run(); }

public interface ICompiler
{
    Assembly Compile(string code, IEnumerable<string> assemblyLocations);
}

/*public sealed class CodeDomCompiler : ICompiler
{
    public Assembly Compile(string code, IEnumerable<string> assemblyLocations)
    {
        using (CSharpCodeProvider compiler = new CSharpCodeProvider())
        {
            var parameters = new CompilerParameters { GenerateExecutable = false, GenerateInMemory = true };

            foreach (string assemblyLocation in assemblyLocations)
                parameters.ReferencedAssemblies.Add(assemblyLocation);

            var result = compiler.CompileAssemblyFromSource(parameters, code);

            if (result.Errors.Count > 0) throw new CodeDomCompilerException("Assembly could not be created.", result);

            try
            {
                return result.CompiledAssembly;
            }
            catch (Exception ex)
            {
                throw new CodeDomCompilerException("Assembly could not be created.", result, ex);
            }
        }
    }

    public override string ToString() => GetType().Name;
}

[Serializable]
public class CodeDomCompilerException : Exception
{
    public CompilerResults Result { get; }

    public CodeDomCompilerException(CompilerResults result) => Result = result;

    public CodeDomCompilerException(string message, CompilerResults result) : base(message) => Result = result;

    public CodeDomCompilerException(string message, CompilerResults result, Exception inner) : base(message, inner) => Result = result;

    protected CodeDomCompilerException(SerializationInfo info, StreamingContext context) : base(info, context)
    {
        if (info == null) throw new ArgumentNullException(nameof(info));

        Result = (CompilerResults)info.GetValue("Result", typeof(CompilerResults));
    }

    [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
    public override void GetObjectData(SerializationInfo info, StreamingContext context)
    {
        if (info == null) throw new ArgumentNullException(nameof(info));

        base.GetObjectData(info, context);

        info.AddValue("Result", Result);
    }
}*/

public sealed class RoslynCompiler : ICompiler
{
    public CSharpCompilationOptions Options { get; } = new CSharpCompilationOptions(
        OutputKind.DynamicallyLinkedLibrary,
        reportSuppressedDiagnostics: true,
        optimizationLevel: OptimizationLevel.Release,
        generalDiagnosticOption: ReportDiagnostic.Error,
        allowUnsafe: true
    );

    /// Compiles the specified code the specified assembly locations.
    public Assembly Compile(string code, IEnumerable<string> assemblyLocations)
    {
        var references = assemblyLocations.Select(loc => MetadataReference.CreateFromFile(loc));

        var compilation = CSharpCompilation.Create($"_{Guid.NewGuid():D}",
            references: references,
            syntaxTrees: new[] { CSharpSyntaxTree.ParseText(code) },
            options: Options
        );

        using var ms = new MemoryStream();
        var compilationResult = compilation.Emit(ms);

        return compilationResult.Success
            ? Assembly.Load(ms.ToArray())
            : throw new RoslynCompilationException("Assembly could not be created.", compilationResult);
    }

    public override string ToString() => GetType().Name;
}

[Serializable]
public class RoslynCompilationException : Exception
{
    public EmitResult Result { get; }

    public RoslynCompilationException(EmitResult result) => Result = result;

    public RoslynCompilationException(string message, EmitResult result) : base(message) => Result = result;

    public RoslynCompilationException(string message, EmitResult result, Exception inner) : base(message, inner) =>
        Result = result;
}

public sealed class CachedCompiler : ICompiler
{
    private readonly ConcurrentDictionary<string, Assembly> _cache = new();
    private readonly ICompiler _compiler;

    public CachedCompiler(ICompiler compiler) => _compiler = compiler ?? throw new ArgumentNullException(nameof(compiler));

    public Assembly Compile(string code, IEnumerable<string> assemblyLocations)
    {
        string GetCacheKey() => String.Join("|", code, assemblyLocations);

        string key = GetCacheKey();

        return _cache.GetOrAdd(key, k => _compiler.Compile(code, assemblyLocations));
    }

    public override string ToString() => $"{nameof(CachedCompiler)} over {_compiler.GetType().Name}";
}

public interface ICompilerInstructions
{
    string Code { get; }

    string ClassName { get; }

    IEnumerable<string> AssemblyLocations { get; }
}

public class CompilerInstructions : ICompilerInstructions
{
    public string Code { get; }

    public string ClassName { get; }

    public IEnumerable<string> AssemblyLocations { get; }

    public CompilerInstructions(string code, string className, IEnumerable<string>? assemblyLocations = null)
    {
        Code = code;
        ClassName = className;
        AssemblyLocations = (assemblyLocations ?? Array.Empty<string>()).ToList();
    }
}

public static class CompilerExtensions
{
    public static object CompileAndCreateObject(this ICompiler compiler, ICompilerInstructions instructions, params object[] constructorParameters)
    {
        var assembly = compiler.Compile(instructions.Code, instructions.AssemblyLocations);

        var type = assembly.GetType(instructions.ClassName) ?? throw new($"'{instructions.ClassName}' type cannot be found");

        return Activator.CreateInstance(type, constructorParameters) ?? throw new("Type cannot be instantiated");
    }

    public static T CompileAndCreateObject<T>(this ICompiler compiler, ICompilerInstructions instructions, params object[] constructorParameters) =>
        (T)compiler.CompileAndCreateObject(instructions, constructorParameters);

    public static TResult RunProducer<TResult>(this ICompiler compiler, ICompilerInstructions instructions, params object[] constructorParameters) =>
        CompileAndCreateObject<IFunction<TResult>>(compiler, instructions, constructorParameters).Run();

    /// Runs the script.
    public static void RunScript(this ICompiler compiler, ICompilerInstructions instructions, params object[] constructorParameters) =>
        CompileAndCreateObject<IScript>(compiler, instructions, constructorParameters).Run();
}
