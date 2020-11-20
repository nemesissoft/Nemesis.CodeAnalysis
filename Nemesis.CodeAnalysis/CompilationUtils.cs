using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

namespace Nemesis.CodeAnalysis
{
    public static class CompilationUtils
    {
        #region Evaluation

        public static bool TryEvaluateExpression(string code, out string result)
        {
            try
            {
                var scriptOptions = ScriptOptions.Default;
                result = CSharpScript.EvaluateAsync(code, scriptOptions).Result?.ToString();
                return true;
            }
            catch (Exception e)
            {
                result = $"<error in '{code}' {e.Message}>";
                return false;
            }
        }

        public static Type GetTypeFromCSharpName(string typeName, IEnumerable<string> additionalNamespaces = null, params Assembly[] additionalAssemblies)
        {
            var standardNamespaces = new[] { "System", "System.Linq", "System.Collections.Generic" };

            var scriptOptions = ScriptOptions.Default
                .AddReferences(new[] { typeof(object).Assembly, typeof(Enumerable).Assembly }.Concat(additionalAssemblies))
                .AddImports(additionalNamespaces == null ? standardNamespaces : standardNamespaces.Concat(additionalNamespaces));

            var state = CSharpScript.RunAsync<Type>($"typeof({typeName})", scriptOptions).Result;
            return state.ReturnValue;
        }

        private static readonly ConcurrentDictionary<string, Type> _nameToTypeMap = new ConcurrentDictionary<string, Type>();
        public static Type GetTypeFromCSharpNameCached(string csharpName, IEnumerable<string> additionalNamespaces = null, params Assembly[] additionalAssemblies)
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
            string source, Assembly[] additionalAssemblies = null, [CallerMemberName]string memberName = null)
        {
            var tree = CSharpSyntaxTree.ParseText(source, 
                    CSharpParseOptions.Default.WithLanguageVersion(LanguageVersion.Latest)
                    )
                /*.WithFilePath(path)*/;

            IEnumerable<Assembly> assemblies = new[] { typeof(object).Assembly };
            if (additionalAssemblies != null)
                assemblies = assemblies.Concat(additionalAssemblies);

            var references = assemblies.Select(ass => MetadataReference.CreateFromFile(ass.Location));

            Compilation compilation = CSharpCompilation.Create($"{memberName}_Compilation", new[] { tree }, references, new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            SyntaxTree sourceTree = compilation.SyntaxTrees.Single();
            return (compilation, sourceTree, compilation.GetSemanticModel(sourceTree));
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
}