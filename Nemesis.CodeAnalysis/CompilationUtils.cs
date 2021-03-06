﻿using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

namespace Nemesis.CodeAnalysis
{
    [PublicAPI]
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

        private static readonly ConcurrentDictionary<string, Type> _nameToTypeMap = new();
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
            string source, Assembly[] additionalAssemblies = null, OutputKind outputKind = OutputKind.DynamicallyLinkedLibrary, [CallerMemberName]string memberName = null)
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

            
            if (additionalAssemblies != null)
                foreach (var ass in additionalAssemblies)
                    references.Add(MetadataReference.CreateFromFile(ass.Location));

            
            var compilation = CSharpCompilation.Create($"{memberName}_Compilation", new[] { tree }, references, new CSharpCompilationOptions(outputKind));
            var sourceTree = compilation.SyntaxTrees.Single();
            return (compilation, sourceTree, compilation.GetSemanticModel(sourceTree));
        }


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
}