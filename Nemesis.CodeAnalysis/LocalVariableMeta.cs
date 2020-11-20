using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Nemesis.CodeAnalysis
{
    public readonly struct LocalVariableMeta : IEquatable<LocalVariableMeta>, IComparable<LocalVariableMeta>, IComparable
    {
        public string DeclaredInMethod { get; }
        public string Name { get; }
        public SimpleType Type { get; }
        public string Initializer { get; }
        public string Documentation { get; }

        public LocalVariableMeta(string declaredInMethod, string name, SimpleType type, string initializer, string documentation)
        {
            DeclaredInMethod = declaredInMethod;
            Name = name;
            Type = type;
            Initializer = initializer;
            Documentation = documentation;
        }

        public static IEnumerable<LocalVariableMeta> GetLocalVariables(MethodDeclarationSyntax method, SemanticModel semanticModel)
        {
            var localVars =
                    method.DescendantNodes()
                        .OfType<LocalDeclarationStatementSyntax>()
                        .Select(f => f.Declaration)
                        .SelectMany(
                            decl => FromVariableDeclaration(decl, method.Identifier.ValueText, semanticModel)
                        ).ToList().AsReadOnly()
                ;
            return localVars;
        }

        public static IEnumerable<LocalVariableMeta> FromVariableDeclaration(VariableDeclarationSyntax variableDeclaration, string declaredIn, SemanticModel semanticModel)
        {
            var type = SimpleType.FromTypeSymbol(semanticModel.GetTypeInfo(variableDeclaration.Type).Type);

            return variableDeclaration.Variables.Select(
                    variable => new LocalVariableMeta
                    (
                        declaredIn,
                        variable.Identifier.ValueText,
                        type,
                        GetInitializerValue(variable.Initializer?.Value, semanticModel),
                        TriviaUtils.GetCommentsFromNode(variable)
                    )
                ).ToList();
        }

        internal static string GetInitializerValue(ExpressionSyntax initValue, SemanticModel semanticModel) =>
            initValue switch
            {
                null => null,
                LiteralExpressionSyntax literal => literal.ToString(),
                IdentifierNameSyntax ident2 when
                    semanticModel.GetSymbolInfo(ident2).Symbol is IFieldSymbol fs2
                    && fs2.DeclaringSyntaxReferences.Length > 0
                    && fs2.DeclaringSyntaxReferences[0].GetSyntax() is VariableDeclaratorSyntax vds
                    => GetInitializerValue(vds.Initializer?.Value, semanticModel),
                _ => initValue.ToString()
            };
        //TODO evaluate local field references
        /*else if (initValue is IdentifierNameSyntax ident1 && semanticModel.GetSymbolInfo(ident1).Symbol is IFieldSymbol fs1 && fs1.IsConst)
            return fs1.ConstantValue?.ToString();*/
        /*else if (initValue is BinaryExpressionSyntax || initValue is InvocationExpressionSyntax|| initValue is MemberAccessExpressionSyntax)//evaluations
        {
            (string Namespace, string Assembly) GetSymbolData(ISymbol symbol) => (symbol.ContainingNamespace.ToString(), symbol.ContainingAssembly.ToString());

            var children = initValue.DescendantNodesAndSelf().Select(n => semanticModel.GetSymbolInfo(n).Symbol).Where(s => !(s is null)).ToList();
            var symData = children.Select(GetSymbolData).ToList();

            var scriptOptions = ScriptOptions.Default
                .AddReferences(symData.Select(sd => sd.Assembly).Distinct())
                .AddImports(symData.Select(sd => sd.Namespace).Distinct());

            var state = CSharpScript.RunAsync(initValue.ToString(), scriptOptions).Result;

            return state.ReturnValue == null ? null :
                Microsoft.CodeAnalysis.CSharp.Scripting.Hosting.CSharpObjectFormatter.Instance.FormatObject(state.ReturnValue);
        }*/


        public override string ToString() => $"{DeclaredInMethod}\\{Name} = ({Initializer}) of type {Type}";

        public bool Equals(LocalVariableMeta other)
            => string.Equals(DeclaredInMethod, other.DeclaredInMethod) &&
               string.Equals(Name, other.Name) && Type.Equals(other.Type) &&
               string.Equals(Initializer, other.Initializer) &&
               string.Equals(Documentation, other.Documentation);

        public override bool Equals(object obj) => obj is LocalVariableMeta meta && Equals(meta);

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = (DeclaredInMethod != null ? DeclaredInMethod.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ (Name != null ? Name.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ Type.GetHashCode();
                hashCode = (hashCode * 397) ^ (Initializer != null ? Initializer.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ (Documentation != null ? Documentation.GetHashCode() : 0);
                return hashCode;
            }
        }

        public static bool operator ==(LocalVariableMeta left, LocalVariableMeta right) => left.Equals(right);

        public static bool operator !=(LocalVariableMeta left, LocalVariableMeta right) => !left.Equals(right);

        public int CompareTo(LocalVariableMeta other)
        {
            var declaredInMethodComparison = string.Compare(DeclaredInMethod, other.DeclaredInMethod, StringComparison.Ordinal);
            if (declaredInMethodComparison != 0) return declaredInMethodComparison;
            var nameComparison = string.Compare(Name, other.Name, StringComparison.Ordinal);
            if (nameComparison != 0) return nameComparison;
            var typeComparison = Type.CompareTo(other.Type);
            if (typeComparison != 0) return typeComparison;
            var initializerComparison = string.Compare(Initializer, other.Initializer, StringComparison.Ordinal);
            if (initializerComparison != 0) return initializerComparison;
            return string.Compare(Documentation, other.Documentation, StringComparison.Ordinal);
        }

        public int CompareTo(object obj)
        {
            if (obj is null) return 1;
            return obj is LocalVariableMeta meta
                ? CompareTo(meta)
                : throw new ArgumentException($"Object must be of type {nameof(LocalVariableMeta)}");
        }
    }
}
