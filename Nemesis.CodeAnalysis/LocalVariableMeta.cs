#nullable enable

namespace Nemesis.CodeAnalysis;

public sealed record LocalVariableMeta(string DeclaredInMethod, string Name, SimpleType Type, string? Initializer, string Documentation) : IComparable<LocalVariableMeta>, IComparable
{
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
        var type = SimpleType.FromTypeSymbol(semanticModel.GetTypeInfo(variableDeclaration.Type)) ??
                   throw new NullReferenceException($"Type stored in '{variableDeclaration.Type}' cannot be null");

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

    internal static string? GetInitializerValue(ExpressionSyntax? initValue, SemanticModel semanticModel) =>
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


    public int CompareTo(LocalVariableMeta? other)
    {
        if (ReferenceEquals(this, other)) return 0;
        if (other is null) return 1;
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

    public int CompareTo(object? obj) => obj switch
    {
        null => 1,
        { } o when ReferenceEquals(this, o) => 0,
        LocalVariableMeta other => CompareTo(other),
        _ => throw new ArgumentException($"Object must be of type {nameof(LocalVariableMeta)}")
    };
}
