using JetBrains.Annotations;

#nullable enable

namespace Nemesis.CodeAnalysis;

[PublicAPI]
public sealed record FieldMeta(string DeclaredInClass, string FieldName, SimpleType Type, string? Initializer, string Documentation) : IComparable<FieldMeta>, IComparable
{
    public static IEnumerable<FieldMeta> GetFieldsMeta(ClassDeclarationSyntax @class, SemanticModel semanticModel)
    {
        var fields =
                @class.ChildNodes().OfType<FieldDeclarationSyntax>()
                    .Select(f => f.Declaration)
                    .SelectMany(
                        decl => FromFieldDeclaration(decl, @class.Identifier.ValueText, semanticModel)
                    ).ToList().AsReadOnly()
            ;
        return fields;
    }

    public static FieldMeta FromFieldSymbol(IFieldSymbol fieldRef, string initializer, string documentation) =>
        new(fieldRef.ContainingType.Name, fieldRef.Name, SimpleType.FromTypeSymbol(fieldRef.Type), initializer, documentation);

    public static IEnumerable<FieldMeta> FromFieldDeclaration(VariableDeclarationSyntax fieldDeclaration, string declaredIn, SemanticModel semanticModel)
    {
        var type = SimpleType.FromTypeSymbol(semanticModel.GetTypeInfo(fieldDeclaration.Type)) ??
            throw new NullReferenceException($"Type stored in '{fieldDeclaration.Type}' cannot be null");

        return fieldDeclaration.Variables.Select(
                variable => new FieldMeta
                (
                    declaredIn,
                    variable.Identifier.ValueText,
                    type,
                    LocalVariableMeta.GetInitializerValue(variable.Initializer?.Value, semanticModel),
                    TriviaUtils.GetCommentsFromNode(variable)
                )
            ).ToList();
    }

    public override string ToString() => $"{DeclaredInClass}.{FieldName} = ({Initializer}) of type {Type}";


    public int CompareTo(FieldMeta? other)
    {
        if (ReferenceEquals(this, other)) return 0;
        if (other is null) return 1;

        return (DeclaredInClass, FieldName, Type, Initializer, Documentation).CompareTo(
            (other.DeclaredInClass, other.FieldName, other.Type, other.Initializer, other.Documentation)
            );
    }

    public int CompareTo(object? obj) => obj switch
    {
        null => 1,
        { } o when ReferenceEquals(this, o) => 0,
        FieldMeta other => CompareTo(other),
        _ => throw new ArgumentException($"Object must be of type {nameof(FieldMeta)}")

    };
}
