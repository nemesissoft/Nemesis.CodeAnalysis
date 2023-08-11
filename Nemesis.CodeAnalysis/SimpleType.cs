using System.ComponentModel;

using JetBrains.Annotations;

#nullable enable

namespace Nemesis.CodeAnalysis;

[PublicAPI]
public sealed record SimpleType(string? Namespace, string Name, SimpleTypeKind Kind) : IComparable<SimpleType>, IComparable
{
    public void Validate()
    {
        if (string.IsNullOrWhiteSpace(Name)) throw new ArgumentException($@"{nameof(Name)} cannot be null nor whitespace", nameof(Name));
        if (!Enum.IsDefined(typeof(SimpleTypeKind), Kind)) throw new InvalidEnumArgumentException(nameof(Kind), (int)Kind, typeof(SimpleTypeKind));
    }

    public bool IsValid => !string.IsNullOrEmpty(Name);

    public static SimpleType? FromTypeSymbol(TypeInfo typeInfo) => typeInfo.Type is { } type ? FromTypeSymbol(type) : null;

    public static SimpleType FromTypeSymbol(ITypeSymbol typeSymbol)
    {
        string? @namespace;

        if (typeSymbol is IArrayTypeSymbol arraySymbol)
        {
            ITypeSymbol elementSymbol = arraySymbol.ElementType;
            while (elementSymbol is IArrayTypeSymbol innerArray)
                elementSymbol = innerArray.ElementType;

            @namespace = elementSymbol.ContainingNamespace.ToString();
        }
        else if (typeSymbol.TypeKind == TypeKind.Error || typeSymbol.TypeKind == TypeKind.Dynamic)
            @namespace = null;
        else
            @namespace = typeSymbol.ContainingNamespace.ToString();

        string name = typeSymbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        /*if (name.Contains(@namespace)) name = name.Substring(@namespace.Length + 1);*/

        return new SimpleType(@namespace, name, typeSymbol.IsAnonymousType ? SimpleTypeKind.AnonymousType : (SimpleTypeKind)typeSymbol.TypeKind);
    }

    public static void ExtractNamespaces(ITypeSymbol typeSymbol, ICollection<string> namespaces)
    {
        if (typeSymbol is INamedTypeSymbol { IsGenericType: true } namedType) //namedType.TypeParameters for unbound generics
        {
            namespaces.Add(namedType.ContainingNamespace.ToDisplayString());

            foreach (var arg in namedType.TypeArguments)
                ExtractNamespaces(arg, namespaces);
        }
        else if (typeSymbol is IArrayTypeSymbol arraySymbol)
        {
            namespaces.Add("System");

            ITypeSymbol elementSymbol = arraySymbol.ElementType;
            while (elementSymbol is IArrayTypeSymbol innerArray)
                elementSymbol = innerArray.ElementType;

            ExtractNamespaces(elementSymbol, namespaces);
        }
        /*else if (typeSymbol.TypeKind == TypeKind.Error || typeSymbol.TypeKind == TypeKind.Dynamic)
        {
            //add appropriate reference to your compilation 
        }*/
        else
            namespaces.Add(typeSymbol.ContainingNamespace.ToDisplayString());
    }

    public override string ToString() => string.IsNullOrWhiteSpace(Namespace) ? Name : $"{Namespace}.{Name}";

    public int CompareTo(SimpleType? other)
    {
        if (ReferenceEquals(this, other)) return 0;
        if (other is null) return 1;
        var namespaceComparison = string.Compare(Namespace, other.Namespace, StringComparison.Ordinal);
        if (namespaceComparison != 0) return namespaceComparison;
        var nameComparison = string.Compare(Name, other.Name, StringComparison.Ordinal);
        if (nameComparison != 0) return nameComparison;
        return Kind.CompareTo(other.Kind);
    }

    public int CompareTo(object? obj) => obj switch
    {
        null => 1,
        { } o when ReferenceEquals(this, o) => 0,
        SimpleType other => CompareTo(other),
        _ => throw new ArgumentException($"Object must be of type {nameof(SimpleType)}")

    };
}

[PublicAPI]
public enum SimpleTypeKind : byte
{
    /// <summary>Type's kind is undefined.</summary>
    Unknown = 0,
    /// <summary>Type is an array type.</summary>
    Array = 1,
    /// <summary>Type is a class.</summary>
    Class = 2,
    /// <summary>Type is a delegate.</summary>
    Delegate = 3,
    /// <summary>Type is dynamic.</summary>
    Dynamic = 4,
    /// <summary>Type is an enumeration.</summary>
    Enum = 5,
    /// <summary>Type is an error type.</summary>
    Error = 6,
    /// <summary>Type is an interface.</summary>
    Interface = 7,
    /// <summary>Type is a module.</summary>
    Module = 8,
    /// <summary>Type is a pointer.</summary>
    Pointer = 9,
    /// <summary>Type is a C# struct or VB Structure</summary>
    Struct = 10, // 0x0A
    /// <summary>Type is a type parameter.</summary>
    TypeParameter = 11, // 0x0B
    /// <summary>Type is an interactive submission.</summary>
    Submission = 12, // 0x0C
    /// <summary>Type is a function pointer.</summary>
    FunctionPointer = 13, // 0x0D

    //Missing from Roslyn
    AnonymousType = 100,
}
