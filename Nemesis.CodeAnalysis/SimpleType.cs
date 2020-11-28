using System;
using System.Collections.Generic;
using System.ComponentModel;

using JetBrains.Annotations;

using Microsoft.CodeAnalysis;

#nullable enable

namespace Nemesis.CodeAnalysis
{
    [PublicAPI]
    public readonly struct SimpleType : IEquatable<SimpleType>, IComparable<SimpleType>, IComparable
    {
        public string? Namespace { get; }
        public string Name { get; }
        public SimpleTypeKind Kind { get; }

        public bool IsValid => !string.IsNullOrEmpty(Name);

        public SimpleType(string? @namespace, string name, SimpleTypeKind kind)
        {
            if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException($@"{nameof(name)} cannot be null nor whitespace", nameof(name));
            if (!Enum.IsDefined(typeof(SimpleTypeKind), kind)) throw new InvalidEnumArgumentException(nameof(kind), (int)kind, typeof(SimpleTypeKind));

            Namespace = @namespace;
            Name = name;
            Kind = kind;
        }

        public void Deconstruct(out string? ns, out string name, out SimpleTypeKind kind)
        {
            ns = Namespace;
            name = Name;
            kind = Kind;
        }

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

        #region Equals and compare

        public bool Equals(SimpleType other) => Namespace == other.Namespace && Name == other.Name && Kind == other.Kind;

        public override bool Equals(object? obj) => obj is SimpleType other && Equals(other);

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = Namespace?.GetHashCode() ?? 0;
                hashCode = (hashCode * 397) ^ Name.GetHashCode();
                hashCode = (hashCode * 397) ^ (int)Kind;
                return hashCode;
            }
        }


        public static bool operator ==(SimpleType left, SimpleType right) => left.Equals(right);
        public static bool operator !=(SimpleType left, SimpleType right) => !left.Equals(right);

        public int CompareTo(SimpleType other)
        {
            var namespaceComparison = string.Compare(Namespace, other.Namespace, StringComparison.Ordinal);
            if (namespaceComparison != 0) return namespaceComparison;
            var nameComparison = string.Compare(Name, other.Name, StringComparison.Ordinal);
            if (nameComparison != 0) return nameComparison;
            return Kind.CompareTo(other.Kind);
        }

        public int CompareTo(object? obj) =>
            obj switch
            {
                null => 1,
                SimpleType other => CompareTo(other),
                _ => throw new ArgumentException($"Object must be of type {nameof(SimpleType)}")
            };

        public static bool operator <(SimpleType left, SimpleType right) => left.CompareTo(right) < 0;
        public static bool operator >(SimpleType left, SimpleType right) => left.CompareTo(right) > 0;
        public static bool operator <=(SimpleType left, SimpleType right) => left.CompareTo(right) <= 0;
        public static bool operator >=(SimpleType left, SimpleType right) => left.CompareTo(right) >= 0;

        #endregion
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
        /// <summary>Type is a C# struct or VB Structure</summary>
        Structure = 10, // 0x0A
        /// <summary>Type is a type parameter.</summary>
        TypeParameter = 11, // 0x0B
        /// <summary>Type is an interactive submission.</summary>
        Submission = 12, // 0x0C
        /// <summary>Type is a function pointer.</summary>
        FunctionPointer = 13, // 0x0D

        //Missing from Roslyn
        AnonymousType = 100,
    }
}
