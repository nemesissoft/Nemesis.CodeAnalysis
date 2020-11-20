using System;
using System.ComponentModel;
using Microsoft.CodeAnalysis;

namespace Nemesis.CodeAnalysis
{
    public readonly struct SimpleType : IEquatable<SimpleType>, IComparable<SimpleType>, IComparable
    {
        public string Namespace { get; }
        public string Name { get; }
        public SimpleTypeKind Kind { get; }

        public SimpleType(string @namespace, string name, SimpleTypeKind kind)
        {
            if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException($@"{nameof(name)} cannot be null nor whitespace", nameof(name));
            if (!Enum.IsDefined(typeof(SimpleTypeKind), kind)) throw new InvalidEnumArgumentException(nameof(kind), (int)kind, typeof(SimpleTypeKind));

            Namespace = @namespace;
            Name = name;
            Kind = kind;
        }

        public void Deconstruct(out string ns, out string name, out SimpleTypeKind kind)
        {
            ns = Namespace;
            name = Name;
            kind = Kind;
        }

        public static SimpleType FromTypeSymbol(TypeInfo typeInfo) => FromTypeSymbol(typeInfo.Type);

        public static SimpleType FromTypeSymbol(ITypeSymbol typeSymbol)
        {
            string @namespace;

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

        public override string ToString() => string.IsNullOrWhiteSpace(Namespace) ? Name : $"{Namespace}.{Name}";

        #region Equals and compare

        public bool Equals(SimpleType other) => string.Equals(Namespace, other.Namespace) && string.Equals(Name, other.Name) && Kind == other.Kind;

        public override bool Equals(object obj) => !(obj is null) && obj is SimpleType type && Equals(type);

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = (Namespace != null ? Namespace.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ (Name != null ? Name.GetHashCode() : 0);
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

        public int CompareTo(object obj)
        {
            if (obj is null) return 1;
            if (!(obj is SimpleType)) throw new ArgumentException($"Object must be of type {nameof(SimpleType)}");
            return CompareTo((SimpleType)obj);
        }

        public static bool operator <(SimpleType left, SimpleType right) => left.CompareTo(right) < 0;

        public static bool operator >(SimpleType left, SimpleType right) => left.CompareTo(right) > 0;

        public static bool operator <=(SimpleType left, SimpleType right) => left.CompareTo(right) <= 0;

        public static bool operator >=(SimpleType left, SimpleType right) => left.CompareTo(right) >= 0;

        #endregion
    }

    public enum SimpleTypeKind : byte
    {
        Unknown = 0,
        Array = 1,
        Class = 2,
        Delegate = 3,
        Dynamic = 4,
        Enum = 5,
        Error = 6,
        Interface = 7,
        Module = 8,
        Pointer = 9,
        Struct = 10,
        Structure = 10,
        TypeParameter = 11,
        Submission = 12,

        //Missing from Roslyn
        AnonymousType = 100,
    }
}
