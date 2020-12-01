using System;
using System.Collections.Generic;
using System.Linq;

using JetBrains.Annotations;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

#nullable enable

namespace Nemesis.CodeAnalysis
{
    [PublicAPI]
    public readonly struct FieldMeta : IEquatable<FieldMeta>, IComparable<FieldMeta>, IComparable
    {
        public string DeclaredInClass { get; }
        public string FieldName { get; }
        public SimpleType Type { get; }
        public string Initializer { get; }
        public string Documentation { get; }

        public FieldMeta(string declaredInClass, string fieldName, SimpleType type, string initializer, string documentation)
        {
            DeclaredInClass = declaredInClass;
            FieldName = fieldName;
            Type = type;
            Initializer = initializer;
            Documentation = documentation;
        }

        public static IEnumerable<FieldMeta> GetFieldMeta(ClassDeclarationSyntax @class, SemanticModel semanticModel)
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

        public bool Equals(FieldMeta other) =>
            string.Equals(DeclaredInClass, other.DeclaredInClass) && string.Equals(FieldName, other.FieldName) && Type.Equals(other.Type);

        public override bool Equals(object obj) => obj is FieldMeta meta && Equals(meta);

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = (DeclaredInClass != null ? DeclaredInClass.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ (FieldName != null ? FieldName.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ Type.GetHashCode();
                return hashCode;
            }
        }

        public static bool operator ==(FieldMeta left, FieldMeta right) => left.Equals(right);

        public static bool operator !=(FieldMeta left, FieldMeta right) => !left.Equals(right);

        public int CompareTo(FieldMeta other)
        {
            var declaredInClassComparison = string.Compare(DeclaredInClass, other.DeclaredInClass, StringComparison.Ordinal);
            if (declaredInClassComparison != 0) return declaredInClassComparison;
            var fieldNameComparison = string.Compare(FieldName, other.FieldName, StringComparison.Ordinal);
            if (fieldNameComparison != 0) return fieldNameComparison;
            return Type.CompareTo(other.Type);
        }
        public int CompareTo(object obj)
        {
            if (obj is null) return 1;
            return obj is FieldMeta meta
                ? CompareTo(meta)
                : throw new ArgumentException($"Object must be of type {nameof(FieldMeta)}");
        }
    }
}
