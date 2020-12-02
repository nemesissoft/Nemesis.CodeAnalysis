using System;
using System.Collections.Generic;
using System.Linq;

using JetBrains.Annotations;

using Microsoft.CodeAnalysis;

namespace Nemesis.CodeAnalysis
{
    [PublicAPI]
    public static class SymbolUtils
    {
        #region Metadata

        public static IEnumerable<IMethodSymbol> GetMethods(this INamespaceOrTypeSymbol symbol, string methodName = null)
            => symbol.GetMembers()
                .Where(s => s.Kind == SymbolKind.Method).OfType<IMethodSymbol>()
                .Where(m => string.IsNullOrEmpty(methodName) || m.Name == methodName)
                .ToList();

        public static IEnumerable<IMethodSymbol> GetInstanceConstructors(this INamespaceOrTypeSymbol symbol)
        {
            return symbol.GetMembers()
                .Where(s => s.Kind == SymbolKind.Method).OfType<IMethodSymbol>()
                .Where(ms => ms.MethodKind == MethodKind.Constructor)
                .ToList();
        }

        public static bool ImplementsInterface(this ITypeSymbol typeSymbol, ISymbol @base) =>
            typeSymbol.ToString() == @base.ToString() || typeSymbol.AllInterfaces.Any(i => i.ToString() == @base.ToString());

        public static IEnumerable<INamedTypeSymbol> GetInterfaceHierarchy(this ITypeSymbol symbol, ITypeSymbol baseInterface)
        {
            while (symbol.BaseType?.ImplementsInterface(baseInterface) ?? false)
            {
                var @base = symbol.BaseType;
                if (nameof(Object) == @base.Name && typeof(object).Namespace == @base.ContainingNamespace.ToString())
                    yield break;
                yield return @base;
                symbol = @base;
            }
        }

        public static IEnumerable<INamedTypeSymbol> GetSymbolHierarchy(this ITypeSymbol symbol)
        {
            while (symbol.BaseType != null)
            {
                var @base = symbol.BaseType;

                if (nameof(Object) == @base.Name && typeof(object).Namespace == @base.ContainingNamespace.ToString())
                    yield break;

                yield return @base;
                symbol = @base;
            }
        }

        public static IEnumerable<INamedTypeSymbol> GetSymbolHierarchy(this ITypeSymbol symbol, Compilation compilation)
        {
            INamedTypeSymbol objType = compilation.GetSpecialType(SpecialType.System_Object);

            while (symbol.BaseType != null)
            {
                var @base = symbol.BaseType;

                if (@base.Equals(objType, SymbolEqualityComparer.Default))
                    yield break;

                yield return @base;
                symbol = @base;
            }
        }

        public static IEnumerable<ITypeSymbol> GetBaseTypesAndThis(this ITypeSymbol type)
        {
            var current = type;
            while (current != null)
            {
                yield return current;
                current = current.BaseType;
            }
        }

        #endregion


        #region Symbol queries

        public static bool IsRefOrOut(this IParameterSymbol symbol) => symbol.RefKind == RefKind.Ref || symbol.RefKind == RefKind.Out;

        public static bool IsPropertyAccessor(this MethodKind kind) => kind == MethodKind.PropertyGet || kind == MethodKind.PropertySet;

        public static IEnumerable<IPropertySymbol> GetValidAnonymousTypeProperties(this ISymbol symbol)
        {
            if (!symbol.IsNormalAnonymousType()) throw new ArgumentException($@"{nameof(symbol)} parameter should point to anonymous type", nameof(symbol));
            return ((INamedTypeSymbol)symbol).GetMembers().OfType<IPropertySymbol>().Where(p => p.CanBeReferencedByName);
        }

        public static bool IsNormalAnonymousType(this ISymbol symbol) => symbol.IsAnonymousType() && !symbol.IsDelegateType();

        public static bool IsAnonymousType(this ISymbol symbol) => symbol is INamedTypeSymbol {IsAnonymousType: true};

        public static bool IsDelegateType(this ISymbol symbol) => symbol is ITypeSymbol {TypeKind: TypeKind.Delegate};

        #endregion

        public static TypeCode ToTypeCode(this SpecialType specialType) =>
            specialType switch
            {
                SpecialType.System_Object => TypeCode.Object,
                SpecialType.System_Boolean => TypeCode.Boolean,
                SpecialType.System_Char => TypeCode.Char,
                SpecialType.System_SByte => TypeCode.SByte,
                SpecialType.System_Byte => TypeCode.Byte,
                SpecialType.System_Int16 => TypeCode.Int16,
                SpecialType.System_UInt16 => TypeCode.UInt16,
                SpecialType.System_Int32 => TypeCode.Int32,
                SpecialType.System_UInt32 => TypeCode.UInt32,
                SpecialType.System_Int64 => TypeCode.Int64,
                SpecialType.System_UInt64 => TypeCode.UInt64,
                SpecialType.System_Decimal => TypeCode.Decimal,
                SpecialType.System_Single => TypeCode.Single,
                SpecialType.System_Double => TypeCode.Double,
                SpecialType.System_String => TypeCode.String,
                SpecialType.System_DateTime => TypeCode.DateTime,
                _ => TypeCode.Empty
            };
    }
}


/*


 public static IEnumerable<(ClassDeclarationSyntax Class, INamedTypeSymbol Symbol)> GetClassDeclarationToSymbolPairs(
            this SemanticModel semanticModel, Func<INamedTypeSymbol, bool> filterPredicate = null)
        {
            return
                from @class in semanticModel.SyntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
                let symbol = semanticModel.GetDeclaredSymbol(@class)
                where filterPredicate == null || filterPredicate(symbol)
                select (@class, symbol);
        }

    public static IDictionary<string, string> GetFieldsNameToTypeLookup(this INamedTypeSymbol symbol)
            => symbol.GetMembers().Where(s => s.Kind == SymbolKind.Field).OfType<IFieldSymbol>()
                .ToReadOnlyDictionary(fs => fs.Name, fs => fs.Type.ToString());

        public static IDictionary<string, string> GetAllFieldsNameToTypeLookup(this INamedTypeSymbol @class)
        {
            var allAncestorsFields = GetSymbolHierarchy(@class)
                .SelectMany(c => c.GetMembers().Where(s => s.Kind == SymbolKind.Field).OfType<IFieldSymbol>());

            var classFields = @class.GetMembers()
                .Where(s => s.Kind == SymbolKind.Field)
                .OfType<IFieldSymbol>();

            var fields = classFields.Concat(allAncestorsFields).DistinctBy(f => f.Name);
            return fields.ToReadOnlyDictionary(fs => fs.Name, fs => fs.Type.ToString());
        }
        public static IDictionary<string, ITypeSymbol> GetAllFieldsNameToTypeSymbolLookup(this INamedTypeSymbol @class)
        {
            var allAncestorsFields = GetSymbolHierarchy(@class)
                .SelectMany(c => c.GetMembers().Where(s => s.Kind == SymbolKind.Field).OfType<IFieldSymbol>());

            var classFields = @class.GetMembers()
                .Where(s => s.Kind == SymbolKind.Field)
                .OfType<IFieldSymbol>();

            var fields = classFields.Concat(allAncestorsFields).DistinctBy(f => f.Name);
            return fields.ToReadOnlyDictionary(fs => fs.Name, fs => fs.Type);
        }
         
            public static IEnumerable<string> GetHierarchy(this ITypeSymbol symbol, ITypeSymbol baseInterface, bool useFullyDistinguishedNames = false)
        {
            while (symbol.BaseType?.ImplementsInterface(baseInterface) ?? false)
            {
                var @base = symbol.BaseType;
                if (typeof(object).Name != @base.Name && typeof(object).Namespace != @base.ContainingNamespace.ToString())
                    yield return useFullyDistinguishedNames ? @base.ToString() : @base.Name;
                symbol = @base;
            }
        }*/
