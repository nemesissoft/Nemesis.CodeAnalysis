using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace Nemesis.CodeAnalysis
{
    public static class EquivalenceExtensions
    {
        public static bool CompatibleSignatureToDelegate(this IMethodSymbol method, INamedTypeSymbol delegateType)
        {
            if (delegateType.TypeKind != TypeKind.Delegate)
                throw new ArgumentException($@"{nameof(delegateType)} parameter must be a delegate type", nameof(delegateType));

            var invoke = delegateType.DelegateInvokeMethod; // It's possible to get events with no invoke method from metadata.  We will assume that no method can be an event handler for one.

            if (method.Parameters.Length != invoke?.Parameters.Length)
                return false;

            if (method.ReturnsVoid != invoke.ReturnsVoid)
                return false;

            if (!method.ReturnType.InheritsFromOrEquals(invoke.ReturnType))
                return false;

            for (int i = 0; i < method.Parameters.Length; i++)
                if (!invoke.Parameters[i].Type.InheritsFromOrEquals(method.Parameters[i].Type))
                    return false;

            return true;
        }

        // Determine if "type" inherits from "baseType", ignoring constructed types and interfaces, dealing only with original types.
        public static bool InheritsFromOrEquals(this ITypeSymbol type, ITypeSymbol baseType) => type.GetBaseTypesAndThis().Any(t => SymbolEquivalenceComparer.Instance.Equals(t, baseType));
    }

    /// <summary>
    /// Provides a way to test two symbols for equivalence.  While there are ways to ask for
    /// different sorts of equivalence, the following must hold for two symbols to be considered
    /// equivalent.
    /// 
    /// 1) The kinds of the two symbols must match.
    /// 
    /// 2) The names of the two symbols must match.
    /// 
    /// 3) The arity of the two symbols must match.
    /// 
    /// 4) If the symbols are methods or parameterized properties, then the signatures of the two
    /// symbols must match.
    /// 
    /// 5) Both symbols must be definitions or must be instantiations.  If they are instantiations,
    /// then they must be instantiated in the same manner.
    /// 
    /// 6) The containing symbols of the two symbols must be equivalent.
    /// 
    /// Note: equivalence does not concern itself with whole symbols.  Two types are considered
    /// equivalent if the above hold, even if one type has different members than the other.  Note:
    /// type parameters, and signature parameters are not considered 'children' when comparing
    /// symbols.
    /// 
    /// Options are provided to tweak the above slightly.  For example, by default, symbols are
    /// equivalent only if they come from the same assembly or different assemblies of the same simple name.
    /// However, one can ask if two symbols are equivalent even if their assemblies differ.
    /// </summary>
    internal class SymbolEquivalenceComparer : IEqualityComparer<ISymbol>
    {
        private readonly ImmutableArray<EquivalenceVisitor> _equivalenceVisitors;
        private readonly ImmutableArray<GetHashCodeVisitor> _getHashCodeVisitors;

        public static readonly SymbolEquivalenceComparer Instance = new SymbolEquivalenceComparer(SimpleNameAssemblyComparer.Instance, distinguishRefFromOut: false);
        public static readonly SymbolEquivalenceComparer IgnoreAssembliesInstance = new SymbolEquivalenceComparer(assemblyComparerOpt: null, distinguishRefFromOut: false);

        private readonly IEqualityComparer<IAssemblySymbol> _assemblyComparerOpt;

        public ParameterSymbolEqualityComparer ParameterEquivalenceComparer { get; }
        public SignatureTypeSymbolEquivalenceComparer SignatureTypeEquivalenceComparer { get; }

        internal SymbolEquivalenceComparer(IEqualityComparer<IAssemblySymbol> assemblyComparerOpt, bool distinguishRefFromOut)
        {
            _assemblyComparerOpt = assemblyComparerOpt;

            this.ParameterEquivalenceComparer = new ParameterSymbolEqualityComparer(this, distinguishRefFromOut);
            this.SignatureTypeEquivalenceComparer = new SignatureTypeSymbolEquivalenceComparer(this);

            // There are only so many EquivalenceVisitors and GetHashCodeVisitors we can have.
            // Create them all up front.
            var equivalenceVisitorsBuilder = ImmutableArray.CreateBuilder<EquivalenceVisitor>();
            equivalenceVisitorsBuilder.Add(new EquivalenceVisitor(this, compareMethodTypeParametersByIndex: true, objectAndDynamicCompareEqually: true));
            equivalenceVisitorsBuilder.Add(new EquivalenceVisitor(this, compareMethodTypeParametersByIndex: true, objectAndDynamicCompareEqually: false));
            equivalenceVisitorsBuilder.Add(new EquivalenceVisitor(this, compareMethodTypeParametersByIndex: false, objectAndDynamicCompareEqually: true));
            equivalenceVisitorsBuilder.Add(new EquivalenceVisitor(this, compareMethodTypeParametersByIndex: false, objectAndDynamicCompareEqually: false));
            _equivalenceVisitors = equivalenceVisitorsBuilder.ToImmutable();

            var getHashCodeVisitorsBuilder = ImmutableArray.CreateBuilder<GetHashCodeVisitor>();
            getHashCodeVisitorsBuilder.Add(new GetHashCodeVisitor(this, compareMethodTypeParametersByIndex: true, objectAndDynamicCompareEqually: true));
            getHashCodeVisitorsBuilder.Add(new GetHashCodeVisitor(this, compareMethodTypeParametersByIndex: true, objectAndDynamicCompareEqually: false));
            getHashCodeVisitorsBuilder.Add(new GetHashCodeVisitor(this, compareMethodTypeParametersByIndex: false, objectAndDynamicCompareEqually: true));
            getHashCodeVisitorsBuilder.Add(new GetHashCodeVisitor(this, compareMethodTypeParametersByIndex: false, objectAndDynamicCompareEqually: false));
            _getHashCodeVisitors = getHashCodeVisitorsBuilder.ToImmutable();
        }

        // Very subtle logic here.  When checking if two parameters are the same, we can end up with
        // a tricky infinite loop.  Specifically, consider the case if the parameter refers to a
        // method type parameter.  i.e. "void Goo<T>(IList<T> arg)".  If we compare two method type
        // parameters for equality, then we'll end up asking if their methods are the same.  And that
        // will cause us to check if their parameters are the same.  And then we'll be right back
        // here.  So, instead, when asking if parameters are equal, we pass an appropriate flag so
        // that method type parameters are just compared by index and nothing else.
        private EquivalenceVisitor GetEquivalenceVisitor(
            bool compareMethodTypeParametersByIndex = false, bool objectAndDynamicCompareEqually = false)
        {
            int visitorIndex = GetVisitorIndex(compareMethodTypeParametersByIndex, objectAndDynamicCompareEqually);
            return _equivalenceVisitors[visitorIndex];
        }

        private GetHashCodeVisitor GetGetHashCodeVisitor(
            bool compareMethodTypeParametersByIndex, bool objectAndDynamicCompareEqually)
        {
            int visitorIndex = GetVisitorIndex(compareMethodTypeParametersByIndex, objectAndDynamicCompareEqually);
            return _getHashCodeVisitors[visitorIndex];
        }

        private static int GetVisitorIndex(
            bool compareMethodTypeParametersByIndex, bool objectAndDynamicCompareEqually)
        {
            if (compareMethodTypeParametersByIndex)
            {
                if (objectAndDynamicCompareEqually)
                {
                    return 0;
                }
                else
                {
                    return 1;
                }
            }
            else
            {
                if (objectAndDynamicCompareEqually)
                {
                    return 2;
                }
                else
                {
                    return 3;
                }
            }
        }

        public bool ReturnTypeEquals(IMethodSymbol x, IMethodSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies = null)
        {
            return GetEquivalenceVisitor().ReturnTypesAreEquivalent(x, y, equivalentTypesWithDifferingAssemblies);
        }

        /// <summary>
        /// Compares given symbols <paramref name="x"/> and <paramref name="y"/> for equivalence.
        /// </summary>
        public bool Equals(ISymbol x, ISymbol y)
        {
            return EqualsCore(x, y, null);
        }

        /// <summary>
        /// Compares given symbols <paramref name="x"/> and <paramref name="y"/> for equivalence and populates <paramref name="equivalentTypesWithDifferingAssemblies"/>
        /// with equivalent non-nested named type key-value pairs that are contained in different assemblies.
        /// These equivalent named type key-value pairs represent possibly equivalent forwarded types, but this API doesn't perform any type forwarding equivalence checks. 
        /// </summary>
        /// <remarks>This API is only supported for <see cref="SymbolEquivalenceComparer.IgnoreAssembliesInstance"/>.</remarks>
        public bool Equals(ISymbol x, ISymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
        {
            Debug.Assert(_assemblyComparerOpt == null);
            return EqualsCore(x, y, equivalentTypesWithDifferingAssemblies);
        }

        private bool EqualsCore(ISymbol x, ISymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
        {
            return GetEquivalenceVisitor().AreEquivalent(x, y, equivalentTypesWithDifferingAssemblies);
        }

        public int GetHashCode(ISymbol x)
        {
            return GetGetHashCodeVisitor(compareMethodTypeParametersByIndex: false, objectAndDynamicCompareEqually: false).GetHashCode(x, currentHash: 0);
        }

        private static ISymbol UnwrapAlias(ISymbol symbol)
        {
            if (symbol.Kind == SymbolKind.Alias)
            {
                return ((IAliasSymbol)symbol).Target;
            }
            else
            {
                return symbol;
            }
        }

        private static SymbolKind GetKindAndUnwrapAlias(ref ISymbol symbol)
        {
            var k = symbol.Kind;
            if (k == SymbolKind.Alias)
            {
                symbol = ((IAliasSymbol)symbol).Target;
                k = symbol.Kind;
            }

            return k;
        }

        private static bool IsConstructedFromSelf(INamedTypeSymbol symbol)
        {
            return symbol.Equals(symbol.ConstructedFrom);
        }

        private static bool IsConstructedFromSelf(IMethodSymbol symbol)
        {
            return symbol.Equals(symbol.ConstructedFrom);
        }

        private static bool IsObjectType(ISymbol symbol)
        {
            return symbol.Kind == SymbolKind.NamedType && ((ITypeSymbol)symbol).SpecialType == SpecialType.System_Object;
        }

        private static bool CheckContainingType(IMethodSymbol x)
        {
            if (x.MethodKind == MethodKind.DelegateInvoke &&
                x.ContainingType != null &&
                x.ContainingType.IsAnonymousType)
            {
                return false;
            }

            return true;
        }

        private static IEnumerable<INamedTypeSymbol> Unwrap(INamedTypeSymbol namedType)
        {
            yield return namedType;


            if (namedType is IErrorTypeSymbol errorType)
            {
                foreach (var type in errorType.CandidateSymbols.OfType<INamedTypeSymbol>())
                {
                    yield return type;
                }
            }
        }

        private static bool IsPartialMethodDefinitionPart(IMethodSymbol symbol)
        {
            return symbol.PartialImplementationPart != null;
        }

        private static bool IsPartialMethodImplementationPart(IMethodSymbol symbol)
        {
            return symbol.PartialDefinitionPart != null;
        }

        private static TypeKind GetTypeKind(INamedTypeSymbol x)
        {
            // Treat static classes as modules.
            var k = x.TypeKind;
            return k == TypeKind.Module ? TypeKind.Class : k;
        }

        private class EquivalenceVisitor
        {
            private readonly bool _compareMethodTypeParametersByIndex;
            private readonly bool _objectAndDynamicCompareEqually;
            private readonly SymbolEquivalenceComparer _symbolEquivalenceComparer;

            public EquivalenceVisitor(
                SymbolEquivalenceComparer symbolEquivalenceComparer,
                bool compareMethodTypeParametersByIndex,
                bool objectAndDynamicCompareEqually)
            {
                _symbolEquivalenceComparer = symbolEquivalenceComparer;
                _compareMethodTypeParametersByIndex = compareMethodTypeParametersByIndex;
                _objectAndDynamicCompareEqually = objectAndDynamicCompareEqually;
            }

#if TRACKDEPTH
            private int depth = 0;
#endif
            public bool AreEquivalent(ISymbol x, ISymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
#if TRACKDEPTH
                try
                { 
                this.depth++;
                if (depth > 100)
                {
                    throw new InvalidOperationException("Stack too deep.");
                }
#endif
                if (ReferenceEquals(x, y))
                {
                    return true;
                }

                if (x == null || y == null)
                {
                    return false;
                }

                var xKind = GetKindAndUnwrapAlias(ref x);
                var yKind = GetKindAndUnwrapAlias(ref y);

                // Normally, if they're different types, then they're not the same.
                if (xKind != yKind)
                {
                    // Special case.  If we're comparing signatures then we want to compare 'object'
                    // and 'dynamic' as the same.  However, since they're different types, we don't
                    // want to bail out using the above check.
                    return _objectAndDynamicCompareEqually &&
                           ((yKind == SymbolKind.DynamicType && xKind == SymbolKind.NamedType && ((ITypeSymbol)x).SpecialType == SpecialType.System_Object) ||
                            (xKind == SymbolKind.DynamicType && yKind == SymbolKind.NamedType && ((ITypeSymbol)y).SpecialType == SpecialType.System_Object));
                }

                return AreEquivalentWorker(x, y, xKind, equivalentTypesWithDifferingAssemblies);

#if TRACKDEPTH
            }
            finally
            {
                this.depth--;
            }
#endif
            }

            private bool AreEquivalent(CustomModifier x, CustomModifier y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                return x.IsOptional == y.IsOptional && AreEquivalent(x.Modifier, y.Modifier, equivalentTypesWithDifferingAssemblies);
            }

            internal bool AreEquivalent(ImmutableArray<CustomModifier> x, ImmutableArray<CustomModifier> y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                Debug.Assert(!x.IsDefault && !y.IsDefault);
                if (x.Length != y.Length)
                {
                    return false;
                }

                for (int i = 0; i < x.Length; i++)
                {
                    if (!AreEquivalent(x[i], y[i], equivalentTypesWithDifferingAssemblies))
                    {
                        return false;
                    }
                }

                return true;
            }

            private bool AreEquivalentWorker(ISymbol x, ISymbol y, SymbolKind k, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                Contract.Requires(x.Kind == y.Kind && x.Kind == k);
                switch (k)
                {
                    case SymbolKind.ArrayType:
                        return ArrayTypesAreEquivalent((IArrayTypeSymbol)x, (IArrayTypeSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.Assembly:
                        return AssembliesAreEquivalent((IAssemblySymbol)x, (IAssemblySymbol)y);
                    case SymbolKind.DynamicType:
                        return DynamicTypesAreEquivalent((IDynamicTypeSymbol)x, (IDynamicTypeSymbol)y);
                    case SymbolKind.Event:
                        return EventsAreEquivalent((IEventSymbol)x, (IEventSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.Field:
                        return FieldsAreEquivalent((IFieldSymbol)x, (IFieldSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.Label:
                        return LabelsAreEquivalent((ILabelSymbol)x, (ILabelSymbol)y);
                    case SymbolKind.Local:
                        return LocalsAreEquivalent((ILocalSymbol)x, (ILocalSymbol)y);
                    case SymbolKind.Method:
                        return MethodsAreEquivalent((IMethodSymbol)x, (IMethodSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.NetModule:
                        return ModulesAreEquivalent((IModuleSymbol)x, (IModuleSymbol)y);
                    case SymbolKind.NamedType:
                    case SymbolKind.ErrorType: // ErrorType is handled in NamedTypesAreEquivalent
                        return NamedTypesAreEquivalent((INamedTypeSymbol)x, (INamedTypeSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.Namespace:
                        return NamespacesAreEquivalent((INamespaceSymbol)x, (INamespaceSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.Parameter:
                        return ParametersAreEquivalent((IParameterSymbol)x, (IParameterSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.PointerType:
                        return PointerTypesAreEquivalent((IPointerTypeSymbol)x, (IPointerTypeSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.Property:
                        return PropertiesAreEquivalent((IPropertySymbol)x, (IPropertySymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.RangeVariable:
                        return RangeVariablesAreEquivalent((IRangeVariableSymbol)x, (IRangeVariableSymbol)y);
                    case SymbolKind.TypeParameter:
                        return TypeParametersAreEquivalent((ITypeParameterSymbol)x, (ITypeParameterSymbol)y, equivalentTypesWithDifferingAssemblies);
                    case SymbolKind.Preprocessing:
                        return PreprocessingSymbolsAreEquivalent((IPreprocessingSymbol)x, (IPreprocessingSymbol)y);
                    default:
                        return false;
                }
            }

            private bool ArrayTypesAreEquivalent(IArrayTypeSymbol x, IArrayTypeSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                return
                    x.Rank == y.Rank &&
                    AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) &&
                    AreEquivalent(x.ElementType, y.ElementType, equivalentTypesWithDifferingAssemblies);
            }

            private bool AssembliesAreEquivalent(IAssemblySymbol x, IAssemblySymbol y)
            {
                return _symbolEquivalenceComparer._assemblyComparerOpt?.Equals(x, y) ?? true;
            }

            private bool DynamicTypesAreEquivalent(IDynamicTypeSymbol x, IDynamicTypeSymbol y)
            {
                return true;
            }

            private bool FieldsAreEquivalent(IFieldSymbol x, IFieldSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                return
                    x.Name == y.Name &&
                    AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) &&
                    AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies);
            }

            private bool LabelsAreEquivalent(ILabelSymbol x, ILabelSymbol y)
            {
                return
                    x.Name == y.Name &&
                    HaveSameLocation(x, y);
            }

            private bool LocalsAreEquivalent(ILocalSymbol x, ILocalSymbol y)
            {
                return HaveSameLocation(x, y);
            }

            private bool MethodsAreEquivalent(IMethodSymbol x, IMethodSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                if (!AreCompatibleMethodKinds(x.MethodKind, y.MethodKind))
                {
                    return false;
                }

                if (x.MethodKind == MethodKind.ReducedExtension)
                {
                    var rx = x.ReducedFrom;
                    var ry = y.ReducedFrom;

                    // reduced from symbols are equivalent
                    if (!AreEquivalent(rx, ry, equivalentTypesWithDifferingAssemblies))
                    {
                        return false;
                    }

                    // receiver types are equivalent
                    if (!AreEquivalent(x.ReceiverType, y.ReceiverType, equivalentTypesWithDifferingAssemblies))
                    {
                        return false;
                    }
                }
                else
                {
                    if (x.MethodKind == MethodKind.AnonymousFunction ||
                        x.MethodKind == MethodKind.LocalFunction)
                    {
                        // Treat local and anonymous functions just like we do ILocalSymbols.  
                        // They're only equivalent if they have the same location.
                        return HaveSameLocation(x, y);
                    }

                    if (IsPartialMethodDefinitionPart(x) != IsPartialMethodDefinitionPart(y) ||
                        IsPartialMethodImplementationPart(x) != IsPartialMethodImplementationPart(y) ||
                        x.IsDefinition != y.IsDefinition ||
                        IsConstructedFromSelf(x) != IsConstructedFromSelf(y) ||
                        x.Arity != y.Arity ||
                        x.Parameters.Length != y.Parameters.Length ||
                        x.Name != y.Name)
                    {
                        return false;
                    }

                    var checkContainingType = CheckContainingType(x);
                    if (checkContainingType)
                    {
                        if (!AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies))
                        {
                            return false;
                        }
                    }

                    if (!ParametersAreEquivalent(x.Parameters, y.Parameters, equivalentTypesWithDifferingAssemblies))
                    {
                        return false;
                    }

                    if (!ReturnTypesAreEquivalent(x, y, equivalentTypesWithDifferingAssemblies))
                    {
                        return false;
                    }
                }

                // If it's an unconstructed method, then we don't need to check the type arguments.
                if (IsConstructedFromSelf(x))
                {
                    return true;
                }

                return TypeArgumentsAreEquivalent(x.TypeArguments, y.TypeArguments, equivalentTypesWithDifferingAssemblies);
            }

            private bool AreCompatibleMethodKinds(MethodKind kind1, MethodKind kind2)
            {
                if (kind1 == kind2)
                {
                    return true;
                }

                if ((kind1 == MethodKind.Ordinary && kind2.IsPropertyAccessor()) ||
                    (kind1.IsPropertyAccessor() && kind2 == MethodKind.Ordinary))
                {
                    return true;
                }

                return false;
            }

            private static bool HaveSameLocation(ISymbol x, ISymbol y)
            {
                return x.Locations.Length == 1 && y.Locations.Length == 1 &&
                    x.Locations.First().Equals(y.Locations.First());
            }

            private bool ModulesAreEquivalent(IModuleSymbol x, IModuleSymbol y)
            {
                return AssembliesAreEquivalent(x.ContainingAssembly, y.ContainingAssembly) && x.Name == y.Name;
            }

            private bool NamedTypesAreEquivalent(INamedTypeSymbol x, INamedTypeSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                // PERF: Avoid multiple virtual calls to fetch the TypeKind property
                var xTypeKind = GetTypeKind(x);
                var yTypeKind = GetTypeKind(y);

                if (xTypeKind == TypeKind.Error ||
                    yTypeKind == TypeKind.Error)
                {
                    // Slow path: x or y is an error type. We need to compare
                    // all the candidates in both.
                    return NamedTypesAreEquivalentError(x, y, equivalentTypesWithDifferingAssemblies);
                }

                // Fast path: we can compare the symbols directly,
                // avoiding any allocations associated with the Unwrap()
                // enumerator.
                return xTypeKind == yTypeKind && HandleNamedTypesWorker(x, y, equivalentTypesWithDifferingAssemblies);
            }

            private bool NamedTypesAreEquivalentError(INamedTypeSymbol x, INamedTypeSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                foreach (var type1 in Unwrap(x))
                {
                    var typeKind1 = GetTypeKind(type1);
                    foreach (var type2 in Unwrap(y))
                    {
                        var typeKind2 = GetTypeKind(type2);
                        if (typeKind1 == typeKind2 && HandleNamedTypesWorker(type1, type2, equivalentTypesWithDifferingAssemblies))
                        {
                            return true;
                        }
                    }
                }

                return false;
            }

            /// <summary>
            /// Worker for comparing two named types for equivalence. Note: The two
            /// types must have the same TypeKind.
            /// </summary>
            /// <param name="x">The first type to compare</param>
            /// <param name="y">The second type to compare</param>
            /// <param name="equivalentTypesWithDifferingAssemblies">
            /// Map of equivalent non-nested types to be populated, such that each key-value pair of named types are equivalent but reside in different assemblies.
            /// This map is populated only if we are ignoring assemblies for symbol equivalence comparison, i.e. <see cref="_assemblyComparerOpt"/> is true.
            /// </param>
            /// <returns>True if the two types are equivalent.</returns>
            private bool HandleNamedTypesWorker(INamedTypeSymbol x, INamedTypeSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                Debug.Assert(GetTypeKind(x) == GetTypeKind(y));

                if (x.IsTupleType || y.IsTupleType)
                {
                    if (x.IsTupleType != y.IsTupleType)
                    {
                        return false;
                    }

                    var xElements = x.TupleElements;
                    var yElements = y.TupleElements;

                    if (xElements.Length != yElements.Length)
                    {
                        return false;
                    }

                    for (int i = 0; i < xElements.Length; i++)
                    {
                        if (!AreEquivalent(xElements[i].Type, yElements[i].Type, equivalentTypesWithDifferingAssemblies))
                        {
                            return false;
                        }
                    }

                    return true;
                }

                if (x.IsDefinition != y.IsDefinition ||
                    IsConstructedFromSelf(x) != IsConstructedFromSelf(y) ||
                    x.Arity != y.Arity ||
                    x.Name != y.Name ||
                    x.IsAnonymousType != y.IsAnonymousType ||
                    x.IsUnboundGenericType != y.IsUnboundGenericType)
                {
                    return false;
                }

                if (!AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies))
                {
                    return false;
                }

                // Above check makes sure that the containing assemblies are considered the same by the assembly comparer being used.
                // If they are in fact not the same (have different name) and the caller requested to know about such types add {x, y} 
                // to equivalentTypesWithDifferingAssemblies map.
                if (equivalentTypesWithDifferingAssemblies != null &&
                    x.ContainingType == null &&
                    x.ContainingAssembly != null &&
                    !AssemblyIdentityComparer.SimpleNameComparer.Equals(x.ContainingAssembly.Name, y.ContainingAssembly.Name) &&
                    !equivalentTypesWithDifferingAssemblies.ContainsKey(x))
                {
                    equivalentTypesWithDifferingAssemblies.Add(x, y);
                }

                if (x.IsAnonymousType)
                {
                    return HandleAnonymousTypes(x, y, equivalentTypesWithDifferingAssemblies);
                }

                // They look very similar at this point.  In the case of non constructed types, we're
                // done.  However, if they are constructed, then their type arguments have to match
                // as well.
                return
                    IsConstructedFromSelf(x) ||
                    x.IsUnboundGenericType ||
                    TypeArgumentsAreEquivalent(x.TypeArguments, y.TypeArguments, equivalentTypesWithDifferingAssemblies);
            }

            private bool ParametersAreEquivalent(
                ImmutableArray<IParameterSymbol> xParameters,
                ImmutableArray<IParameterSymbol> yParameters,
                Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies,
                bool compareParameterName = false,
                bool isParameterNameCaseSensitive = false)
            {
                // Note the special parameter comparer we pass in.  We do this so we don't end up
                // infinitely looping between parameters -> type parameters -> methods -> parameters
                var count = xParameters.Length;
                if (yParameters.Length != count)
                {
                    return false;
                }

                for (int i = 0; i < count; i++)
                {
                    if (!_symbolEquivalenceComparer.ParameterEquivalenceComparer.Equals(xParameters[i], yParameters[i], equivalentTypesWithDifferingAssemblies, compareParameterName, isParameterNameCaseSensitive))
                    {
                        return false;
                    }
                }

                return true;
            }

            internal bool ReturnTypesAreEquivalent(IMethodSymbol x, IMethodSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies = null)
            {
                return _symbolEquivalenceComparer.SignatureTypeEquivalenceComparer.Equals(x.ReturnType, y.ReturnType, equivalentTypesWithDifferingAssemblies) &&
                       AreEquivalent(x.ReturnTypeCustomModifiers, y.ReturnTypeCustomModifiers, equivalentTypesWithDifferingAssemblies);
            }

            private bool TypeArgumentsAreEquivalent(ImmutableArray<ITypeSymbol> xTypeArguments, ImmutableArray<ITypeSymbol> yTypeArguments, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                int count = xTypeArguments.Length;
                if (yTypeArguments.Length != count)
                {
                    return false;
                }

                for (int i = 0; i < count; i++)
                {
                    if (!AreEquivalent(xTypeArguments[i], yTypeArguments[i], equivalentTypesWithDifferingAssemblies))
                    {
                        return false;
                    }
                }

                return true;
            }

            private bool HandleAnonymousTypes(INamedTypeSymbol x, INamedTypeSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                if (x.TypeKind == TypeKind.Delegate)
                {
                    return AreEquivalent(x.DelegateInvokeMethod, y.DelegateInvokeMethod, equivalentTypesWithDifferingAssemblies);
                }
                else
                {
                    var xMembers = x.GetValidAnonymousTypeProperties();
                    var yMembers = y.GetValidAnonymousTypeProperties();

                    var xMembersEnumerator = xMembers.GetEnumerator();
                    var yMembersEnumerator = yMembers.GetEnumerator();

                    while (xMembersEnumerator.MoveNext())
                    {
                        if (!yMembersEnumerator.MoveNext())
                        {
                            return false;
                        }

                        var p1 = xMembersEnumerator.Current;
                        var p2 = yMembersEnumerator.Current;

                        if (p1.Name != p2.Name ||
                            p1.IsReadOnly != p2.IsReadOnly ||
                            !AreEquivalent(p1.Type, p2.Type, equivalentTypesWithDifferingAssemblies))
                        {
                            return false;
                        }
                    }

                    return !yMembersEnumerator.MoveNext();
                }
            }

            private bool NamespacesAreEquivalent(INamespaceSymbol x, INamespaceSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                if (x.IsGlobalNamespace != y.IsGlobalNamespace ||
                    x.Name != y.Name)
                {
                    return false;
                }

                if (x.IsGlobalNamespace && _symbolEquivalenceComparer._assemblyComparerOpt == null)
                {
                    // No need to compare the containers of global namespace when assembly identities are ignored.
                    return true;
                }

                return AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies);
            }

            private bool ParametersAreEquivalent(IParameterSymbol x, IParameterSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                return
                    x.IsRefOrOut() == y.IsRefOrOut() &&
                    x.Name == y.Name &&
                    AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) &&
                    AreEquivalent(x.Type, y.Type, equivalentTypesWithDifferingAssemblies) &&
                    AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies);
            }

            private bool PointerTypesAreEquivalent(IPointerTypeSymbol x, IPointerTypeSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                return
                    AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) &&
                    AreEquivalent(x.PointedAtType, y.PointedAtType, equivalentTypesWithDifferingAssemblies);
            }

            private bool PropertiesAreEquivalent(IPropertySymbol x, IPropertySymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                if (x.ContainingType.IsAnonymousType && y.ContainingType.IsAnonymousType)
                {
                    // We can short circuit here and just use the symbols themselves to determine
                    // equality.  This will properly handle things like the VB case where two
                    // anonymous types will be considered the same if they have properties that
                    // differ in casing.
                    if (x.Equals(y))
                    {
                        return true;
                    }
                }

                return
                    x.IsIndexer == y.IsIndexer &&
                    x.MetadataName == y.MetadataName &&
                    x.Parameters.Length == y.Parameters.Length &&
                    ParametersAreEquivalent(x.Parameters, y.Parameters, equivalentTypesWithDifferingAssemblies) &&
                    AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies);
            }

            private bool EventsAreEquivalent(IEventSymbol x, IEventSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                return
                    x.Name == y.Name &&
                    AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies);
            }

            private bool TypeParametersAreEquivalent(ITypeParameterSymbol x, ITypeParameterSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies)
            {
                Contract.Requires(
                    (x.TypeParameterKind == TypeParameterKind.Method && IsConstructedFromSelf(x.DeclaringMethod)) ||
                    (x.TypeParameterKind == TypeParameterKind.Type && IsConstructedFromSelf(x.ContainingType)) ||
                    x.TypeParameterKind == TypeParameterKind.Cref);
                Contract.Requires(
                    (y.TypeParameterKind == TypeParameterKind.Method && IsConstructedFromSelf(y.DeclaringMethod)) ||
                    (y.TypeParameterKind == TypeParameterKind.Type && IsConstructedFromSelf(y.ContainingType)) ||
                    y.TypeParameterKind == TypeParameterKind.Cref);

                if (x.Ordinal != y.Ordinal ||
                    x.TypeParameterKind != y.TypeParameterKind)
                {
                    return false;
                }

                // If this is a method type parameter, and we are in 'non-recurse' mode (because
                // we're comparing method parameters), then we're done at this point.  The types are
                // equal.
                if (x.TypeParameterKind == TypeParameterKind.Method && _compareMethodTypeParametersByIndex)
                {
                    return true;
                }

                if (x.TypeParameterKind == TypeParameterKind.Type && x.ContainingType.IsAnonymousType)
                {
                    // Anonymous type type parameters compare by index as well to prevent
                    // recursion.
                    return true;
                }

                if (x.TypeParameterKind == TypeParameterKind.Cref)
                {
                    return true;
                }

                return AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies);
            }

            private bool RangeVariablesAreEquivalent(IRangeVariableSymbol x, IRangeVariableSymbol y)
            {
                return HaveSameLocation(x, y);
            }

            private bool PreprocessingSymbolsAreEquivalent(IPreprocessingSymbol x, IPreprocessingSymbol y)
            {
                return x.Name == y.Name;
            }
        }

        private class GetHashCodeVisitor
        {
            private readonly SymbolEquivalenceComparer _symbolEquivalenceComparer;
            private readonly bool _compareMethodTypeParametersByIndex;
            private readonly bool _objectAndDynamicCompareEqually;
            private readonly Func<int, IParameterSymbol, int> _parameterAggregator;
            private readonly Func<int, ISymbol, int> _symbolAggregator;

            public GetHashCodeVisitor(
                SymbolEquivalenceComparer symbolEquivalenceComparer,
                bool compareMethodTypeParametersByIndex,
                bool objectAndDynamicCompareEqually)
            {
                _symbolEquivalenceComparer = symbolEquivalenceComparer;
                _compareMethodTypeParametersByIndex = compareMethodTypeParametersByIndex;
                _objectAndDynamicCompareEqually = objectAndDynamicCompareEqually;
                _parameterAggregator = (acc, sym) => Hash.Combine(symbolEquivalenceComparer.ParameterEquivalenceComparer.GetHashCode(sym), acc);
                _symbolAggregator = (acc, sym) => GetHashCode(sym, acc);
            }

            public int GetHashCode(ISymbol x, int currentHash)
            {
                if (x == null)
                {
                    return 0;
                }

                x = UnwrapAlias(x);

                // Special case.  If we're comparing signatures then we want to compare 'object'
                // and 'dynamic' as the same.  However, since they're different types, we don't
                // want to bail out using the above check.

                if (x.Kind == SymbolKind.DynamicType ||
                    (_objectAndDynamicCompareEqually && IsObjectType(x)))
                {
                    return Hash.Combine(typeof(IDynamicTypeSymbol), currentHash);
                }

                return GetHashCodeWorker(x, currentHash);
            }

            private int GetHashCodeWorker(ISymbol x, int currentHash)
            {
                switch (x.Kind)
                {
                    case SymbolKind.ArrayType:
                        return CombineHashCodes((IArrayTypeSymbol)x, currentHash);
                    case SymbolKind.Assembly:
                        return CombineHashCodes((IAssemblySymbol)x, currentHash);
                    case SymbolKind.Event:
                        return CombineHashCodes((IEventSymbol)x, currentHash);
                    case SymbolKind.Field:
                        return CombineHashCodes((IFieldSymbol)x, currentHash);
                    case SymbolKind.Label:
                        return CombineHashCodes((ILabelSymbol)x, currentHash);
                    case SymbolKind.Local:
                        return CombineHashCodes((ILocalSymbol)x, currentHash);
                    case SymbolKind.Method:
                        return CombineHashCodes((IMethodSymbol)x, currentHash);
                    case SymbolKind.NetModule:
                        return CombineHashCodes((IModuleSymbol)x, currentHash);
                    case SymbolKind.NamedType:
                        return CombineHashCodes((INamedTypeSymbol)x, currentHash);
                    case SymbolKind.Namespace:
                        return CombineHashCodes((INamespaceSymbol)x, currentHash);
                    case SymbolKind.Parameter:
                        return CombineHashCodes((IParameterSymbol)x, currentHash);
                    case SymbolKind.PointerType:
                        return CombineHashCodes((IPointerTypeSymbol)x, currentHash);
                    case SymbolKind.Property:
                        return CombineHashCodes((IPropertySymbol)x, currentHash);
                    case SymbolKind.RangeVariable:
                        return CombineHashCodes((IRangeVariableSymbol)x, currentHash);
                    case SymbolKind.TypeParameter:
                        return CombineHashCodes((ITypeParameterSymbol)x, currentHash);
                    case SymbolKind.Preprocessing:
                        return CombineHashCodes((IPreprocessingSymbol)x, currentHash);
                    default:
                        return -1;
                }
            }

            private int CombineHashCodes(IArrayTypeSymbol x, int currentHash)
            {
                return
                    Hash.Combine(x.Rank,
                    GetHashCode(x.ElementType, currentHash));
            }

            private int CombineHashCodes(IAssemblySymbol x, int currentHash)
            {
                return Hash.Combine(_symbolEquivalenceComparer._assemblyComparerOpt?.GetHashCode(x) ?? 0, currentHash);
            }

            private int CombineHashCodes(IFieldSymbol x, int currentHash)
            {
                return
                    Hash.Combine(x.Name,
                    GetHashCode(x.ContainingSymbol, currentHash));
            }

            private int CombineHashCodes(ILabelSymbol x, int currentHash)
            {
                return
                    Hash.Combine(x.Name,
                    Hash.Combine(x.Locations.FirstOrDefault(), currentHash));
            }

            private int CombineHashCodes(ILocalSymbol x, int currentHash)
            {
                return Hash.Combine(x.Locations.FirstOrDefault(), currentHash);
            }

            private static int CombineHashCodes<T>(ImmutableArray<T> array, int currentHash, Func<int, T, int> func)
            {
                return array.Aggregate<int, T>(currentHash, func);
            }

            private int CombineHashCodes(IMethodSymbol x, int currentHash)
            {
                currentHash = Hash.Combine(x.MetadataName, currentHash);
                if (x.MethodKind == MethodKind.AnonymousFunction)
                {
                    return Hash.Combine(x.Locations.FirstOrDefault(), currentHash);
                }

                currentHash =
                    Hash.Combine(IsPartialMethodImplementationPart(x),
                    Hash.Combine(IsPartialMethodDefinitionPart(x),
                    Hash.Combine(x.IsDefinition,
                    Hash.Combine(IsConstructedFromSelf(x),
                    Hash.Combine(x.Arity,
                    Hash.Combine(x.Parameters.Length,
                    Hash.Combine(x.Name, currentHash)))))));

                var checkContainingType = CheckContainingType(x);
                if (checkContainingType)
                {
                    currentHash = GetHashCode(x.ContainingSymbol, currentHash);
                }

                currentHash =
                    CombineHashCodes(x.Parameters, currentHash, _parameterAggregator);

                return IsConstructedFromSelf(x)
                    ? currentHash
                    : CombineHashCodes(x.TypeArguments, currentHash, _symbolAggregator);
            }

            private int CombineHashCodes(IModuleSymbol x, int currentHash)
            {
                return CombineHashCodes(x.ContainingAssembly, Hash.Combine(x.Name, currentHash));
            }

            private int CombineHashCodes(INamedTypeSymbol x, int currentHash)
            {
                currentHash = CombineNamedTypeHashCode(x, currentHash);

                if (x is IErrorTypeSymbol errorType)
                {
                    foreach (var candidate in errorType.CandidateSymbols)
                    {
                        if (candidate is INamedTypeSymbol candidateNamedType)
                        {
                            currentHash = CombineNamedTypeHashCode(candidateNamedType, currentHash);
                        }
                    }
                }

                return currentHash;
            }

            private int CombineNamedTypeHashCode(INamedTypeSymbol x, int currentHash)
            {
                if (x.IsTupleType)
                {
                    return Hash.Combine(currentHash, Hash.CombineValues(x.TupleElements));
                }

                // If we want object and dynamic to be the same, and this is 'object', then return
                // the same hash we do for 'dynamic'.
                currentHash =
                    Hash.Combine(x.IsDefinition,
                    Hash.Combine(IsConstructedFromSelf(x),
                    Hash.Combine(x.Arity,
                    Hash.Combine((int)GetTypeKind(x),
                    Hash.Combine(x.Name,
                    Hash.Combine(x.IsAnonymousType,
                    Hash.Combine(x.IsUnboundGenericType,
                    GetHashCode(x.ContainingSymbol, currentHash))))))));

                if (x.IsAnonymousType)
                {
                    return CombineAnonymousTypeHashCode(x, currentHash);
                }

                return IsConstructedFromSelf(x) || x.IsUnboundGenericType
                    ? currentHash
                    : CombineHashCodes(x.TypeArguments, currentHash, _symbolAggregator);
            }

            private int CombineAnonymousTypeHashCode(INamedTypeSymbol x, int currentHash)
            {
                if (x.TypeKind == TypeKind.Delegate)
                {
                    return GetHashCode(x.DelegateInvokeMethod, currentHash);
                }
                else
                {
                    var xMembers = x.GetValidAnonymousTypeProperties();

                    return xMembers.Aggregate(currentHash, (a, p) =>
                    {
                        return Hash.Combine(p.Name,
                            Hash.Combine(p.IsReadOnly,
                            GetHashCode(p.Type, a)));
                    });
                }
            }

            private int CombineHashCodes(INamespaceSymbol x, int currentHash)
            {
                if (x.IsGlobalNamespace && _symbolEquivalenceComparer._assemblyComparerOpt == null)
                {
                    // Exclude global namespace's container's hash when assemblies can differ.
                    return Hash.Combine(x.Name, currentHash);
                }

                return
                    Hash.Combine(x.IsGlobalNamespace,
                    Hash.Combine(x.Name,
                    GetHashCode(x.ContainingSymbol, currentHash)));
            }

            private int CombineHashCodes(IParameterSymbol x, int currentHash)
            {
                return
                    Hash.Combine(x.IsRefOrOut(),
                    Hash.Combine(x.Name,
                    GetHashCode(x.Type,
                    GetHashCode(x.ContainingSymbol, currentHash))));
            }

            private int CombineHashCodes(IPointerTypeSymbol x, int currentHash)
            {
                return
                    Hash.Combine(typeof(IPointerTypeSymbol).GetHashCode(),
                    GetHashCode(x.PointedAtType, currentHash));
            }

            private int CombineHashCodes(IPropertySymbol x, int currentHash)
            {
                currentHash =
                    Hash.Combine(x.IsIndexer,
                    Hash.Combine(x.Name,
                    Hash.Combine(x.Parameters.Length,
                    GetHashCode(x.ContainingSymbol, currentHash))));

                return CombineHashCodes(x.Parameters, currentHash, _parameterAggregator);
            }

            private int CombineHashCodes(IEventSymbol x, int currentHash)
            {
                return
                    Hash.Combine(x.Name,
                    GetHashCode(x.ContainingSymbol, currentHash));
            }

            public int CombineHashCodes(ITypeParameterSymbol x, int currentHash)
            {
                Contract.Requires(
                    (x.TypeParameterKind == TypeParameterKind.Method && IsConstructedFromSelf(x.DeclaringMethod)) ||
                    (x.TypeParameterKind == TypeParameterKind.Type && IsConstructedFromSelf(x.ContainingType)) ||
                    x.TypeParameterKind == TypeParameterKind.Cref);

                currentHash =
                    Hash.Combine(x.Ordinal,
                    Hash.Combine((int)x.TypeParameterKind, currentHash));

                if (x.TypeParameterKind == TypeParameterKind.Method && _compareMethodTypeParametersByIndex)
                {
                    return currentHash;
                }

                if (x.TypeParameterKind == TypeParameterKind.Type && x.ContainingType.IsAnonymousType)
                {
                    // Anonymous type type parameters compare by index as well to prevent
                    // recursion.
                    return currentHash;
                }

                if (x.TypeParameterKind == TypeParameterKind.Cref)
                {
                    return currentHash;
                }

                return
                    GetHashCode(x.ContainingSymbol, currentHash);
            }

            private int CombineHashCodes(IRangeVariableSymbol x, int currentHash)
            {
                return Hash.Combine(x.Locations.FirstOrDefault(), currentHash);
            }

            private int CombineHashCodes(IPreprocessingSymbol x, int currentHash)
            {
                return Hash.Combine(x.GetHashCode(), currentHash);
            }
        }

        internal class ParameterSymbolEqualityComparer : IEqualityComparer<IParameterSymbol>
        {
            private readonly SymbolEquivalenceComparer _symbolEqualityComparer;
            private readonly bool _distinguishRefFromOut;

            public ParameterSymbolEqualityComparer(
                SymbolEquivalenceComparer symbolEqualityComparer,
                bool distinguishRefFromOut)
            {
                _symbolEqualityComparer = symbolEqualityComparer;
                _distinguishRefFromOut = distinguishRefFromOut;
            }

            public bool Equals(
                IParameterSymbol x,
                IParameterSymbol y,
                Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies,
                bool compareParameterName,
                bool isCaseSensitive)
            {
                if (ReferenceEquals(x, y))
                {
                    return true;
                }

                if (x == null || y == null)
                {
                    return false;
                }

                var nameComparisonCheck = true;
                if (compareParameterName)
                {
                    nameComparisonCheck = isCaseSensitive ?
                        x.Name == y.Name
                        : string.Equals(x.Name, y.Name, StringComparison.OrdinalIgnoreCase);
                }

                // See the comment in the outer type.  If we're comparing two parameters for
                // equality, then we want to consider method type parameters by index only.

                return
                    AreRefKindsEquivalent(x.RefKind, y.RefKind, _distinguishRefFromOut) &&
                    nameComparisonCheck &&
                    _symbolEqualityComparer.GetEquivalenceVisitor().AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) &&
                    _symbolEqualityComparer.SignatureTypeEquivalenceComparer.Equals(x.Type, y.Type, equivalentTypesWithDifferingAssemblies);
            }

            public bool Equals(IParameterSymbol x, IParameterSymbol y)
            {
                return this.Equals(x, y, null, false, false);
            }

            public bool Equals(IParameterSymbol x, IParameterSymbol y, bool compareParameterName, bool isCaseSensitive)
            {
                return this.Equals(x, y, null, compareParameterName, isCaseSensitive);
            }

            public int GetHashCode(IParameterSymbol x)
            {
                if (x == null)
                {
                    return 0;
                }

                return
                    Hash.Combine(x.IsRefOrOut(),
                    _symbolEqualityComparer.SignatureTypeEquivalenceComparer.GetHashCode(x.Type));
            }
        }

        public static bool AreRefKindsEquivalent(RefKind rk1, RefKind rk2, bool distinguishRefFromOut)
        {
            return distinguishRefFromOut
                ? rk1 == rk2
                : (rk1 == RefKind.None) == (rk2 == RefKind.None);
        }

        internal class SignatureTypeSymbolEquivalenceComparer : IEqualityComparer<ITypeSymbol>
        {
            private readonly SymbolEquivalenceComparer _symbolEquivalenceComparer;

            public SignatureTypeSymbolEquivalenceComparer(SymbolEquivalenceComparer symbolEquivalenceComparer) => _symbolEquivalenceComparer = symbolEquivalenceComparer;

            public bool Equals(ITypeSymbol x, ITypeSymbol y) => Equals(x, y, null);

            public bool Equals(ITypeSymbol x, ITypeSymbol y, Dictionary<INamedTypeSymbol, INamedTypeSymbol> equivalentTypesWithDifferingAssemblies) => _symbolEquivalenceComparer.GetEquivalenceVisitor(compareMethodTypeParametersByIndex: true, objectAndDynamicCompareEqually: true).AreEquivalent(x, y, equivalentTypesWithDifferingAssemblies);

            public int GetHashCode(ITypeSymbol x) => _symbolEquivalenceComparer.GetGetHashCodeVisitor(compareMethodTypeParametersByIndex: true, objectAndDynamicCompareEqually: true).GetHashCode(x, currentHash: 0);
        }

        private sealed class SimpleNameAssemblyComparer : IEqualityComparer<IAssemblySymbol>
        {
            public static readonly IEqualityComparer<IAssemblySymbol> Instance = new SimpleNameAssemblyComparer();

            public bool Equals(IAssemblySymbol x, IAssemblySymbol y)
            {
                return AssemblyIdentityComparer.SimpleNameComparer.Equals(x.Name, y.Name);
            }

            public int GetHashCode(IAssemblySymbol obj)
            {
                return AssemblyIdentityComparer.SimpleNameComparer.GetHashCode(obj.Name);
            }
        }
    }

    internal static class Hash
    {
        /// <summary>
        /// This is how VB Anonymous Types combine hash values for fields.
        /// </summary>
        internal static int Combine(int newKey, int currentKey)
        {
            return unchecked((currentKey * (int)0xA5555529) + newKey);
        }

        internal static int Combine(bool newKeyPart, int currentKey)
        {
            return Combine(currentKey, newKeyPart ? 1 : 0);
        }

        /// <summary>
        /// This is how VB Anonymous Types combine hash values for fields.
        /// PERF: Do not use with enum types because that involves multiple
        /// unnecessary boxing operations.  Unfortunately, we can't constrain
        /// T to "non-enum", so we'll use a more restrictive constraint.
        /// </summary>
        internal static int Combine<T>(T newKeyPart, int currentKey) where T : class
        {
            int hash = unchecked(currentKey * (int)0xA5555529);

            if (newKeyPart != null)
            {
                return unchecked(hash + newKeyPart.GetHashCode());
            }

            return hash;
        }

        internal static int CombineValues<T>(IEnumerable<T> values, int maxItemsToHash = int.MaxValue)
        {
            if (values == null)
            {
                return 0;
            }

            var hashCode = 0;
            var count = 0;
            foreach (var value in values)
            {
                if (count++ >= maxItemsToHash)
                {
                    break;
                }

                // Should end up with a constrained virtual call to object.GetHashCode (i.e. avoid boxing where possible).
                if (value != null)
                {
                    hashCode = Combine(value.GetHashCode(), hashCode);
                }
            }

            return hashCode;
        }

        internal static int CombineValues<T>(T[] values, int maxItemsToHash = int.MaxValue)
        {
            if (values == null)
            {
                return 0;
            }

            var maxSize = Math.Min(maxItemsToHash, values.Length);
            var hashCode = 0;

            for (int i = 0; i < maxSize; i++)
            {
                T value = values[i];

                // Should end up with a constrained virtual call to object.GetHashCode (i.e. avoid boxing where possible).
                if (value != null)
                {
                    hashCode = Combine(value.GetHashCode(), hashCode);
                }
            }

            return hashCode;
        }

        internal static int CombineValues<T>(ImmutableArray<T> values, int maxItemsToHash = int.MaxValue)
        {
            if (values.IsDefaultOrEmpty)
            {
                return 0;
            }

            var hashCode = 0;
            var count = 0;
            foreach (var value in values)
            {
                if (count++ >= maxItemsToHash)
                {
                    break;
                }

                // Should end up with a constrained virtual call to object.GetHashCode (i.e. avoid boxing where possible).
                if (value != null)
                {
                    hashCode = Combine(value.GetHashCode(), hashCode);
                }
            }

            return hashCode;
        }

        internal static int CombineValues(IEnumerable<string> values, StringComparer stringComparer, int maxItemsToHash = int.MaxValue)
        {
            if (values == null)
            {
                return 0;
            }

            var hashCode = 0;
            var count = 0;
            foreach (var value in values)
            {
                if (count++ >= maxItemsToHash)
                {
                    break;
                }

                if (value != null)
                {
                    hashCode = Combine(stringComparer.GetHashCode(value), hashCode);
                }
            }

            return hashCode;
        }

        /// <summary>
        /// The offset bias value used in the FNV-1a algorithm
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        internal const int FNV_OFFSET_BIAS = unchecked((int)2166136261);

        /// <summary>
        /// The generative factor used in the FNV-1a algorithm
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        internal const int FNV_PRIME = 16777619;

        /// <summary>
        /// Compute the FNV-1a hash of a sequence of bytes
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="data">The sequence of bytes</param>
        /// <returns>The FNV-1a hash of <paramref name="data"/></returns>
        internal static int GetFNVHashCode(byte[] data)
        {
            int hashCode = Hash.FNV_OFFSET_BIAS;

            for (int i = 0; i < data.Length; i++)
            {
                hashCode = unchecked((hashCode ^ data[i]) * Hash.FNV_PRIME);
            }

            return hashCode;
        }

        /// <summary>
        /// Compute the FNV-1a hash of a sequence of bytes and determines if the byte
        /// sequence is valid ASCII and hence the hash code matches a char sequence
        /// encoding the same text.
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="data">The sequence of bytes that are likely to be ASCII text.</param>
        /// <param name="length">The length of the sequence.</param>
        /// <param name="isAscii">True if the sequence contains only characters in the ASCII range.</param>
        /// <returns>The FNV-1a hash of <paramref name="data"/></returns>
        internal static unsafe int GetFNVHashCode(byte* data, int length, out bool isAscii)
        {
            int hashCode = Hash.FNV_OFFSET_BIAS;

            byte asciiMask = 0;

            for (int i = 0; i < length; i++)
            {
                byte b = data[i];
                asciiMask |= b;
                hashCode = unchecked((hashCode ^ b) * Hash.FNV_PRIME);
            }

            isAscii = (asciiMask & 0x80) == 0;
            return hashCode;
        }

        /// <summary>
        /// Compute the FNV-1a hash of a sequence of bytes
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="data">The sequence of bytes</param>
        /// <returns>The FNV-1a hash of <paramref name="data"/></returns>
        internal static int GetFNVHashCode(ImmutableArray<byte> data)
        {
            int hashCode = Hash.FNV_OFFSET_BIAS;

            for (int i = 0; i < data.Length; i++)
            {
                hashCode = unchecked((hashCode ^ data[i]) * Hash.FNV_PRIME);
            }

            return hashCode;
        }

        /// <summary>
        /// Compute the hashcode of a sub-string using FNV-1a
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// Note: FNV-1a was developed and tuned for 8-bit sequences. We're using it here
        /// for 16-bit Unicode chars on the understanding that the majority of chars will
        /// fit into 8-bits and, therefore, the algorithm will retain its desirable traits
        /// for generating hash codes.
        /// </summary>
        /// <param name="text">The input string</param>
        /// <param name="start">The start index of the first character to hash</param>
        /// <param name="length">The number of characters, beginning with <paramref name="start"/> to hash</param>
        /// <returns>The FNV-1a hash code of the substring beginning at <paramref name="start"/> and ending after <paramref name="length"/> characters.</returns>
        internal static int GetFNVHashCode(string text, int start, int length)
        {
            int hashCode = Hash.FNV_OFFSET_BIAS;
            int end = start + length;

            for (int i = start; i < end; i++)
            {
                hashCode = unchecked((hashCode ^ text[i]) * Hash.FNV_PRIME);
            }

            return hashCode;
        }

        internal static int GetCaseInsensitiveFNVHashCode(string text)
        {
            return GetCaseInsensitiveFNVHashCode(text, 0, text.Length);
        }

        internal static int GetCaseInsensitiveFNVHashCode(string text, int start, int length)
        {
            int hashCode = Hash.FNV_OFFSET_BIAS;
            int end = start + length;

            for (int i = start; i < end; i++)
            {
                hashCode = unchecked((hashCode ^ CaseInsensitiveComparison.ToLower(text[i])) * Hash.FNV_PRIME);
            }

            return hashCode;
        }

        /// <summary>
        /// Compute the hashcode of a sub-string using FNV-1a
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="text">The input string</param>
        /// <param name="start">The start index of the first character to hash</param>
        /// <returns>The FNV-1a hash code of the substring beginning at <paramref name="start"/> and ending at the end of the string.</returns>
        internal static int GetFNVHashCode(string text, int start)
        {
            return GetFNVHashCode(text, start, length: text.Length - start);
        }

        /// <summary>
        /// Compute the hashcode of a string using FNV-1a
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="text">The input string</param>
        /// <returns>The FNV-1a hash code of <paramref name="text"/></returns>
        internal static int GetFNVHashCode(string text)
        {
            return CombineFNVHash(Hash.FNV_OFFSET_BIAS, text);
        }

        /// <summary>
        /// Compute the hashcode of a string using FNV-1a
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="text">The input string</param>
        /// <returns>The FNV-1a hash code of <paramref name="text"/></returns>
        internal static int GetFNVHashCode(System.Text.StringBuilder text)
        {
            int hashCode = Hash.FNV_OFFSET_BIAS;
            int end = text.Length;

            for (int i = 0; i < end; i++)
            {
                hashCode = unchecked((hashCode ^ text[i]) * Hash.FNV_PRIME);
            }

            return hashCode;
        }

        /// <summary>
        /// Compute the hashcode of a sub string using FNV-1a
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="text">The input string as a char array</param>
        /// <param name="start">The start index of the first character to hash</param>
        /// <param name="length">The number of characters, beginning with <paramref name="start"/> to hash</param>
        /// <returns>The FNV-1a hash code of the substring beginning at <paramref name="start"/> and ending after <paramref name="length"/> characters.</returns>
        internal static int GetFNVHashCode(char[] text, int start, int length)
        {
            int hashCode = Hash.FNV_OFFSET_BIAS;
            int end = start + length;

            for (int i = start; i < end; i++)
            {
                hashCode = unchecked((hashCode ^ text[i]) * Hash.FNV_PRIME);
            }

            return hashCode;
        }

        /// <summary>
        /// Compute the hashcode of a single character using the FNV-1a algorithm
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// Note: In general, this isn't any more useful than "char.GetHashCode". However,
        /// it may be needed if you need to generate the same hash code as a string or
        /// substring with just a single character.
        /// </summary>
        /// <param name="ch">The character to hash</param>
        /// <returns>The FNV-1a hash code of the character.</returns>
        internal static int GetFNVHashCode(char ch)
        {
            return Hash.CombineFNVHash(Hash.FNV_OFFSET_BIAS, ch);
        }

        /// <summary>
        /// Combine a string with an existing FNV-1a hash code
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="hashCode">The accumulated hash code</param>
        /// <param name="text">The string to combine</param>
        /// <returns>The result of combining <paramref name="hashCode"/> with <paramref name="text"/> using the FNV-1a algorithm</returns>
        internal static int CombineFNVHash(int hashCode, string text)
        {
            foreach (char ch in text)
            {
                hashCode = unchecked((hashCode ^ ch) * Hash.FNV_PRIME);
            }

            return hashCode;
        }

        /// <summary>
        /// Combine a char with an existing FNV-1a hash code
        /// See http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        /// </summary>
        /// <param name="hashCode">The accumulated hash code</param>
        /// <param name="ch">The new character to combine</param>
        /// <returns>The result of combining <paramref name="hashCode"/> with <paramref name="ch"/> using the FNV-1a algorithm</returns>
        internal static int CombineFNVHash(int hashCode, char ch)
        {
            return unchecked((hashCode ^ ch) * Hash.FNV_PRIME);
        }
    }
}
