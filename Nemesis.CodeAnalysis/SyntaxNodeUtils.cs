namespace Nemesis.CodeAnalysis;

public static class SyntaxNodeUtils
{
    public static TParentType ParentOfType<TParentType>(this SyntaxNode current) where TParentType : SyntaxNode
    {
        while (current.Parent != null)
        {
            if (current.Parent is TParentType parentType) return parentType;
            current = current.Parent;
        }
        return null;
    }

    public static TypeDeclarationSyntax GetParentType(this SyntaxNode node) => node.Ancestors().OfType<TypeDeclarationSyntax>().First();

    public static bool IsNameOfExpression(this InvocationExpressionSyntax invocation, SemanticModel model)
    {
        return invocation.Expression is IdentifierNameSyntax ident &&
               ident.Identifier.ValueText == "nameof" &&
               !invocation.Ancestors().OfType<MemberAccessExpressionSyntax>().Any() &&
               ModelExtensions.GetSymbolInfo(model, invocation) is { } si && si.Symbol == null && si.CandidateSymbols.IsEmpty;
    }

    /// <summary>Determines whether the specified TypeSyntax is actually 'var'. </summary>
    public static bool IsTypeInferred(this TypeSyntax typeSyntax, SemanticModel semanticModel)
    {
        if (!typeSyntax.IsVar) return false;

        if (semanticModel.GetAliasInfo(typeSyntax) != null)
            return false;

        var type = ModelExtensions.GetTypeInfo(semanticModel, typeSyntax).Type;
        return type != null && type.Name != "var";
    }

    public static bool IsVoid(this TypeSyntax typeSyntax) => typeSyntax.IsKind(SyntaxKind.PredefinedType) && ((PredefinedTypeSyntax)typeSyntax).Keyword.IsKind(SyntaxKind.VoidKeyword);

    public static bool IsPartial(this TypeSyntax typeSyntax) => typeSyntax is IdentifierNameSyntax ident && ident.Identifier.IsKind(SyntaxKind.PartialKeyword);

    public static CompilationUnitSyntax GetCompilationUnit(this SyntaxNode node) =>
        node as CompilationUnitSyntax ?? node.SyntaxTree.GetCompilationUnitRoot();

    #region Tree manipulations

    public static TType RemoveFieldVariables<TType>(TType type, IReadOnlyCollection<string> fieldVariablesToRemove, bool createTypeCommentsOutOfRemovedVariables = false) where TType : TypeDeclarationSyntax
    {
        const SyntaxRemoveOptions REMOVE_OPTIONS = SyntaxRemoveOptions.KeepDirectives | SyntaxRemoveOptions.KeepUnbalancedDirectives;

        IReadOnlyCollection<VariableDeclaratorSyntax> GetAllFieldVariables() => type.ChildNodes().OfType<FieldDeclarationSyntax>().SelectMany(field => field.Declaration.Variables).ToList();

        var allVariables = GetAllFieldVariables();

        var missing = fieldVariablesToRemove.Where(name => allVariables.Select(v => v.Identifier.ValueText).All(varName => name != varName)).ToList();
        if (missing.Any())
            throw new ArgumentException(
                $@"Missing variables to be removed at type {type.Identifier.ValueText}: {string.Join(", ", missing.Select(mv => $"'{mv}'"))}", nameof(fieldVariablesToRemove));

        var toRemove = allVariables.Where(var => fieldVariablesToRemove.Contains(var.Identifier.ValueText)).ToList();

        if (toRemove.Any())
            type = type.RemoveNodes(toRemove, REMOVE_OPTIONS);

        var phantomFields = type.ChildNodes().OfType<FieldDeclarationSyntax>().Where(f => f.Declaration.Variables.Count == 0).ToList();
        if (phantomFields.Any())
            type = type.RemoveNodes(phantomFields, REMOVE_OPTIONS);

        var afterRemoval = GetAllFieldVariables();
        if (allVariables.Count - toRemove.Count != afterRemoval.Count)
            throw new InvalidOperationException($"Removal operation failed. Before removal #{allVariables.Count}, to remove #{toRemove.Count}, after #{afterRemoval.Count}, remaining fields {string.Join(", ", afterRemoval.Select(v => v.Identifier.ValueText))}");

        if (createTypeCommentsOutOfRemovedVariables && toRemove.Any())
        {
            string commentText = string.Join(Environment.NewLine,
                toRemove.Select(variable => TriviaUtils.RemoveExteriorNonCommentTrivia(variable).ToFullString().Replace("*/", "*❤/"))
                    );
            var comment = SyntaxFactory.Comment($"/*{commentText}*/");

            type = type.WithLeadingTrivia(type.GetLeadingTrivia().Add(comment).Add(SyntaxFactory.CarriageReturnLineFeed));
        }

        return type;
    }

    #endregion
}
