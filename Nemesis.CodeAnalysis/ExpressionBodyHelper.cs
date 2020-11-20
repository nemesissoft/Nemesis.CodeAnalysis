using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SF = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Nemesis.CodeAnalysis
{
    public enum ExpressionBodyPreference : byte { Never = 0, WhenPossible = 1, WhenOnSingleLine = 2 }

    internal abstract class ExpressionBodyHelper<TDeclaration> where TDeclaration : SyntaxNode
    {
        #region Public members

        public BlockSyntax GetBody(SyntaxNode declaration) => GetBodyCore((TDeclaration)declaration);

        public ArrowExpressionClauseSyntax GetExpressionBody(SyntaxNode declaration) => GetExpressionBodyCore((TDeclaration)declaration);

        public bool CanConvertToExpressionBody(TDeclaration declaration, ExpressionBodyPreference preference)
            => preference != ExpressionBodyPreference.Never && GetExpressionBodyCore(declaration) == null &&
               TryConvertToExpressionBody(declaration, preference, out var _, out var _);

        public bool CanConvertToBlockBody(TDeclaration declaration, ExpressionBodyPreference preference)
        {
            var expressionBodyOpt = GetExpressionBodyCore(declaration);
            bool canConvert = expressionBodyOpt?.TryConvertToBlock(SF.Token(SyntaxKind.SemicolonToken), false, out var _) == true;
            if (!canConvert) return false;

            var languageVersion = ((CSharpParseOptions)declaration.SyntaxTree.Options).LanguageVersion;
            if (expressionBodyOpt.Expression.IsKind(SyntaxKind.ThrowExpression) && languageVersion < LanguageVersion.CSharp7) //throw expression are not valid for C# version < 7
                return true;

            var isAccessorOrConstructor = declaration is AccessorDeclarationSyntax || declaration is ConstructorDeclarationSyntax;
            if (isAccessorOrConstructor && languageVersion < LanguageVersion.CSharp7) //expression bodies for accessors/constructors are not valid for C# version < 7
                return true;
            else if (languageVersion < LanguageVersion.CSharp6) //expression bodies are not valid for C# version < 6
                return true;

            return preference == ExpressionBodyPreference.Never;
        }

        public TDeclaration ConvertToExpressionBody(TDeclaration declaration)
        {
            TryConvertToExpressionBody(declaration, ExpressionBodyPreference.WhenPossible,
                out var expressionBody, out var semicolonToken);

            var trailingTrivia = semicolonToken.TrailingTrivia.Where(t => t.Kind() != SyntaxKind.EndOfLineTrivia).Concat(declaration.GetTrailingTrivia());
            semicolonToken = semicolonToken.WithTrailingTrivia(trailingTrivia);

            return WithSemicolonToken(WithExpressionBody(WithBody(declaration, null),expressionBody),semicolonToken);
        }

        public TDeclaration ConvertToBlockBody(TDeclaration declaration) => WithSemicolonToken(WithExpressionBody(WithGenerateBody(declaration), expressionBody: null), default);

        #endregion
        
        protected virtual bool TryConvertToExpressionBody(TDeclaration declaration, ExpressionBodyPreference conversionPreference, out ArrowExpressionClauseSyntax expressionWhenOnSingleLine, out SyntaxToken semicolonWhenOnSingleLine)
            => TryConvertToExpressionBodyWorker(declaration, conversionPreference, out expressionWhenOnSingleLine, out semicolonWhenOnSingleLine);
        
        protected bool TryConvertToExpressionBodyWorker(SyntaxNode declaration, ExpressionBodyPreference conversionPreference, out ArrowExpressionClauseSyntax expressionWhenOnSingleLine, out SyntaxToken semicolonWhenOnSingleLine)
        {
            var languageVersion = ((CSharpParseOptions)declaration.SyntaxTree.Options).LanguageVersion;

            var body = GetBody(declaration);

            return body.TryConvertToExpressionBody(declaration.Kind(), languageVersion, conversionPreference, out expressionWhenOnSingleLine, out semicolonWhenOnSingleLine);
        }
        
        protected abstract BlockSyntax GetBodyCore(TDeclaration declaration);
        protected abstract ArrowExpressionClauseSyntax GetExpressionBodyCore(TDeclaration declaration);
        protected abstract SyntaxToken GetSemicolonToken(TDeclaration declaration);
        protected abstract bool NeedsReturnStatementForExpression(TDeclaration declaration);

        protected abstract TDeclaration WithSemicolonToken(TDeclaration declaration, SyntaxToken token);
        protected abstract TDeclaration WithExpressionBody(TDeclaration declaration, ArrowExpressionClauseSyntax expressionBody);
        protected abstract TDeclaration WithBody(TDeclaration declaration, BlockSyntax body);

        protected virtual TDeclaration WithGenerateBody(TDeclaration declaration)
        {
            var expressionBody = GetExpressionBodyCore(declaration);//var semicolonToken = GetSemicolonToken(declaration);

            if (expressionBody.TryConvertToBlock(GetSemicolonToken(declaration), NeedsReturnStatementForExpression(declaration), out var block))
                return WithBody(declaration, block);

            return declaration;
        }
    }

    internal abstract class ExpressionBodyBaseMethodDeclarationHelper<TDeclaration> : ExpressionBodyHelper<TDeclaration> where TDeclaration : BaseMethodDeclarationSyntax
    {
        protected sealed override BlockSyntax GetBodyCore(TDeclaration declaration) => declaration.Body;

        protected sealed override ArrowExpressionClauseSyntax GetExpressionBodyCore(TDeclaration declaration) => declaration.ExpressionBody;

        protected sealed override SyntaxToken GetSemicolonToken(TDeclaration declaration) => declaration.SemicolonToken;
    }

    //SyntaxKind.ConstructorDeclaration
    internal sealed class ExpressionBodyForConstructorsHelper : ExpressionBodyBaseMethodDeclarationHelper<ConstructorDeclarationSyntax>
    {
        public static readonly ExpressionBodyForConstructorsHelper Instance = new ExpressionBodyForConstructorsHelper();

        protected override ConstructorDeclarationSyntax WithSemicolonToken(ConstructorDeclarationSyntax declaration, SyntaxToken token) => declaration.WithSemicolonToken(token);

        protected override ConstructorDeclarationSyntax WithExpressionBody(ConstructorDeclarationSyntax declaration, ArrowExpressionClauseSyntax expressionBody) => declaration.WithExpressionBody(expressionBody);

        protected override ConstructorDeclarationSyntax WithBody(ConstructorDeclarationSyntax declaration, BlockSyntax body) => declaration.WithBody(body);

        protected override bool NeedsReturnStatementForExpression(ConstructorDeclarationSyntax declaration) => false;
    }

    //SyntaxKind.ConversionOperatorDeclaration
    internal sealed class ExpressionBodyForConversionOperatorsHelper : ExpressionBodyBaseMethodDeclarationHelper<ConversionOperatorDeclarationSyntax>
    {
        public static readonly ExpressionBodyForConversionOperatorsHelper Instance = new ExpressionBodyForConversionOperatorsHelper();

        protected override ConversionOperatorDeclarationSyntax WithSemicolonToken(ConversionOperatorDeclarationSyntax declaration, SyntaxToken token) => declaration.WithSemicolonToken(token);

        protected override ConversionOperatorDeclarationSyntax WithExpressionBody(ConversionOperatorDeclarationSyntax declaration, ArrowExpressionClauseSyntax expressionBody) => declaration.WithExpressionBody(expressionBody);

        protected override ConversionOperatorDeclarationSyntax WithBody(ConversionOperatorDeclarationSyntax declaration, BlockSyntax body) => declaration.WithBody(body);

        protected override bool NeedsReturnStatementForExpression(ConversionOperatorDeclarationSyntax declaration) => true;
    }

    //SyntaxKind.MethodDeclaration
    internal sealed class ExpressionBodyForMethodsHelper : ExpressionBodyBaseMethodDeclarationHelper<MethodDeclarationSyntax>
    {
        public static readonly ExpressionBodyForMethodsHelper Instance = new ExpressionBodyForMethodsHelper();

        protected override MethodDeclarationSyntax WithSemicolonToken(MethodDeclarationSyntax declaration, SyntaxToken token) => declaration.WithSemicolonToken(token);

        protected override MethodDeclarationSyntax WithExpressionBody(MethodDeclarationSyntax declaration, ArrowExpressionClauseSyntax expressionBody) => declaration.WithExpressionBody(expressionBody);

        protected override MethodDeclarationSyntax WithBody(MethodDeclarationSyntax declaration, BlockSyntax body) => declaration.WithBody(body);

        protected override bool NeedsReturnStatementForExpression(MethodDeclarationSyntax declaration) => !declaration.ReturnType.IsVoid();
    }

    //SyntaxKind.OperatorDeclaration
    internal sealed class ExpressionBodyForOperatorsHelper : ExpressionBodyBaseMethodDeclarationHelper<OperatorDeclarationSyntax>
    {
        public static readonly ExpressionBodyForOperatorsHelper Instance = new ExpressionBodyForOperatorsHelper();

        protected override OperatorDeclarationSyntax WithSemicolonToken(OperatorDeclarationSyntax declaration, SyntaxToken token) => declaration.WithSemicolonToken(token);

        protected override OperatorDeclarationSyntax WithExpressionBody(OperatorDeclarationSyntax declaration, ArrowExpressionClauseSyntax expressionBody) => declaration.WithExpressionBody(expressionBody);

        protected override OperatorDeclarationSyntax WithBody(OperatorDeclarationSyntax declaration, BlockSyntax body) => declaration.WithBody(body);

        protected override bool NeedsReturnStatementForExpression(OperatorDeclarationSyntax declaration) => true;
    }

    //SyntaxKind.GetAccessorDeclaration, SyntaxKind.SetAccessorDeclaration
    internal sealed class ExpressionBodyForAccessorsHelper : ExpressionBodyHelper<AccessorDeclarationSyntax>
    {
        public static readonly ExpressionBodyForAccessorsHelper Instance = new ExpressionBodyForAccessorsHelper();

        protected override BlockSyntax GetBodyCore(AccessorDeclarationSyntax declaration) => declaration.Body;

        protected override ArrowExpressionClauseSyntax GetExpressionBodyCore(AccessorDeclarationSyntax declaration) => declaration.ExpressionBody;

        protected override SyntaxToken GetSemicolonToken(AccessorDeclarationSyntax declaration) => declaration.SemicolonToken;

        protected override AccessorDeclarationSyntax WithSemicolonToken(AccessorDeclarationSyntax declaration, SyntaxToken token) => declaration.WithSemicolonToken(token);

        protected override AccessorDeclarationSyntax WithExpressionBody(AccessorDeclarationSyntax declaration, ArrowExpressionClauseSyntax expressionBody) => declaration.WithExpressionBody(expressionBody);

        protected override AccessorDeclarationSyntax WithBody(AccessorDeclarationSyntax declaration, BlockSyntax body) => declaration.WithBody(body);

        protected override bool NeedsReturnStatementForExpression(AccessorDeclarationSyntax declaration) => declaration.IsKind(SyntaxKind.GetAccessorDeclaration);
    }

    internal abstract class ExpressionBodyForBaseProperty<TDeclaration> : ExpressionBodyHelper<TDeclaration> where TDeclaration : BasePropertyDeclarationSyntax
    {
        protected TDeclaration WithAccessorList(TDeclaration declaration)
        {
            var expressionBody = GetExpressionBodyCore(declaration);
            var semicolonToken = GetSemicolonToken(declaration);

            // When converting an expression-bodied property to a block body, always attempt to create an accessor with a block body (even if the user likes expression bodied accessors.
            // While this technically doesn't match their preferences, it fits with the far more likely scenario that the user wants to convert this property into a full property so that they
            // can flesh out the body contents.  If we keep around an expression bodied accessor they'll just have to convert that to a block as well and that means two steps to take instead of one.

            expressionBody.TryConvertToBlock(GetSemicolonToken(declaration), NeedsReturnStatementForExpression(declaration), out var block);

            var accessor = SF.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration);
            accessor = block != null
                ? accessor.WithBody(block)
                : accessor.WithExpressionBody(expressionBody)
                    .WithSemicolonToken(semicolonToken);

            return WithAccessorList(declaration, SF.AccessorList(SF.SingletonList(accessor)));
        }

        protected abstract TDeclaration WithAccessorList(TDeclaration declaration, AccessorListSyntax accessorListSyntax);

        protected bool TryConvertToExpressionBodyForBaseProperty(BasePropertyDeclarationSyntax declaration, ExpressionBodyPreference conversionPreference, out ArrowExpressionClauseSyntax arrowExpression, out SyntaxToken semicolonToken)
        {
            if (TryConvertToExpressionBodyWorker(declaration, conversionPreference, out arrowExpression, out semicolonToken))
                return true;

            var getAccessor = GetSingleGetAccessor(declaration.AccessorList);
            if (getAccessor?.ExpressionBody?.Expression is { } expression &&
                (conversionPreference == ExpressionBodyPreference.WhenPossible
                 ||
                 conversionPreference == ExpressionBodyPreference.WhenOnSingleLine && SourceUtils.IsOnSingleLine(expression, fullSpan: false)
                )
            )
            {
                arrowExpression = SF.ArrowExpressionClause(expression);
                semicolonToken = getAccessor.SemicolonToken;
                return true;
            }

            return false;
        }

        protected static AccessorDeclarationSyntax GetSingleGetAccessor(AccessorListSyntax accessorList)
        {
            if (accessorList != null &&
                accessorList.Accessors.Count == 1 &&
                accessorList.Accessors[0].AttributeLists.Count == 0 &&
                accessorList.Accessors[0].IsKind(SyntaxKind.GetAccessorDeclaration))
            {
                return accessorList.Accessors[0];
            }

            return null;
        }

        protected static BlockSyntax GetBodyFromSingleGetAccessor(AccessorListSyntax accessorList) => GetSingleGetAccessor(accessorList)?.Body;
    }

    //SyntaxKind.IndexerDeclaration
    internal sealed class ExpressionBodyForIndexersHelper : ExpressionBodyForBaseProperty<IndexerDeclarationSyntax>
    {
        public static readonly ExpressionBodyForIndexersHelper Instance = new ExpressionBodyForIndexersHelper();

        protected override BlockSyntax GetBodyCore(IndexerDeclarationSyntax declaration) => GetBodyFromSingleGetAccessor(declaration.AccessorList);

        protected override ArrowExpressionClauseSyntax GetExpressionBodyCore(IndexerDeclarationSyntax declaration) => declaration.ExpressionBody;

        protected override SyntaxToken GetSemicolonToken(IndexerDeclarationSyntax declaration) => declaration.SemicolonToken;

        protected override IndexerDeclarationSyntax WithSemicolonToken(IndexerDeclarationSyntax declaration, SyntaxToken token) => declaration.WithSemicolonToken(token);

        protected override IndexerDeclarationSyntax WithExpressionBody(IndexerDeclarationSyntax declaration, ArrowExpressionClauseSyntax expressionBody) => declaration.WithExpressionBody(expressionBody);

        protected override IndexerDeclarationSyntax WithAccessorList(IndexerDeclarationSyntax declaration, AccessorListSyntax accessorList) => declaration.WithAccessorList(accessorList);

        protected override IndexerDeclarationSyntax WithBody(IndexerDeclarationSyntax declaration, BlockSyntax body)
        {
            return body == null ? declaration.WithAccessorList(null) : throw new InvalidOperationException();
        }

        protected override IndexerDeclarationSyntax WithGenerateBody(IndexerDeclarationSyntax declaration) => WithAccessorList(declaration);

        protected override bool NeedsReturnStatementForExpression(IndexerDeclarationSyntax declaration) => true;

        protected override bool TryConvertToExpressionBody(IndexerDeclarationSyntax declaration, ExpressionBodyPreference conversionPreference, out ArrowExpressionClauseSyntax arrowExpression, out SyntaxToken semicolonToken)
        {
            return TryConvertToExpressionBodyForBaseProperty(declaration, conversionPreference, out arrowExpression, out semicolonToken);
        }
    }

    //SyntaxKind.PropertyDeclaration
    internal sealed class ExpressionBodyForPropertiesHelper : ExpressionBodyForBaseProperty<PropertyDeclarationSyntax>
    {
        public static readonly ExpressionBodyForPropertiesHelper Instance = new ExpressionBodyForPropertiesHelper();

        protected override BlockSyntax GetBodyCore(PropertyDeclarationSyntax declaration) => GetBodyFromSingleGetAccessor(declaration.AccessorList);

        protected override ArrowExpressionClauseSyntax GetExpressionBodyCore(PropertyDeclarationSyntax declaration) => declaration.ExpressionBody;

        protected override SyntaxToken GetSemicolonToken(PropertyDeclarationSyntax declaration) => declaration.SemicolonToken;

        protected override PropertyDeclarationSyntax WithSemicolonToken(PropertyDeclarationSyntax declaration, SyntaxToken token) => declaration.WithSemicolonToken(token);

        protected override PropertyDeclarationSyntax WithExpressionBody(PropertyDeclarationSyntax declaration, ArrowExpressionClauseSyntax expressionBody) => declaration.WithExpressionBody(expressionBody);

        protected override PropertyDeclarationSyntax WithAccessorList(PropertyDeclarationSyntax declaration, AccessorListSyntax accessorListSyntax) => declaration.WithAccessorList(accessorListSyntax);

        protected override PropertyDeclarationSyntax WithBody(PropertyDeclarationSyntax declaration, BlockSyntax body)
        {
            return body == null ? declaration.WithAccessorList(null) : throw new InvalidOperationException();
        }

        protected override PropertyDeclarationSyntax WithGenerateBody(PropertyDeclarationSyntax declaration) => WithAccessorList(declaration);

        protected override bool NeedsReturnStatementForExpression(PropertyDeclarationSyntax declaration) => true;

        protected override bool TryConvertToExpressionBody(PropertyDeclarationSyntax declaration, ExpressionBodyPreference conversionPreference, out ArrowExpressionClauseSyntax arrowExpression, out SyntaxToken semicolonToken)
        {
            return TryConvertToExpressionBodyForBaseProperty(declaration, conversionPreference, out arrowExpression, out semicolonToken);
        }
    }
}