using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;


namespace Nemesis.CodeAnalysis
{
    public class ExpressionBodyRewriter : CSharpSyntaxRewriter
    {
        private readonly TransformationDirection _direction;
        private readonly ExpressionBodySettings _settings;

        public ExpressionBodyRewriter(TransformationDirection direction, ExpressionBodySettings settings, bool visitIntoStructuredTrivia = false) : base(visitIntoStructuredTrivia)
        {
            _direction = direction;
            _settings = settings;
        }

        private static TDeclaration Convert<TDeclaration>(ExpressionBodyHelper<TDeclaration> helper, TDeclaration declaration, ExpressionBodyPreference preference, TransformationDirection direction) where TDeclaration : SyntaxNode
        {
            if (direction == TransformationDirection.StatementToExpressionBody)
            {
                if (helper.CanConvertToExpressionBody(declaration, preference))
                {
                    TDeclaration newDeclaration= helper.ConvertToExpressionBody(declaration);
                    newDeclaration = (TDeclaration)Formatter.Format(newDeclaration, new AdhocWorkspace());
                    newDeclaration = newDeclaration.WithLeadingTrivia(declaration.GetLeadingTrivia());
                    return newDeclaration;
                }
            }
            else
            {
                if (helper.CanConvertToBlockBody(declaration, preference))
                    return helper.ConvertToBlockBody(declaration);
            }
            return declaration;
        }

        public override SyntaxNode VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            node = Convert(ExpressionBodyForConstructorsHelper.Instance, node, _settings.ExpressionBodiedConstructors, _direction);
            return base.VisitConstructorDeclaration(node);
        }

        public override SyntaxNode VisitConversionOperatorDeclaration(ConversionOperatorDeclarationSyntax node)
        {
            node = Convert(ExpressionBodyForConversionOperatorsHelper.Instance, node, _settings.ExpressionBodiedConversionOperators, _direction);
            return base.VisitConversionOperatorDeclaration(node);
        }

        public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            node = Convert(ExpressionBodyForMethodsHelper.Instance, node, _settings.ExpressionBodiedMethods, _direction);
            return base.VisitMethodDeclaration(node);
        }

        public override SyntaxNode VisitOperatorDeclaration(OperatorDeclarationSyntax node)
        {
            node = Convert(ExpressionBodyForOperatorsHelper.Instance, node, _settings.ExpressionBodiedOperators, _direction);
            return base.VisitOperatorDeclaration(node);
        }

        public override SyntaxNode VisitIndexerDeclaration(IndexerDeclarationSyntax node)
        {
            node = Convert(ExpressionBodyForIndexersHelper.Instance, node, _settings.ExpressionBodiedIndexers, _direction);
            return base.VisitIndexerDeclaration(node);
        }

        public override SyntaxNode VisitAccessorDeclaration(AccessorDeclarationSyntax node)
        {
            node = Convert(ExpressionBodyForAccessorsHelper.Instance, node, _settings.ExpressionBodiedAccessors, _direction);
            return base.VisitAccessorDeclaration(node);
        }

        public override SyntaxNode VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            var @base = (PropertyDeclarationSyntax)base.VisitPropertyDeclaration(node);

            node = Convert(ExpressionBodyForPropertiesHelper.Instance, @base, _settings.ExpressionBodiedProperties, _direction);

            return node;
        }
    }

    public enum TransformationDirection : byte { StatementToExpressionBody, ExpressionBodyToStatement, }

    public readonly struct ExpressionBodySettings
    {
        public ExpressionBodyPreference ExpressionBodiedAccessors { get; }
        public ExpressionBodyPreference ExpressionBodiedConstructors { get; }
        public ExpressionBodyPreference ExpressionBodiedConversionOperators { get; }
        public ExpressionBodyPreference ExpressionBodiedIndexers { get; }
        public ExpressionBodyPreference ExpressionBodiedMethods { get; }
        public ExpressionBodyPreference ExpressionBodiedOperators { get; }
        public ExpressionBodyPreference ExpressionBodiedProperties { get; }

        public ExpressionBodySettings(ExpressionBodyPreference expressionBodiedAccessors, ExpressionBodyPreference expressionBodiedConstructors, ExpressionBodyPreference expressionBodiedConversionOperators, ExpressionBodyPreference expressionBodiedIndexers, ExpressionBodyPreference expressionBodiedMethods, ExpressionBodyPreference expressionBodiedOperators, ExpressionBodyPreference expressionBodiedProperties)
        {
            ExpressionBodiedAccessors = expressionBodiedAccessors;
            ExpressionBodiedConstructors = expressionBodiedConstructors;
            ExpressionBodiedConversionOperators = expressionBodiedConversionOperators;
            ExpressionBodiedIndexers = expressionBodiedIndexers;
            ExpressionBodiedMethods = expressionBodiedMethods;
            ExpressionBodiedOperators = expressionBodiedOperators;
            ExpressionBodiedProperties = expressionBodiedProperties;
        }

        public static readonly ExpressionBodySettings WhenPossible = new ExpressionBodySettings(ExpressionBodyPreference.WhenPossible, ExpressionBodyPreference.WhenPossible, ExpressionBodyPreference.WhenPossible, ExpressionBodyPreference.WhenPossible, ExpressionBodyPreference.WhenPossible, ExpressionBodyPreference.WhenPossible, ExpressionBodyPreference.WhenPossible);
    }
}
