using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Nemesis.CodeAnalysis
{
    public class NameSyntaxIterator : IEnumerable<NameSyntax>
    {
        private readonly NameSyntax _name;

        public NameSyntaxIterator(NameSyntax name) => _name = name ?? throw new ArgumentNullException(nameof(name));

        public IEnumerator<NameSyntax> GetEnumerator()
        {
            var nodes = new LinkedList<NameSyntax>();

            var currentNode = _name;
            while (true)
            {
                if (currentNode.Kind() == SyntaxKind.QualifiedName && currentNode is QualifiedNameSyntax qualifiedName)
                {
                    nodes.AddFirst(qualifiedName.Right);
                    currentNode = qualifiedName.Left;
                }
                else
                {
                    nodes.AddFirst(currentNode);
                    break;
                }
            }

            return nodes.GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
