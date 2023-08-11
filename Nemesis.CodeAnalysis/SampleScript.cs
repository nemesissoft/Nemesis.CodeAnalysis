using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

namespace Nemesis.CodeAnalysis;

public class SimpleScript<TReturn>
{
    public string Code { get; }
    public Script<TReturn> Script { get; }
    public IReadOnlyList<string> AllIdentifiers { get; }
    public IReadOnlyList<string> DeclaredVariables { get; }
    public IReadOnlyList<string> UndeclaredVariables { get; }

    private SimpleScript(string code, Script<TReturn> script, IReadOnlyList<string> allIdentifiers,
        IReadOnlyList<string> declaredVariables, IReadOnlyList<string> undeclaredVariables)
    {
        Code = code;
        Script = script;
        AllIdentifiers = allIdentifiers;
        DeclaredVariables = declaredVariables;
        UndeclaredVariables = undeclaredVariables;
    }

    public static SimpleScript<TReturn> FromCode(string code)
    {
        Script<TReturn> script = CSharpScript.Create<TReturn>(code);
        var compilation = script.GetCompilation();
        var symModels = compilation.SyntaxTrees.Select(synTree => compilation.GetSemanticModel(synTree, true)).Single();

        var allIdentifiers = symModels.SyntaxTree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Select(v => v.Identifier.ValueText).ToList();

        var declaredVariables = symModels.SyntaxTree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>()
            .Select(v => v.Identifier.ValueText).ToList();

        return new SimpleScript<TReturn>(code, script,
            allIdentifiers.AsReadOnly(),
            declaredVariables.AsReadOnly(),
            allIdentifiers.Except(declaredVariables).ToList().AsReadOnly()
        );
    }
}
