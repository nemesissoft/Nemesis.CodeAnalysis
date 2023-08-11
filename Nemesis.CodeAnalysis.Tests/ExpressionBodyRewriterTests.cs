namespace Nemesis.CodeAnalysis.Tests;

[TestFixture]
public class ExpressionBodyRewriterTests
{
    [Test]
    public void ExpressionBodyRewriter_ToExpressionBody()
    {
        const string CODE = @"using System;
class SimpleClass
{
    private readonly int _integer;
    private float _floating;

    event EventHandler PreDrawEvent;

    event EventHandler OnDraw
    {
        add { PreDrawEvent += value; }
        remove { PreDrawEvent -= value; }
    }

    public SimpleClass(int integer)
    {
        _integer = integer;
    }

    public SimpleClass(float floating)
    {
        _floating = floating;
    }

    public SimpleClass(int integer, float floating)
    {
        _integer = integer;
        _floating = floating;
        PreDrawEvent?.Invoke(this, EventArgs.Empty);
    }

    public int GetNumber()
    {
        return 15 + 6;
    }

    public int GetNumber2() { return 15 + 6; }

    public void DumpToConsole()
    {
        Console.WriteLine(_integer);
    }

    public void DumpToConsole(string text)
    {
        Console.WriteLine(text);
    }

    public void Throw()
    {
        throw new NotSupportedException(123.ToString());
    }

    public float ConditionalMax()
    {
        return _integer > _floating ? _integer : _floating;
    }

    public static SimpleClass operator +(SimpleClass sc1, SimpleClass sc2)
    {
        return new SimpleClass(sc1._integer + sc2._integer, sc1._floating + sc2._floating);
    }

    public static SimpleClass operator -(SimpleClass sc1, SimpleClass sc2)
    {
        return new SimpleClass(sc1._integer - sc2._integer, sc1._floating - sc2._floating);
    }

    public static explicit operator int(SimpleClass sc)
    {
        return sc._integer;
    }

    public static implicit operator float(SimpleClass sc)
    {
        return sc._floating;
    }

    public int Integer { get { return _integer; } }

    public float Float { get { return _floating; } set { _floating = value; } }

    public int this[int i]
    {
        get { return i > _integer ? i : _integer; }            
    }

    public int this[int i, float f]
    {
        get { return i > _integer ? i : _integer; }
        set { _floating = value + i + f; }
    }
}";
        const string EXPECTED_CODE = @"class SimpleClass
{
    private readonly int _integer;
    private float _floating;

    event EventHandler PreDrawEvent;

    event EventHandler OnDraw
    {
        add => PreDrawEvent += value;
        remove => PreDrawEvent -= value;
    }

    public SimpleClass(int integer) => _integer = integer;

    public SimpleClass(float floating) => _floating = floating;

    public SimpleClass(int integer, float floating)
    {
        _integer = integer;
        _floating = floating;
        PreDrawEvent?.Invoke(this, EventArgs.Empty);
    }

    public int GetNumber() => 15 + 6;

    public int GetNumber2() => 15 + 6;

    public void DumpToConsole() => Console.WriteLine(_integer);

    public void DumpToConsole(string text) => Console.WriteLine(text);

    public void Throw() => throw new NotSupportedException(123.ToString());

    public float ConditionalMax() => _integer > _floating ? _integer : _floating;

    public static SimpleClass operator +(SimpleClass sc1, SimpleClass sc2) => new SimpleClass(sc1._integer + sc2._integer, sc1._floating + sc2._floating);

    public static SimpleClass operator -(SimpleClass sc1, SimpleClass sc2) => new SimpleClass(sc1._integer - sc2._integer, sc1._floating - sc2._floating);

    public static explicit operator int(SimpleClass sc) => sc._integer;

    public static implicit operator float(SimpleClass sc) => sc._floating;

    public int Integer => _integer;

    public float Float { get => _floating;set => _floating = value;}

    public int this[int i] => i > _integer ? i : _integer;

    public int this[int i, float f]
    {
        get => i > _integer ? i : _integer;
        set => _floating = value + i + f;
    }
}";

        var tree = CSharpSyntaxTree.ParseText(CODE);

        var root = tree.GetRoot();

        var @class = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var actual = new ExpressionBodyRewriter(TransformationDirection.StatementToExpressionBody, ExpressionBodySettings.WhenPossible)
            .Visit(@class).ToFullString();

        Assert.That(actual, Is.EqualTo(EXPECTED_CODE));
    }
}
