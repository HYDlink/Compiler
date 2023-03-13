namespace LuaAnalyzer.Syntax;

public record Expression : AST
{
    public virtual LuaType Type => LuaType.Any;
}
public record LiteralExpression : Expression
{
    public override LuaType Type => Value.Type;
    public Literal Value { get; set; }

    public LiteralExpression(Literal literal) => Value = literal;
}

public record IdExpression(string Id) : Expression;

public record FuncCall(string Id, List<Expression> Arguments) : Expression;