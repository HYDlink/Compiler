namespace LuaAnalyzer.Syntax;

public record Expression : AST
{
    public string Type => Value.Type;
    public Literal Value { get; set; }

    public Expression(Literal literal) => Value = literal;
}