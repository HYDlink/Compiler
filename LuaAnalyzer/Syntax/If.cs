namespace LuaAnalyzer.Syntax;

public interface IBlockOwner
{
    public Block Block { get; set; }
}
public record If(LiteralExpression Condition) : Statement, IBlockOwner
{
    public Block Block { get; set; }
}