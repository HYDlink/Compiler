namespace LuaAnalyzer.Syntax;

public interface IBlockOwner
{
    public Block Block { get; set; }
}
public record If(Expression Condition) : Statement, IBlockOwner
{
    public Block Block { get; set; }
}