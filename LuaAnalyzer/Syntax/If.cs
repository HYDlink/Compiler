namespace LuaAnalyzer.Syntax;

public record If(Expression Condition, Block Block) : Statement
{
    
}