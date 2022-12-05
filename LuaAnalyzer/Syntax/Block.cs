using LuaAnalyzer.Infomation;

namespace LuaAnalyzer.Syntax;

public record Block(List<Statement> Statements) : AST
{
    public SymbolTable SymbolTable { get; set; }
}