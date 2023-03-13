using LuaAnalyzer.Infomation;

namespace LuaAnalyzer.Syntax;

public record Statement() : AST;

public record FuncCallStatement(FuncCall FuncCall) : Statement;

public record AssignStatement(bool IsLocal, string Id, Expression Expression) : Statement
{
    public string SymbolLayerName { get; set; } = "";
}

public record FuncDefineStatement(bool IsLocal, string Id, Block Block) : Statement;