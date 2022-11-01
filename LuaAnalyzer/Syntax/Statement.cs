namespace LuaAnalyzer.Syntax;

public record Statement(string Id, Expression Expression) : AST;