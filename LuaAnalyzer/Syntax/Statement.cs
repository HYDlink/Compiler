namespace LuaAnalyzer.Syntax;

public record Statement() : AST;

public record AssignStatement(string Id, Expression Expression) : Statement;