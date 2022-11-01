using LuaAnalyzer.Infomation;

namespace LuaAnalyzer.Syntax;

public record Block(SymbolTable SymbolTable, List<Statement> Statements) : AST;