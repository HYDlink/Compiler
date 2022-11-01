namespace LuaAnalyzer.Syntax;

public record Literal(string Type): AST
{
    
}

public record BoolLiteral(bool Value) : Literal("Bool");
public record IntLiteral(int Value) : Literal("Int");
public record StringLiteral(string Value) : Literal("String");