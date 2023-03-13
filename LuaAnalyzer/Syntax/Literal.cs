namespace LuaAnalyzer.Syntax;

using static LuaType;

public record Literal(LuaType Type): AST
{
    
}

public record BoolLiteral(bool Value) : Literal(Bool);
public record IntLiteral(int Value) : Literal(Int);
public record StringLiteral(string Value) : Literal(String);
public record NilLiteral() : Literal(Nil);

public enum LuaType
{
    Any,
    Int,
    String,
    Bool,
    Nil,
    Function,
    Table,
}

public static class LuaTypeExtension
{
}

public record Value();
public record StringValue(string Str): Value;
public record NumberValue(double Number): Value;
