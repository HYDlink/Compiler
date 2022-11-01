using LuaAnalyzer.Infomation;
using LuaAnalyzer.Syntax;
using sly.lexer;
using sly.parser.generator;

namespace LuaAnalyzer;

using Tok = Token<LexToken>;

public class SyntaxParser
{
    public Stack<Block> ProcessingBlocks = new();

    [Production("Block: Statement+")]
    public Block Block(List<AST> statements)
        => new Block(new SymbolTable(), statements.Cast<Statement>().ToList());

    [Production("Statement: IDENTIFIER ASSIGN Expression")]
    public Statement Statement(Tok idToken, Tok assign, Expression expression)
    {
        return new Statement(idToken.Value, expression);
    }

    // [Production("BinOp: PLUS")]

    [Production("Literal: STRING")]
    public Literal String(Tok str)
    {
        return new StringLiteral(str.StringWithoutQuotes);
    }

    [Production("BoolLiteral: TRUE")]
    public BoolLiteral BoolLiteralTrue(Tok t)
        => new BoolLiteral(true);

    [Production("BoolLiteral: FALSE")]
    public BoolLiteral BoolLiteralFalse(Tok t)
        => new BoolLiteral(false);

    [Production("Literal: BoolLiteral")]
    public Literal Bool(BoolLiteral boolLiteral)
        => boolLiteral;

    [Production("Literal: INT")]
    public Literal Int(Tok str)
    {
        return new IntLiteral(str.IntValue);
    }

    [Production("Expression: Literal")]
    public Expression Expression(Literal literal)
        => new Expression(literal);
}
