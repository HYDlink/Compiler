using LuaAnalyzer.Infomation;
using LuaAnalyzer.Syntax;
using sly.lexer;
using sly.parser.generator;

namespace LuaAnalyzer;

using Tok = Token<LexToken>;

public class SyntaxParser
{
    public Stack<Block> ProcessingBlocks = new();
    public List<string> StringTables = new();

    #region Statement

    // TODO how to start block, before statement being processing
    //  how to input Epsilon preserved code
    [Production("Block: BlockStart[d] Statement+")]
    public Block Block(List<AST> statements)
    {
        return new Block(new SymbolTable(), statements.Cast<Statement>().ToList());
    }

    [Production("BlockStart:")]
    public AST BlockStart()
    {
        var block = new Block(new SymbolTable(), new());
        ProcessingBlocks.Push(block);
        return block;
    }

    [Production("If: IF[d] Expression THEN[d] Block END[d]")]
    public If If(Expression condition, Block block)
    {
        return new If(condition, block);
    }

    [Production("AssignStatement: IDENTIFIER ASSIGN Expression")]
    public Statement AssignStatement(Tok idToken, Tok assign, Expression expression)
    {
        return new AssignStatement(idToken.Value, expression);
    }

    [Production("Statement: [If | AssignStatement]")]
     public Statement Statement(Statement statement)
        => statement;

    

    #endregion
    // [Production("BinOp: PLUS")]

    #region Literal

    [Production("StringLiteral: STRING")]
    public StringLiteral String(Tok str)
    {
        return new StringLiteral(str.StringWithoutQuotes);
    }

    [Production("BoolLiteral: [TRUE | FALSE]")]
    public BoolLiteral BoolLiteralTrue(Tok t)
    {
        var value = t.TokenID switch
        {
            LexToken.TRUE => true,
            LexToken.FALSE => false,
            _ => throw new ArgumentOutOfRangeException()
        };
        return new BoolLiteral(value);
    }
    
    [Production("IntLiteral: INT")]
    public IntLiteral Int(Tok str)
    {
        return new IntLiteral(str.IntValue);
    }

    [Production("Literal: [BoolLiteral | IntLiteral | StringLiteral]")]
    public Literal Literal(Literal literal)
        => literal;

    #endregion


    [Production("Expression: Literal")]
    public Expression Expression(Literal literal)
        => new Expression(literal);
    
}
