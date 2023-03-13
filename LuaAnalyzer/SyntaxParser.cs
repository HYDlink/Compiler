using System.Diagnostics;
using LuaAnalyzer.Infomation;
using LuaAnalyzer.Syntax;
using sly.lexer;
using sly.parser.generator;
using sly.parser.parser;

namespace LuaAnalyzer;

using Tok = Token<LexToken>;
using static LexToken;

public class SyntaxParser
{
    #region SymbolTable Management

    public SymbolTableManager SymbolTableManager = new();
    
    public static void ConstructSymbolTable(Block block, SymbolTableManager symbolTableManager)
    {
        symbolTableManager.InitializeScope();
        var current_table = symbolTableManager.CurrentTable;

        // construct for current layer assignment
        Debug.Assert(current_table != null);
        foreach (var assign_statement in block.Statements.OfType<AssignStatement>())
        {
            var type = assign_statement.Expression.Type;
            var id = assign_statement.Id;
            var layer = current_table.Name;
            if (symbolTableManager.Get(id) is ({ } table, { } symbol_item))
            {
                layer = table.Name;
                if (symbol_item.Type != type)
                {
                    Console.WriteLine($"assign wrong for {id}");
                }

                Console.WriteLine($"assign multiple times for {id}");
            }
            else
            {
                symbolTableManager.Set(id, type);
            }

            assign_statement.SymbolLayerName = layer;
        }

        // construct for child blocks
        var blocks_in_owner = block.Statements.OfType<IBlockOwner>().Select(ib => ib.Block);
        var direct_child_blocks = block.Statements.OfType<Block>();
        var child_blocks = direct_child_blocks.Concat(blocks_in_owner);
        foreach (var child_block in child_blocks)
        {
            ConstructSymbolTable(child_block, symbolTableManager);
        }
        
        symbolTableManager.PopScope();
    }

    #endregion

    #region Statement

    // TODO how to start block, before statement being processing
    //  how to input Epsilon preserved code
    [Production("Block: Statement+")]
    public Block Block(List<AST> statements)
    {
        var actual_statements = statements.Cast<Statement>().ToList();
        return new Block(actual_statements);
    }

    [Production($"If: IF[d] Expression THEN[d] {nameof(Block)} END[d]")]
    public If If(LiteralExpression condition, Block block)
    {
        return new If(condition) { Block = block };
    }

    [Production($"AssignStatement: {nameof(LOCAL)}? {nameof(IDENTIFIER)} {nameof(ASSIGN)}[d] Expression")]
    public Statement AssignStatement(ValueOption<Tok> isLocal, Tok idToken, Expression literalExpression)
    {
        return new AssignStatement(isLocal.IsSome, idToken.Value, literalExpression);
    }

    [Production($"{nameof(FuncDefineStatement)}: {nameof(LOCAL)}? {nameof(FUNCTION)}[d] {nameof(ASSIGN)}[d] {nameof(Block)}")]
    public FuncDefineStatement FuncDefineStatement(ValueOption<Tok> isLocal, Tok idToken, Block block)
    {
        return new FuncDefineStatement(isLocal.IsSome, idToken.Value, block);
    }

    [Production($"{nameof(FuncCallStatement)} : {nameof(FuncCall)}")]
    public FuncCallStatement FuncCallStatement(FuncCall funcCall)
        => new FuncCallStatement(funcCall);

    [Production($"Statement: [{nameof(If)} | {nameof(AssignStatement)} | {nameof(FuncCallStatement)}]")]
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

    [Production($"{nameof(Expression)}: [{nameof(LiteralExpression)} | {nameof(FuncCall)} | {nameof(IdExpression)}]")]
    public Expression Expression(AST ast) => (Expression)ast;

    [Production($"{nameof(IdExpression)}: {nameof(IDENTIFIER)}")]
    public IdExpression IdExpression(Tok idTok) => new IdExpression(idTok.Value);

    [Production("LiteralExpression: Literal")]
    public LiteralExpression LiteralExpression(Literal literal)
        => new LiteralExpression(literal);

    [Production($"{nameof(FuncCall)}: {nameof(IDENTIFIER)} {nameof(LPAREN)}[d] {nameof(Expression)}* {nameof(RPAREN)}[d]")]
    public FuncCall FuncCall(Token<LexToken> idTok, List<AST> args)
        => new FuncCall(idTok.Value, args.Cast<Expression>().ToList());
}