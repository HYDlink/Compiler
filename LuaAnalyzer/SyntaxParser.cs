using System.Diagnostics;
using LuaAnalyzer.Infomation;
using LuaAnalyzer.Syntax;
using sly.lexer;
using sly.parser.generator;

namespace LuaAnalyzer;

using Tok = Token<LexToken>;

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

    [Production("If: IF[d] Expression THEN[d] Block END[d]")]
    public If If(Expression condition, Block block)
    {
        return new If(condition) { Block = block };
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