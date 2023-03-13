using LuaAnalyzer.Syntax;

namespace LuaAnalyzer;

public class ExecuteMachine
{
    public ExecuteMachine()
    {
    }

    public Literal EvaluateFunc(FuncCall funcCall)
    {
        if (funcCall.Id == "print")
        {
            var expr = funcCall.Arguments.First();
            var value = Evaluate(expr);
            Console.WriteLine(value.ToString());
        }

        return new Literal(LuaType.Nil);
    }

    public Literal Evaluate(LiteralExpression expression)
        => expression.Value;

    public Literal Evaluate(Expression expression)
        => expression switch
        {
            LiteralExpression literalExpression => Evaluate(literalExpression),
            FuncCall funcCall => EvaluateFunc(funcCall),
            IdExpression {Id: {} id} => RuntimeValues.TryGetValue(id, out var value) ? value : new NilLiteral(),
            _ => throw new NotImplementedException(),
        };

    public void Execute(Statement statement)
    {
        switch (statement)
        {
            case FuncCallStatement { FuncCall: { } funcCall }:
                Evaluate(funcCall)
                    ;
                break;
            case If { Condition: { } condition, Block: { } block }:
            {
                var literal = Evaluate(condition);
                if (literal is not { Type: LuaType.Nil } or BoolLiteral { Value: false })
                {
                    Execute(block);
                }
            }
                break;
            case AssignStatement { Id: { } id, Expression: { } expression }:
                RuntimeValues[id] = Evaluate(expression);
                break;
            default:
                throw new Exception();
        }
    }

    private Dictionary<string, Literal> RuntimeValues = new();

    public void Execute(Block block)
    {
        block.Statements.ForEach(Execute);
    }
}