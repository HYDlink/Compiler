using LuaAnalyzer.IR;

namespace TestProject;

public class TestIR
{
    public IRBlock Example = new IRBlock(new()
    {
        Op.Store(1, "a"),
        Op.Store(2, "b"),
        new Op(OpCode.Add, new VariableOperand("a"), new VariableOperand("a"), new VariableOperand("a")),
        new Op(OpCode.Compare, new VariableOperand("a"), new VariableOperand("b"), new VariableOperand("cond")),
        Op.Branch("cond", "T", "F"),
        Op.Label("T"),
        Op.Store(1, "flow"),
        Op.Jump("E"),
        Op.Label("F"),
        Op.Store(-1, "flow"),
        Op.Label("E"),
    });

    [Test]
    public void Test() => TestBlock(Example);
    
    public void TestBlock(IRBlock block)
    {
        var ir_context = block.Execute();
        foreach (var (key, value) in ir_context.Register)
        {
            Console.WriteLine($"{key}: {value}");
        }
    }
}