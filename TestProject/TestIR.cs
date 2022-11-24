using QuikGraph.Graphviz;
using QuikGraph.Graphviz.Dot;
using SyntaxAnalyzer;

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

        var control_flow_graph = block.ToControlFlowGraph();
        control_flow_graph.ToGraphviz(algorithm =>
        {
            algorithm.FormatVertex += (sender, args)
                =>
            {
                args.VertexFormat.Style = GraphvizVertexStyle.Rounded;
                args.VertexFormat.Label = string.Join('\n', args.Vertex.Ops.Select(o => o.ToString()));
            };
            algorithm.FormatEdge += (sender, args) => args.EdgeFormat.Label = new GraphvizEdgeLabel() { Value = args.Edge.Tag };
        }).ExportDotToSvg(nameof(Example), "svg");
    }
}