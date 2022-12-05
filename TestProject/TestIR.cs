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

    [Test]
    [TestCase("Store @a @a")]
    [TestCase("Label %a")]
    [TestCase("Jump %a")]
    [TestCase("Compare @c $1 @a")]
    public void TestParse(string irCode)
    {
        var op = Op.Parse(irCode);
        Console.WriteLine(op);
    }

    [Test]
    [TestCase(@"Store @a @a
    Label %a
    Jump %a
    Compare @c $1 @a")]
    [TestCase(@"Add @a @b @c
Sub @b @a @d
Add @c @b @c
Sub @d @a @d")]
    public void TestIrParse(string irCode)
    {
        var ir_block = IRBlock.Parse(irCode);
        Console.WriteLine(ir_block);
        if (ir_block.IsBasicBlock())
        {
            var basic_block_dag = ir_block.BasicBlockDAG();
            
            basic_block_dag.ToGraphviz(algorithm =>
            {
                algorithm.FormatVertex += (sender, args)
                    =>
                {
                    if (args.Vertex is IRBlock.Leaf leaf) 
                    {
                        args.VertexFormat.Style = GraphvizVertexStyle.Rounded;
                        args.VertexFormat.Shape = GraphvizVertexShape.Circle;
                        args.VertexFormat.Label = leaf.Var;
                    }

                    if (args.Vertex is IRBlock.Node node)
                    {
                        args.VertexFormat.Style = GraphvizVertexStyle.Bold;
                        args.VertexFormat.Shape = GraphvizVertexShape.Box;
                        args.VertexFormat.Label = node.OpCode.ToString() + '\n' + string.Join(", ", node.Variables);
                    }
                };
            }).ExportDotToSvg(nameof(Example), "svg");
        }
    }
    
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