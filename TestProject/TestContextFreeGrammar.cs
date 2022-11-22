using QuikGraph;
using QuikGraph.Graphviz;
using SyntaxAnalyzer;

namespace TestProject;

public class TestContextFreeGrammar
{
    [Test]
    public void Generate()
    {
        var calculate_name = "Calculate";
        var cfg = new ContextFreeGrammar(calculate_name, new()
        {
            new CfgRule(
                "Expr", new()
                {
                    new() { CfgNode.NonTerminal("Expr"), "+", CfgNode.NonTerminal("Term") },
                    new() { CfgNode.NonTerminal("Term") }
                }
            ),
            new CfgRule(
                "Term", new()
                {
                    new() { CfgNode.NonTerminal("Term"), "*", CfgNode.NonTerminal("Factor") },
                    new() { CfgNode.NonTerminal("Factor") }
                }
            ),
            new CfgRule(
                "Factor", new()
                {
                    new() { "(", CfgNode.NonTerminal("Expr"), ")" },
                    new() { "a" }
                }
            )
        });

        var graph = cfg.Rules.ExportConnections();
        var dot = graph.ToGraphviz(algorithm =>
        {
            algorithm.FormatVertex += (sender, args)
                => args.VertexFormat.Label = args.Vertex.ToString();
        });

        Utilities.ExportDotToSvg(dot, calculate_name, "svg");
    }

    [Test]
    public void TestAST()
    {
        var ast = new AST("+",
            new("*",
                new("a"), new("2")),
            new AST("*",
                new("*", new("a"), new("2")),
                new("b")));
        var graph = ast.ToGraph();
        var dot = graph.ToGraphviz(algorithm =>
        {
            algorithm.FormatVertex += (sender, args)
                => args.VertexFormat.Label = args.Vertex.ToString();
        });

        Utilities.ExportDotToSvg(dot, "test", "svg");
    }
}