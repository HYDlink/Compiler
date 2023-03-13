using AsciiTableFormatter;
using QuikGraph;
using QuikGraph.Graphviz;
using QuikGraph.Graphviz.Dot;
using SyntaxAnalyzer;
using TextTableFormatter;
using static SyntaxAnalyzer.CfgNode;
using static SyntaxAnalyzer.CfgProduction;

namespace TestProject;

public class TestContextFreeGrammar
{
    // 000111
    private ContextFreeGrammar ZeroOne = new ContextFreeGrammar("ZeroOne", new()
        { new CfgRule("S", new() { new() { "0", NonTerminal("S"), "1" } }) });

    // +*aaa
    private ContextFreeGrammar AddOrMul = new ContextFreeGrammar(nameof(AddOrMul), new()
    {
        new CfgRule("S", new()
        {
            new() { "+", NonTerminal("S"), NonTerminal("S") },
            new() { "*", NonTerminal("S"), NonTerminal("S") },
            new() { "a" }
        })
    });

    // (()())
    private ContextFreeGrammar Brace = new ContextFreeGrammar(nameof(Brace), new()
    {
        new CfgRule("S", new()
        {
            new() { NonTerminal("S"), "(", NonTerminal("S"), ")", NonTerminal("S") },
            Epsilon
        })
    });


    static string calculate_name = "Calculate";

    ContextFreeGrammar CalculateCfg = new ContextFreeGrammar(calculate_name, new()
    {
        new CfgRule(
            "Expr", new()
            {
                new() { NonTerminal("Expr"), "+", NonTerminal("Term") },
                new() { NonTerminal("Term") }
            }
        ),
        new CfgRule(
            "Term", new()
            {
                new() { NonTerminal("Term"), "*", NonTerminal("Factor") },
                new() { NonTerminal("Factor") }
            }
        ),
        new CfgRule(
            "Factor", new()
            {
                new() { "(", NonTerminal("Expr"), ")" },
                new() { "a" }
            }
        )
    });


    /// <summary>
    /// E -> T E'
    /// E' -> + T E' | e
    /// T -> F T'
    /// T' -> * F T' | e
    /// F -> ( E ) | id
    /// </summary>
    private ContextFreeGrammar RightRecursiveCfg = new("RightCalculate", new()
    {
        new("E", new()
        {
            new() { NonTerminal("T"), NonTerminal("E'") }
        }),
        new("E'", new()
        {
            new() { "+", NonTerminal("T"), NonTerminal("E'") },
            Epsilon
        }),
        new("T", new()
        {
            new() { NonTerminal("F"), NonTerminal("T'") },
        }),
        new("T'", new()
        {
            new() { "*", NonTerminal("F"), NonTerminal("T'") },
            Epsilon
        }),
        new("F", new()
        {
            new() { "(", NonTerminal("E"), ")" },
            new() { "a" }
        }),
    }) { RootRule = "E" };

    [Test]
    public void Generate()
    {
        ExportGraphviz(CalculateCfg);
        ExportGraphviz(RightRecursiveCfg);
    }

    private static void ExportGraphviz(ContextFreeGrammar cfg)
    {
        var graph = cfg.Rules.ExportConnections();
        var dot = graph.ToGraphviz(algorithm =>
        {
            algorithm.FormatVertex += (sender, args)
                => args.VertexFormat.Label = args.Vertex.ToString();
        });

        dot.ExportDotToSvg(cfg.Name, "svg");
    }

    [Test]
    public void TestPredict()
    {
        var prediction_table_generator = new PredictionTableGenerator(RightRecursiveCfg.Rules);
        prediction_table_generator.Gen("E");
        var table = prediction_table_generator.first_table;
        var follow_table = prediction_table_generator.follow_table;
        var full = prediction_table_generator.first_full_table;
        Console.WriteLine("======= First Table =======");
        foreach (var (key, value) in table)
        {
            Console.WriteLine($"{key}\t: {string.Join(',', value)}");
        }

        Console.WriteLine("======= Follow Table =======");
        foreach (var (key, value) in follow_table)
        {
            Console.WriteLine($"{key}\t: {string.Join(',', value)}");
        }

        Console.WriteLine("======= Full Version =======");

        var all_terminal = RightRecursiveCfg.Rules.GetAllTerminal();
        var text_table = new TextTable(all_terminal.Count + 1);
        text_table.AddCell("");
        all_terminal.ForEach(t => text_table.AddCell(t));
        foreach (var cfg_rule in RightRecursiveCfg.Rules)
        {
            var rule = cfg_rule.Variable;
            text_table.AddCell(rule);
            foreach (var terminal in all_terminal)
            {
                if (full.TryGetValue((rule, terminal), out var prod))
                    text_table.AddCell(new CfgRule(rule, prod).ToString());
                else
                    text_table.AddCell("");
            }
        }

        Console.WriteLine(text_table.Render());
        // Console.WriteLine(format);
    }

    [Test]
    public void TestLL1()
    {
        var ll1_parser = new LL1Parser(RightRecursiveCfg);
        TestParse(ll1_parser, "a", "F");
        TestParse(ll1_parser, "a+a", "E");
        TestParse(ll1_parser, "a+a*a", "E");
    }

    [Test]
    [TestCase("a", "F")]
    [TestCase("a+a", "E")]
    [TestCase("a+a*a", "E")]
    public void TestCalculateLL1(string input, string rule)
    {
        var ll1_parser = new LL1Parser(RightRecursiveCfg);
        TestParse(ll1_parser, input, rule);
    }

    void TestParse(LL1Parser parser, string input, string rule)
    {
        var syntax_node = parser.Parse(input, rule);
        syntax_node?.Print();
        var dot = syntax_node.ToGraph().ToGraphviz(algorithm =>
        {
            algorithm.FormatVertex += (_, args) =>
            {
                args.VertexFormat.Shape = args.Vertex.OnlyNode ? GraphvizVertexShape.Box : GraphvizVertexShape.Circle;
                args.VertexFormat.Label = args.Vertex.ToString();
            };
        });
        var random = new Random();
        var rand_str = new string(Enumerable.Range(0, 6).Select(ch => (char)random.Next('a', 'z')).ToArray());
        dot.ExportDotToSvg(rand_str, "svg");
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