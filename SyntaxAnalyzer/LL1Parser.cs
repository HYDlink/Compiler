using System.Diagnostics;

namespace SyntaxAnalyzer;

public class LL1Parser
{
    private PredictionTable PredictionTable;
    private ContextFreeGrammar ContextFreeGrammar { get; set; }
    private string RootRule { get; set; }

    public LL1Parser(ContextFreeGrammar contextFreeGrammar)
    {
        ContextFreeGrammar = contextFreeGrammar;
        var prediction_table_generator = new PredictionTableGenerator(ContextFreeGrammar.Rules);
        RootRule = contextFreeGrammar.RootRule ??
                   contextFreeGrammar.Rules.SortRules().First().Variable;
        Debug.Assert(RootRule != null);
        PredictionTable = prediction_table_generator.Gen(RootRule);
    }

    public void Parse(string input, string? rootRule = null)
    {
        // TODO 使用另一个栈，来保存当前已经解析完成，使用产生式产生了的字符串结果

        var stack = new Stack<CfgNode>();
        var span = input.AsSpan();
        rootRule ??= RootRule;
        stack.Push(CfgNode.NonTerminal(rootRule));
        while (stack.Any())
        {
            var cfg_node = stack.Pop();
            var (value, cfg_node_type) = cfg_node;
            if (cfg_node_type == CfgNodeType.Terminal)
            {
                var success = span.Slice(0, value.Length).SequenceEqual(value);
                Console.WriteLine($"{(success ? "Success" : "Failure")} processing: {cfg_node}");
                span = span[value.Length..];
            }
            else if (cfg_node_type == CfgNodeType.NonTerminal)
            {
                // assume element is one length
                if (span.IsEmpty)
                {
                    Console.WriteLine($"Parsing complete, but left stack {stack}");
                    return;
                }
                else
                {
                    var c = span[0].ToString();
                    if (PredictionTable.FirstFullTable.TryGetValue((value, c), out var prods))
                    {
                        if (prods.Count == 1)
                        {
                            var prod = prods[0];
                            for (int i = prod.Count - 1; i >= 0; i--)
                            {
                                stack.Push(prod[i]);
                            }
                        }
                        else if (prods.Count == 0)
                        {
                            // epsilon, nothing
                        }
                        else
                        {
                            Console.WriteLine("error, no else back");
                        }
                    }
                    else
                    {
                        if (PredictionTable.FollowTable.TryGetValue(value, out var follows) && follows.Contains(c))
                        {
                            // error, no consume input
                            Console.WriteLine($"error parsing {value}, hit follow {c}");
                        }
                        else
                        {
                            // error, consume input, and push back
                            stack.Push(new CfgNode(value, CfgNodeType.NonTerminal));
                            Console.WriteLine($"error parsing {value}, no hit follow");
                        }
                    }
                }
            }
        }
        // check first char and prediction table
        // if correspond, derivation
    }
}