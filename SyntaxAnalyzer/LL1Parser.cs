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

    private record ProductionNode(CfgProduction Production, string Rule)
        : CfgNode(Rule, CfgNodeType.NonTerminal)
    {
        public override string ToString() => $"{Rule} → {Production}";
    }

    public SyntaxNode Parse(string input, string? rootRule = null)
    {
        Console.WriteLine($"Parsing {input}, with rule: {rootRule}");
        var stack = new Stack<CfgNode>();
        // 使用另一个栈，来保存当前已经解析完成，使用产生式产生了的字符串结果
        var processed_stack = new Stack<SyntaxNode>();

        void LogPresent()
        {
            Console.WriteLine($"◜P {string.Join(", ", stack)}");
            Console.WriteLine($"◟F {string.Join(", ", processed_stack)}");
        }

        var span = input.AsSpan();
        rootRule ??= RootRule;
        stack.Push(CfgNode.NonTerminal(rootRule));
        var cur_char_index = 0;
        while (stack.Any())
        {
            var cfg_node = stack.Pop();
            if (cfg_node is ProductionNode { Production: { } production, Rule: { } rule })
            {
                Console.WriteLine($"Processing production: {production}");
                LogPresent();

                var cur_node = new SyntaxNode() { CfgNode = new CfgNode(rule, CfgNodeType.NonTerminal) };
                if (processed_stack.Count < production.Count)
                {
                    Console.WriteLine($"Error processing production: {production}");
                }

                var success = true;
                foreach (var node in Enumerable.Reverse(production))
                {
                    var syntax_node = processed_stack.Pop();
                    if (syntax_node.CfgNode == node)
                    {
                        cur_node.Children.Add(syntax_node);
                    }
                    else
                    {
                        // error
                        Console.WriteLine("Error collecting production");
                        success = false;
                        break;
                    }
                }

                if (success)
                    processed_stack.Push(cur_node);

                LogPresent();
                continue;
            }

            var (value, cfg_node_type) = cfg_node;
            if (cfg_node_type == CfgNodeType.Terminal)
            {
                var sliced = span.Slice(cur_char_index, value.Length);
                var success = sliced.SequenceEqual(value);
                // 对上了 terminal 的匹配
                Console.WriteLine($"{(success ? "Success" : "Failure")} processing: {cfg_node}");
                // span = span[value.Length..];
                var processed = new SyntaxNode()
                {
                    Input = sliced.ToString(),
                    Index = cur_char_index, Length = value.Length,
                    // NodeType = value,
                    CfgNode = cfg_node,
                };
                processed_stack.Push(processed);
                LogPresent();
                cur_char_index += value.Length;
            }
            else if (cfg_node_type == CfgNodeType.NonTerminal)
            {
                // assume element is one length
                if (cur_char_index >= span.Length)
                {
                    Console.WriteLine($"Parsing complete, but left stack {string.Join(", ", stack)}\n" +
                                      $"\tProcessing stack {string.Join(", ", processed_stack)}");
                    // break;
                    // 解析完成了，但是还要把 Production 全部收集起来然后 Parse
                    // 处理 empty production
                    var epsilon_prod = ContextFreeGrammar.Rules.GetRule(value)?.Productions
                        ?.FirstOrDefault(p => p.IsEpsilon);
                    if (epsilon_prod is { })
                    {
                        processed_stack.Push(new SyntaxNode() { Input = "", CfgNode = cfg_node });
                    }
                }
                else
                {
                    var c = span[cur_char_index].ToString();
                    if (PredictionTable.FirstFullTable.TryGetValue((value, c), out var prods))
                    {
                        if (prods.Count == 1)
                        {
                            var prod = prods[0];
                            // 把准备处理的 production 放入
                            // if (!prod.IsEpsilon)
                            {
                                var production_node = new ProductionNode(prod, value);
                                Console.WriteLine($"Pushing {production_node}");
                                stack.Push(production_node);
                            }
                            for (int i = prod.Count - 1; i >= 0; i--)
                            {
                                stack.Push(prod[i]);
                            }

                            LogPresent();
                        }
                        else if (prods.Count == 0)
                        {
                            // epsilon, nothing
                        }

                        // 底下的都是错误处理程序
                        else
                        {
                            Console.WriteLine("error, no else back");
                        }
                    }
                    else
                    {
                        if (PredictionTable.FollowTable.TryGetValue(value, out var follows)
                            && follows.Contains(c))
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
        if (processed_stack.Count > 0)
        {
            return processed_stack.Pop();
        }

        return null;
    }
}