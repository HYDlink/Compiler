using SyntaxAnalyzer.Exceptions;

namespace SyntaxAnalyzer;

public class InfixParser : ParserBase
{
    public InfixParser(string input) : base(input)
    { }

    public override void Parse()
    {
        var syntax_nodes = new List<SyntaxNode>();
        for (int i = 0; i < Input.Length; i++)
        {
            var ch = Input[i];
            var syntax_node = new SyntaxNode()
            {
                Input = Input,
                Index = i,
                Length = 1
            };
            if (Char.IsDigit(ch))
            {
                syntax_node.NodeType = "Digit";
            }
            else
            {
                syntax_node.NodeType = ch.ToString();
            }
            syntax_nodes.Add(syntax_node);
        }

        // expr -> add | sub | digit
        // add -> expr + digit
        // sub -> expr - digit
        SyntaxNode left_root = null;
        for (int i = 0; i < syntax_nodes.Count;)
        {
            var current = syntax_nodes[i];
            if (current.NodeType is "Digit")
            {
                i++;
            }
            if (current.NodeType is { } syntax_type and ("+" or "-"))
            {
                if (i + 1 >= syntax_nodes.Count)
                {
                    throw new SyntaxException(current, $"{syntax_type} not end correctly");
                }
                if (left_root is null || !IsExpr(left_root))
                {
                    throw new SyntaxException(current, $"{syntax_type} left is not expr");
                }
                if (syntax_nodes[i + 1].NodeType is not "digit")
                {

                }
                current.Children = new() { left_root, syntax_nodes[i + 1] };
                current.NodeType = current.NodeType switch { "+" => "add", "-" => "sub", _ => "" };
                i += 2;
            }
            left_root = current;
        }

        left_root?.Print();
    }

    public bool IsExpr(SyntaxNode syntaxNode)
    {
        return syntaxNode.NodeType is "expr" or "add" or "sub" or "Digit";
    }
}