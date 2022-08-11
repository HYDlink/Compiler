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
                Input = Input, Index = i, Length = 1
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
        for (int i = 0; i < syntax_nodes.Count; )
        {
            
        }
    }

    public bool IsExpr(SyntaxNode syntaxNode)
    {
        return syntaxNode.NodeType is "expr" or "add" or "sub" or "digit";
    }
}