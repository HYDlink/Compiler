namespace SyntaxAnalyzer;

public class SyntaxNode
{
    public string Input { get; set; }
    public int Index { get; set; }
    public int Length { get; set; }
    
    public string NodeType { get; set; }
    public string Id { get; set; }

    public override string ToString()
    {
        return Input.Substring(Index, Length);
    }
}

public class SyntaxTree
{
    public SyntaxNode Element { get; set; }
    public List<SyntaxTree> Children { get; set; } = new();
    public SyntaxTree() {}

    public SyntaxTree(SyntaxNode syntaxNode)
    {
        Element = syntaxNode;
    }
}