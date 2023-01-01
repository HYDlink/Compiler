namespace SyntaxAnalyzer;

public class SyntaxNode
{
    public string Input { get; set; }
    public int Index { get; set; }
    public int Length { get; set; }
    
    public string NodeType { get; set; }
    public CfgNode CfgNode { get; set; }
    public string Id { get; set; }

    public override string ToString()
    {
        return $"<{Input}, {CfgNode.Value}>";
    }

    public List<SyntaxNode> Children { get; set; } = new();

    private void PrintRecursive(int tab = 0) {
        System.Console.WriteLine(new string('\t', tab) + ToString());
        Children.ForEach(n => n.PrintRecursive(tab + 1));
    }

    public void Print() {
        PrintRecursive();
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