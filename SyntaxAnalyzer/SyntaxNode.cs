using QuikGraph;

namespace SyntaxAnalyzer;

public class SyntaxNode
{
    public string Input { get; set; }
    public int Index { get; set; }
    public int Length { get; set; }
    
    public string NodeType { get; set; }
    public CfgNode CfgNode { get; set; }
    public string Id { get; set; }

    public bool OnlyNode => string.IsNullOrEmpty(Input);

    public override string ToString()
    {
        return string.IsNullOrEmpty(Input)
            ? $"{CfgNode.Value}"
            : Input;
    }

    public List<SyntaxNode> Children { get; set; } = new();

    private void PrintRecursive(int tab = 0) {
        System.Console.WriteLine(new string('\t', tab) + ToString());
        Children.ForEach(n => n.PrintRecursive(tab + 1));
    }

    public void Print() {
        PrintRecursive();
    }

    public AdjacencyGraph<SyntaxNode, TaggedEdge<SyntaxNode, string>> ToGraph()
    {
        List<TaggedEdge<SyntaxNode, string>> Edges = new();

        void RecursiveAdd(SyntaxNode node)
        {
            foreach (var syntax_node in node.Children)
            {
                Edges.Add(new TaggedEdge<SyntaxNode, string>(node, syntax_node, ""));
                RecursiveAdd(syntax_node);
            }
        }
        
        RecursiveAdd(this);
        return Edges.ToAdjacencyGraph<SyntaxNode, TaggedEdge<SyntaxNode, string>>();
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