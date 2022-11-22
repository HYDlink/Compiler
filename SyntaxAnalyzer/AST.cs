using QuikGraph;

namespace SyntaxAnalyzer;

public class AST
{
    public string Name { get; set; }
    public AST? Left { get; set; }
    public AST? Right { get; set; }

    public bool IsLeaf => Left is null && Right is null;

    public AST(string name)
    {
        Name = name;
    }
    
    public static Dictionary<int, AST> Memo { get; set; } = new();
    public int GetMemoCode()
    {
        return HashCode.Combine(Name.GetHashCode(), Left?.GetHashCode(), Right?.GetHashCode());
    }

    public static AST? GetMemo(AST? node)
    {
        if (node is null) return node;
        var code = node.GetMemoCode();
        if (Memo.TryGetValue(code, out var new_leaf))
        {
            return new_leaf;
        }
        else
        {
            Memo[code] = node;
            return node;
        }
    }

    /// <summary>
    /// Construct from memorization
    /// </summary>
    /// <param name="name"></param>
    /// <param name="left"></param>
    /// <param name="right"></param>
    public AST(string name, AST left, AST? right = null)
    {
        Name = name;
        Left = GetMemo(left);
        Right = GetMemo(right);
    }

    public override string ToString() => Name;

    public enum D
    {
        L,
        R
    }

    public AdjacencyGraph<AST, TaggedEdge<AST, D>> ToGraph()
    {
        var q = new Queue<AST>();
        q.Enqueue(this);
        var edges = new List<TaggedEdge<AST, D>>();
        while (q.Any())
        {
            var count = q.Count;
            for (int i = 0; i < count; i++)
            {
                var t = q.Dequeue();
                if (t.Left is not null)
                {
                    q.Enqueue(t.Left);
                    edges.Add(new(t, t.Left, D.L));
                }

                if (t.Right is not null)
                {
                    q.Enqueue(t.Right);
                    edges.Add(new(t, t.Right, D.R));
                }
            }
        }

        return edges.ToAdjacencyGraph<AST, TaggedEdge<AST, D>>();
    }

    public AST toDAG()
    {
        throw new NotImplementedException();
    }

    public bool IsEqualTree(AST other)
    {
        if (this is null && other is null)
            return true;
        if (this is null || other is null)
            return false;
        return Name == other.Name && Left.IsEqualTree(other) && Right.IsEqualTree(other);
    }
}