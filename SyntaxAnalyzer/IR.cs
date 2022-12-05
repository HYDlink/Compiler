using System.Diagnostics;
using QuikGraph;

namespace SyntaxAnalyzer;

public enum OpCode
{
    Store,
    Add,
    Sub,
    Branch,
    Jump,
    Label,
    Compare,
}

public record Operand() : IParsable<Operand>
{
    public static Operand? Parse(string s, IFormatProvider? provider)
    {
        if (s.Length <= 1)
            return null;
        var id = s.Substring(1);
        Debug.Assert(id.All(char.IsLetterOrDigit));
        return s[0] switch
        {
            '%' => new LabelOperand(id),
            '$' => new IntOperand(int.Parse(id)),
            '@' => new VariableOperand(id),
            _ => null
        };
    }

    public static bool TryParse(string? s, IFormatProvider? provider, out Operand result)
    {
        result = Parse(s, provider);
        return result != null;
    }
}

public record LabelOperand(string Label) : Operand
{
    public override string ToString() => "%" + Label;
}

public record IntOperand(int Value) : Operand
{
    public override string ToString() => "$" + Value;
}

public record VariableOperand(string Variable) : Operand
{
    public override string ToString() => "@" + Variable;
}

/// <summary>
/// TODO: 更好的定义不同 OpCode 的放置，不同的 OpCode 的实现，而非使用 switch <br/>
/// Parse Syntax: <c>OpCode Dest Operand1[Optional] Operand2[Optional]</c> <br/>
/// eg: <c>Jump %a</c>, <c>Store @a, $2</c>
/// </summary>
/// <remarks>
/// 实现问题，没有统一好标准，后续修改起来也这里一串那里一串的
/// </remarks>
/// <param name="OpCode"></param>
/// <param name="Operand1"></param>
/// <param name="Operand2"></param>
/// <param name="Dest"></param>
public record Op(OpCode OpCode, Operand? Operand1 = null, Operand? Operand2 = null, Operand? Dest = null) : IParsable<Op>
{
    public void Execute()
    {
    }

    public static Op Jump(string label)
        => new Op(OpCode.Jump, null, null, new LabelOperand(label));

    public static Op Label(string label) => new Op(OpCode.Label, new LabelOperand(label));

    public static Op Store(int value, string var)
        => new Op(OpCode.Store, new IntOperand(value), null, new VariableOperand(var));

    public static Op Branch(string condVar, string then, string @else)
        => new Op(OpCode.Branch, new LabelOperand(@then), new LabelOperand(@else), new VariableOperand(condVar));

    public static bool IsNotLabel(Op op) => op.OpCode != OpCode.Label;
    
    /// <summary>
    /// Syntax: <c>Op dest arg1 arg2</c>
    /// </summary>
    /// <param name="s"></param>
    /// <param name="provider"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static Op Parse(string s, IFormatProvider? provider = null)
    {
        var str = s.Split().Where(value => !string.IsNullOrWhiteSpace(value)).ToArray();
        Debug.Assert(str.Length >= 1);
        if (!Enum.TryParse<OpCode>(str[0], out var opcode))
        {
            throw new FormatException();
        }

        var operands = str.Skip(1).Select(s1 => Operand.Parse(s1, provider)).ToList();
        Debug.Assert(operands.All(o => o != null));
        var dest = operands.Count > 0 ? operands[0] : null;
        var arg1 = operands.Count > 1 ? operands[1] : null;
        var arg2 = operands.Count > 2 ? operands[2] : null;
        return new Op(opcode, arg1, arg2, dest);
    }

    public static bool TryParse(string? s, IFormatProvider? provider, out Op result)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        return OpCode + " " + string.Join(", ", 
            new[] { Dest, Operand1, Operand2 }
            .TakeWhile(o => o is {})
            // .OfType<Operand>()
            .Select(o => o.ToString()));
        return OpCode + " " +
               (Dest?.ToString() ?? "") + " " + 
               (Operand1?.ToString() ?? "") + " " +
               (Operand2?.ToString() ?? "");
    }
};

public record class BasicBlock(List<Op> Ops, int Start, int End);

public record IRBlock(List<Op> OpCodes): IParsable<IRBlock>
{
    public IRContext IrContext = new();

    public int GetValue(Operand operand) => operand switch
    {
        IntOperand int_operand => int_operand.Value,
        LabelOperand { Label: { } label } => GetPcByLabel(label),
        VariableOperand { Variable: { } var } => IrContext.Get(var),
        _ => throw new ArgumentOutOfRangeException(nameof(operand))
    };

    public int GetPcByLabel(string label)
        => OpCodes.FindIndex(o => o.OpCode == OpCode.Label
                                  && o.Operand1 is LabelOperand { Label: { } label_operand }
                                  && label == label_operand);

    public Op? GetOpByLabel(string label)
        => OpCodes.FirstOrDefault(o => o.OpCode == OpCode.Label
                                       && o.Operand1 is LabelOperand { Label: { } label_operand }
                                       && label == label_operand);

    private void Jump(Operand operand)
    {
        var next_pc = GetValue(operand);
        Jump(next_pc);
    }

    private void Jump(int pc)
    {
        IrContext.SetPC(pc);
    }

    public bool IsBasicBlock()
        => OpCodes.All(op => op.OpCode != OpCode.Branch && op.OpCode != OpCode.Jump);

    public IRContext Execute()
    {
        while (IrContext.GetPC() < OpCodes.Count)
        {
            var pc = IrContext.GetPC();
            var op = OpCodes[pc];
            switch (op.OpCode)
            {
                case OpCode.Store:
                {
                    var op1 = GetValue(op.Operand1);
                    var operand = op.Dest as VariableOperand;
                    IrContext.Set(operand.Variable, op1);
                }
                    break;
                case OpCode.Add:
                {
                    var op1 = GetValue(op.Operand1);
                    var op2 = GetValue(op.Operand2);
                    var operand = op.Dest as VariableOperand;
                    IrContext.Set(operand.Variable, op1 + op2);
                }
                    break;
                case OpCode.Sub:
                case OpCode.Compare:
                {
                    var op1 = GetValue(op.Operand1);
                    var op2 = GetValue(op.Operand2);
                    var operand = op.Dest as VariableOperand;
                    IrContext.Set(operand.Variable, op1 - op2);
                }
                    break;
                case OpCode.Branch:
                {
                    var op1 = GetValue(op.Operand1);
                    var op2 = GetValue(op.Operand2);
                    var cond = GetValue(op.Dest);
                    Jump(cond == 0 ? op1 : op2);
                }
                    break;
                case OpCode.Jump:
                    Jump(op.Dest);
                    continue;
                case OpCode.Label:
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }

            IrContext.IncPC();
        }

        return IrContext;
    }


    public abstract record DagNode
    {
        public DagNode? Parent { get; set; }
    }

    public record Leaf(string Var) : DagNode
    {
        public override int GetHashCode()
        {
            return Var.GetHashCode();
        }
    }

    public record Node(OpCode OpCode) : DagNode
    {
        public List<DagNode> Children { get; } = new();
        public List<string> Variables { get; } = new();
        public void AddChild(DagNode node, bool setParent = false)
        {
            Children.Add(node);
            if (setParent)
                node.Parent = this;
        }

        public void BindChildren()
        {
            foreach (var child in Children)
            {
                child.Parent = this;
            }
        }

        public override int GetHashCode()
        {
            HashCode hash_code = new();
            foreach (var child in Children)
            {
                hash_code.Add(child.GetHashCode());
            }
            hash_code.Add(OpCode);
            
            return hash_code.ToHashCode();
        }
    }

    public AdjacencyGraph<DagNode, Edge<DagNode>> BasicBlockDAG()
    {
        var definitions = new Dictionary<string, DagNode>();
        var memos = new HashSet<DagNode>();

        DagNode GetNode(Operand operand)
        {
            var var = ((VariableOperand)operand).Variable;
            if (definitions.TryGetValue(var, out var node))
                return node;
            var leaf = new Leaf(var);
            definitions[var] = leaf;
            return leaf;
        }

        void Override(string var, DagNode node)
        {
            if (definitions.TryGetValue(var, out var value))
            {
                Console.WriteLine($"Killed {var}");
                memos.Remove(value);
            }

            definitions[var] = node;
        }

        DagNode GetOpNode(Op op)
        {
            var var = ((VariableOperand)op.Dest).Variable;
            var node = new Node(op.OpCode);
            if (op.Operand1 is VariableOperand {} o)
            {
                var child = GetNode(o);
                node.AddChild(child);
            }
            if (op.Operand2 is VariableOperand {} o2)
            {
                var child = GetNode(o2);
                node.AddChild(child);
            }

            // find memo in code
            // if (memos.Contains(node))
            if (memos.FirstOrDefault(m => m.GetHashCode() == node.GetHashCode()) is Node memo)
            {
                // var memo = memos.FirstOrDefault(m => m.GetHashCode() == node.GetHashCode()) as Node;
                Console.WriteLine($"Used memo: {op}");
                Debug.Assert(memo is {});
                node = memo;
            }
            else // store memo
            {
                Console.WriteLine($"Store memo: {op}");
                node.BindChildren();
                memos.Add(node);
            }
            
            Override(var, node);
            return node;
        }

        DagNode GetRoot(DagNode node)
        {
            var cur = node;
            while (cur.Parent != null) cur = cur.Parent;
            return cur;
        }

        var dag_nodes = OpCodes.Select(GetOpNode).ToList();
        var result_nodes = dag_nodes.Select(GetRoot).Distinct().ToList();
        foreach (var (key, value) in definitions)
        {
            if (value is Node node) node.Variables.Add(key);
        }

        List<Edge<DagNode>> GetChildrenEdges(DagNode node)
        {
            if (node is Leaf or null)
            {
                return new();
            }
            if (node is Node n)
            {
                var current = n.Children.Select(c => new Edge<DagNode>(n, c));
                var childrens_edges = n.Children.SelectMany(c => GetChildrenEdges(c));
                return childrens_edges.Concat(current).ToList();
            }
            else
            {
                throw new NotSupportedException();
            }
        }

        var edges = result_nodes.SelectMany(GetChildrenEdges).ToList();
        return edges.ToAdjacencyGraph<DagNode, Edge<DagNode>>();
    }

    // public bool Verify()
    // {
    //     OpCodes.Where(op => op.OpCode == OpCode.Jump)
    // }

    public AdjacencyGraph<BasicBlock, TaggedEdge<BasicBlock, string>> ToControlFlowGraph()
    {
        // var first_index = OpCodes.FirstOrDefault(Op.IsNotLabel);
        var first_index = OpCodes.FindIndex(Op.IsNotLabel);
        if (first_index is -1)
            return null;
        List<int> bb_labels = new() { first_index };
        // var last_index = OpCodes.Last(Op.IsNotLabel);
        var last_index = OpCodes.Count - 1;
        List<int> end_labels = new() { last_index };

        List<(int from, int to, string transition)> transitions = new();

        for (int i = 0; i < OpCodes.Count; i++)
        {
            var op = OpCodes[i];
            if (op.OpCode == OpCode.Jump)
            {
                // end_labels.Add(OpCodes[i]);
                if (i + 1 < OpCodes.Count)
                    bb_labels.Add(i + 1);
                var jump_dest = GetValue(op.Dest);
                bb_labels.Add(jump_dest);
                transitions.Add((i, jump_dest, "Jump"));
            }

            if (op.OpCode == OpCode.Branch)
            {
                // end_labels.Add(i);
                if (i + 1 < OpCodes.Count)
                    bb_labels.Add(i + 1);
                var jump_dest = GetValue(op.Operand1);
                var j2_dest = GetValue(op.Operand2);
                transitions.Add((i, j2_dest, $"{op.Dest} Success"));
                transitions.Add((i, jump_dest, $"{op.Dest} Failed"));
                bb_labels.Add(j2_dest);
                bb_labels.Add(jump_dest);
            }
        }


        // var non_labels_op = OpCodes.Where(Op.IsNotLabel).ToList();
        var bb_label_indexes = bb_labels.Distinct()
            // .Select(b => OpCodes.IndexOf(b))
            .OrderBy(i => i).ToList();
        // end_labels = end_labels.Distinct().OrderBy(OpCodes.IndexOf).ToList();
        var basic_blocks = bb_label_indexes.Zip(bb_label_indexes.Skip(1).Append(last_index + 1))
            .Select(t =>
                new BasicBlock(OpCodes.GetRange(t.First, t.Second - t.First), t.First, t.Second - 1))
            .ToList();

        BasicBlock GetFrom(int i) => basic_blocks.First(b => b.End == i);
        BasicBlock GetTo(int i) => basic_blocks.First(b => b.Start == i);

        return transitions.Select(t =>
                new TaggedEdge<BasicBlock, string>(
                    GetFrom(t.from),
                    GetTo(t.to),
                    t.transition))
            .ToAdjacencyGraph<BasicBlock, TaggedEdge<BasicBlock, string>>();
    }

    public static IRBlock Parse(string s, IFormatProvider? provider = null)
    {
        var lines = s.Split(new [] {'\r', '\n'}, StringSplitOptions.RemoveEmptyEntries);
        return new IRBlock(lines.Select(line => Op.Parse(line)).ToList());
    }

    public static bool TryParse(string? s, IFormatProvider? provider, out IRBlock result)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        return string.Join('\n', OpCodes.Select(o => o.ToString()));
    }
}

public class IRContext
{
    public const string PC = "PC";
    public Dictionary<string, int> Register = new() { { PC, 0 } };

    public void Set(string var, int val) => Register[var] = val;

    // TODO exception
    public int Get(string var) => Register[var];
    public int GetPC() => Get(PC);
    public void SetPC(int val) => Set(PC, val);
    public void IncPC() => Set(PC, GetPC() + 1);
}