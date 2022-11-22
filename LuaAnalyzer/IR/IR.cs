using System.Diagnostics;

namespace LuaAnalyzer.IR;

public enum OpCode
{
    Store,
    Add,
    Branch,
    Jump,
    Label,
    Compare,
}

public record Operand();

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
/// TODO: 更好的定义不同 OpCode 的放置，不同的 OpCode 的实现，而非使用
/// </summary>
/// <param name="OpCode"></param>
/// <param name="Operand1"></param>
/// <param name="Operand2"></param>
/// <param name="Dest"></param>
public record Op(OpCode OpCode, Operand? Operand1 = null, Operand? Operand2 = null, Operand? Dest = null)
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
        => new Op(OpCode.Branch, new VariableOperand(condVar), new LabelOperand(@then), new LabelOperand(@else));

    public static bool IsNotLabel(Op op) => op.OpCode != OpCode.Label;
};

public record BasicBlock(string name, List<Op> Ops);

public record IRBlock(List<Op> OpCodes)
{
    public IRContext IrContext = new();

    public int GetValue(Operand operand) => operand switch
    {
        IntOperand int_operand => int_operand.Value,
        LabelOperand {Label: {} label} => GetPcByLabel(label),
        VariableOperand {Variable: {} var} => IrContext.Get(var),
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
    
    private void Jump(int pc){
        IrContext.SetPC(pc);
    }

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
                    var dest = GetValue(op.Dest);
                    Jump(op1 == 0 ? op2: dest);
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

    // public bool Verify()
    // {
    //     OpCodes.Where(op => op.OpCode == OpCode.Jump)
    // }

    public void ToControlFlowGraph()
    {
        var first_index = OpCodes.FirstOrDefault(Op.IsNotLabel);
        if (first_index is null)
            return; // null;
        List<Op> bb_labels = new() { first_index };
        List<Op> end_labels = new() { OpCodes.Last(Op.IsNotLabel) };
        
        var jump_to = OpCodes.Where(op => op.OpCode != OpCode.Jump).Select(op => op.Dest);
        var branches = OpCodes.Where(op => op.OpCode != OpCode.Branch);
        var cond_to = branches.Select(op => op.Operand2).Concat(branches.Select(op => op.Dest));
        var to_labels = jump_to.Concat(cond_to).ToList();

        for (int i = 0; i < OpCodes.Count; i++)
        {
            var op = OpCodes[i];
            if (op.OpCode == OpCode.Jump)
            {
                end_labels.Add(op);
                var jump_dest = OpCodes[GetValue(op.Dest)];
                bb_labels.Add(jump_dest);
            }
            if (op.OpCode == OpCode.Branch)
            {
                end_labels.Add(op);
                var j2_dest = OpCodes[GetValue(op.Operand2)];
                var jump_dest = OpCodes[GetValue(op.Dest)];
                bb_labels.Add(j2_dest);
                bb_labels.Add(jump_dest);
            }
        }

        // return no_label_ops;
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