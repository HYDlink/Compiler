using QuikGraph;
using QuikGraph.Algorithms;

namespace SyntaxAnalyzer;

public enum CfgNodeType
{
    NonTerminal,
    Terminal
}

public record CfgNode(string Value, CfgNodeType Type)
{
    public static implicit operator CfgNode(string terminal)
        => Terminal(terminal);

    public static CfgNode Terminal(string value)
    {
        return new CfgNode(value, CfgNodeType.Terminal);
    }

    public static CfgNode NonTerminal(string value)
    {
        return new CfgNode(value, CfgNodeType.NonTerminal);
    }

    public override string ToString()
        => Type switch
        {
            CfgNodeType.NonTerminal => $"<{Value}>",
            CfgNodeType.Terminal => $"{Value}",
            _ => throw new ArgumentOutOfRangeException()
        };
}

public class CfgProduction : List<CfgNode>
{
    public static readonly CfgProduction Epsilon = new();
    public bool IsEpsilon => Count == 0;
    public string Name { get; set; }

    public override string ToString()
        => IsEpsilon
            ? "ε"
            : string.Join(' ', this);

    public IEnumerable<string> LastNonEpsilonVariables(IEnumerable<string> canBeEpsilon)
    {
        foreach (var (value, type) in Enumerable.Reverse(this))
        {
            if (type == CfgNodeType.Terminal)
                yield break;
            if (type == CfgNodeType.NonTerminal)
                yield return value;
            if (!canBeEpsilon.Contains(value))
                yield break;
        }
    }
}

public record CfgRule(string Variable, List<CfgProduction> Productions)
{
    public List<string> GetProductVariables()
    {
        return Productions.SelectMany(i => i)
            .Where(c => c.Type == CfgNodeType.NonTerminal)
            .Select(c => c.Value).Distinct().ToList();
    }

    public override string ToString()
    {
        return $"{Variable} → {string.Join("\n\t| ", Productions)}";
    }
}

public record class ShiftedCfgRule(string Variable, List<CfgProduction> Productions, int Shift)
    : CfgRule(Variable, Productions)
{
    public ShiftedCfgRule(CfgRule rule, int shift) : this(rule.Variable, rule.Productions, shift)
    {
    }
}

public static class CfgRuleExtension
{
    public static CfgRule? GetRule(this IEnumerable<CfgRule> rules, string variable)
        => rules.FirstOrDefault(r => r.Variable == variable);

    public static List<CfgRule> SortRules(this List<CfgRule> rules)
    {
        return ExportConnections(rules).TopologicalSort().ToList();
    }

    /// <summary>
    /// for all productions in nodes
    /// </summary>
    /// <returns></returns>
    public static AdjacencyGraph<CfgRule, TaggedEdge<CfgRule, string>> ExportConnections(this List<CfgRule> rules)
        => rules.Select(r => r.GetProductVariables()
                .Select(v => GetRule(rules, v))
                .OfType<CfgRule>()
                .Select(target => new TaggedEdge<CfgRule, string>(r, target, target.Variable)))
            .SelectMany(i => i)
            .ToAdjacencyGraph<CfgRule, TaggedEdge<CfgRule, string>>();

    public static AdjacencyGraph<List<ShiftedCfgRule>, TaggedEdge<List<ShiftedCfgRule>, CfgNode>>
        ExportDk(this List<CfgRule> rules)
    {
        throw new NotImplementedException();
    }

    public static List<string> GetAllTerminal(this List<CfgRule> cfgRules)
        => cfgRules.SelectMany(r =>
                r.Productions.SelectMany(p => p)
                    .Where(n => n.Type == CfgNodeType.Terminal)
                    .Select(n => n.Value))
            .Distinct().ToList();
}

public record class ContextFreeGrammar(string Name, List<CfgRule> Rules)
{
    public string? RootRule;
}