using System.Diagnostics;

namespace SyntaxAnalyzer;


public class PredictionTable
{
    public Dictionary<string, HashSet<string>> FirstTable { get; set; } = new();

    public Dictionary<string, HashSet<string>> FollowTable { get; set; } = new();

    public Dictionary<(string rule, string first), List<CfgProduction>> FirstFullTable { get; set; } = new();
}

public class PredictionTableGenerator
{
    public Dictionary<string, HashSet<string>> first_table { get; private set; } = new();

    public Dictionary<string, HashSet<string>> follow_table { get; private set; } = new();

    public Dictionary<(string rule, string first), List<CfgProduction>> first_full_table { get; private set; } = new();

    HashSet<string> traversal = new HashSet<string>();
    HashSet<string> canBeEpsilon = new();

    void AddFirstFull(string rule, string first, CfgProduction production)
    {
        var key = (rule, first);
        if (!first_full_table.ContainsKey(key))
            first_full_table[key] = new();
        first_full_table[key].Add(production);
    }

    void AddFollow(string? prev, string follow)
    {
        if (prev is null)
            return;
        if (!follow_table.ContainsKey(prev))
            follow_table[prev] = new();
        follow_table[prev].Add(follow);
    }


    public static Dictionary<string, HashSet<CfgNode>> CollectRightNeighbors(List<CfgRule> rules,
        HashSet<string> canBeEpsilon)
    {
        var neighbors = new Dictionary<string, HashSet<CfgNode>>();

        foreach (var (variable, _) in rules)
        {
            neighbors[variable] = new();
        }

        foreach (var (_, productions) in rules)
        {
            foreach (var production in productions)
            {
                for (var i = 0; i < production.Count - 1; i++)
                {
                    if (production[i].Type == CfgNodeType.Terminal)
                        continue;
                    var next_idx = i;
                    do
                    {
                        next_idx++;
                        neighbors[production[i].Value].Add(production[next_idx]);
                    } while (next_idx < production.Count - 1 &&
                             production[next_idx] is { Type: CfgNodeType.NonTerminal, Value: { } str }
                             && canBeEpsilon.Contains(str));
                }
            }
        }

        return neighbors;
    }

    public static Dictionary<string, HashSet<string>> SolveFollow(List<CfgRule> rules, HashSet<string> canBeEpsilon,
        Dictionary<string, HashSet<string>> firstTable)
    {
        var followTable = new Dictionary<string, HashSet<string>>();
        var right_neighbors = CollectRightNeighbors(rules, canBeEpsilon);
        var right_non_terminal_neighbors = right_neighbors.Select(kv =>
                (kv.Key,
                    kv.Value.Where(node => node.Type == CfgNodeType.NonTerminal)
                        .Select(node => node.Value).ToList()))
            .ToDictionary(tup => tup.Key, tup => tup.Item2);

        void Init()
        {
            foreach (var (variable, _) in rules)
            {
                followTable[variable] = new();
            }
        }

        bool UnionOtherRulesFollow(string cur, IEnumerable<string> from)
        {
            var changed = false;
            var to_add = followTable[cur];
            foreach (var last in from)
            {
                var except = to_add.Except(followTable[last]);
                changed |= except.Any();
                foreach (var s in except)
                {
                    followTable[last].UnionWith(except);
                }
            }

            return changed;
        }

        bool AddLast()
        {
            var changed = false;
            foreach (var (rule, productions) in rules)
            {
                var lasts = productions
                    // reverse get all last epsilon
                    .SelectMany(prod => prod.LastNonEpsilonVariables(canBeEpsilon));
                    // .Where(item => item is { Type: CfgNodeType.NonTerminal })
                    // .Select(item => item.Value);
                changed |= UnionOtherRulesFollow(rule, lasts);
            }

            return changed;
        }

        void AddTerminalNeighbor()
        {
            foreach (var (rule, neighbors) in right_neighbors)
            {
                var select = neighbors.Where(n => n.Type == CfgNodeType.Terminal).Select(n => n.Value);
                followTable[rule].UnionWith(select);
            }
        }

        bool AddNeighbor()
        {
            var changed = false;
            foreach (var (rule, neighbors) in right_non_terminal_neighbors)
            {
                var firsts = neighbors.SelectMany(n => firstTable[n]).Distinct();
                var to_add = firsts.Except(followTable[rule]).ToList();
                changed |= to_add.Any();
                followTable[rule].UnionWith(to_add);
            }

            return changed;
        }

        Init();
        AddTerminalNeighbor();
        var changed = true;
        int iter = 1;
        while (changed)
        {
            changed = AddLast() || AddNeighbor();

            if (false) // enable_debug
            {
                Console.WriteLine($"Iteration {iter}, Changed: {changed}");
                foreach (var (key, value) in followTable)
                {
                    Console.WriteLine($"{key}\t: {string.Join(',', value)}");
                }

                iter++;
            }
        }

        return followTable;
    }

    public static HashSet<string> SolveEpsilon(List<CfgRule> rules)
    {
        var canBeEpsilon = new HashSet<string>();
        var initial = rules
            .Where(r => r.Productions.Any(prod => prod.IsEpsilon))
            .Select(r => r.Variable);
        canBeEpsilon.UnionWith(initial);
        var changed = true;
        while (changed)
        {
            var next_epsilon_rules = rules
                .Where(r => r.Productions.Any(prod => 
                        prod.All(n => n.Type == CfgNodeType.NonTerminal && canBeEpsilon.Contains(n.Value)))
                    )
                .Select(r => r.Variable);
            var to_add = next_epsilon_rules.Except(canBeEpsilon);
            changed = to_add.Any();
            canBeEpsilon.UnionWith(to_add);
        }

        return canBeEpsilon;
    }

    private void SolveFirst()
    {
        foreach (var (variable, _) in CfgRules)
        {
            first_table[variable] = new();
        }

        // 存储，A -> Bc | C，的 A 与 B 和 C 的 First 包含关系，以进行迭代方程求解
        var to_solve = new Dictionary<string, List<(string, CfgProduction)>>();

        foreach (var (rule, prods) in CfgRules)
        {
            foreach (var prod in prods)
            {
                if (prod.IsEpsilon)
                {
                    // AddFirstFull(rule, );
                    // first_table[rule].Add()
                    // TODO solve epsilon
                    continue;
                }

                foreach (var (value, node_type) in prod)
                {
                    if (node_type == CfgNodeType.Terminal)
                    {
                        first_table[rule].Add(value);
                        AddFirstFull(rule, value, prod);
                        break;
                    }

                    if (node_type == CfgNodeType.NonTerminal)
                    {
                        if (!to_solve.ContainsKey(value))
                            to_solve[rule] = new();
                        to_solve[rule].Add((value, prod));

                        if (!canBeEpsilon.Contains(value))
                        {
                            break;
                        }
                    }
                }
            }
        }

        Console.WriteLine($"To solve relation: ");
        foreach (var (key, va) in to_solve)
        {
            Console.WriteLine($"\t{key} -> {string.Join(", ", va)}");
        }
        var changed = true;
        var iter = 0;
        while (changed)
        {
            changed = false;
            foreach (var (rule, children) in to_solve)
            {
                foreach (var (child, production) in children)
                {
                    var firsts = first_table[child];
                    foreach (var first in firsts)
                    {
                        // 检查是否已经在 first_table 或者 first_full_table 里面了，如果没有则全部进行添加
                        if (!first_table[rule].Contains(first) ||
                            !first_full_table.TryGetValue((rule, first), out var prods) ||
                            !prods.Contains(production))
                        {
                            changed = true;
                            first_table[rule].Add(first);
                            if (!first_full_table.ContainsKey((rule, first)))
                                first_full_table[(rule, first)] = new();
                            first_full_table[(rule, first)].Add(production);
                        }
                    }
                }
                
            }
            
            if (false) // enable_debug
            {
                Console.WriteLine($"Iteration {iter}, Changed: {changed}");
                foreach (var (key, value) in first_table)
                {
                    Console.WriteLine($"{key}\t: {string.Join(',', value)}");
                }

                iter++;
            }
        }
    }

    void Dfs(string rule)
    {
        traversal.Add(rule);
        first_table[rule] = new();
        foreach (var cfg_production in CfgRules.GetRule(rule).Productions)
        {
            foreach (var (node, type) in cfg_production)
            {
                if (type == CfgNodeType.Terminal)
                {
                    first_table[rule].Add(node);
                    AddFirstFull(rule, node, cfg_production);
                    break;
                }

                Debug.Assert(node != rule);
                if (!traversal.Contains(node))
                    Dfs(node);
                foreach (var s in first_table[node])
                {
                    AddFirstFull(rule, s, cfg_production);
                }

                first_table[rule].UnionWith(first_table[node]);
                if (!canBeEpsilon.Contains(node))
                    break;
            }
        }
    }

    private void AddFollowToEpsilonProd()
    {
        foreach (var rule in canBeEpsilon)
        {
            if (follow_table.TryGetValue(rule, out var follows))
            {
                foreach (var follow in follows)
                {
                    AddFirstFull(rule, follow, CfgProduction.Epsilon);
                }
            }
        }
    }

    public bool ValidateLl1()
    {
        return CfgRules.Select(r => r.Variable).All(r =>
                   !first_table.ContainsKey(r) || !follow_table.ContainsKey(r) ||
                   !first_table[r].Intersect(follow_table[r]).Any())
               && first_full_table.Values.All(prods => prods.Count <= 1);
    }

    private List<CfgRule> CfgRules;

    public PredictionTableGenerator(List<CfgRule> cfgRules)
    {
        CfgRules = cfgRules;
    }

    public PredictionTable Gen(string rootRule)
    {
        canBeEpsilon = SolveEpsilon(CfgRules);
        Console.WriteLine($"Epsilon rules: " + string.Join(", ",canBeEpsilon));
        
        SolveFirst();
        // Dfs(rootRule);

        follow_table = SolveFollow(CfgRules, canBeEpsilon, first_table);
        AddFollowToEpsilonProd();
        var validate_ll1 = ValidateLl1();
        Console.WriteLine($"is LL1: {validate_ll1}");
        return new()
        {
            FirstTable = first_table,
            FollowTable = follow_table,
            FirstFullTable = first_full_table,
        };
    }
}