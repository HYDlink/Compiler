using System.Diagnostics;
using LuaAnalyzer.Syntax;

namespace LuaAnalyzer.Infomation;

public class SymbolTable
{
    public List<SymbolItem> SymbolItems { get; } = new();
    public int Layer { get; set; }
    public int IdInLayer { get; set; }

    public string Name { get; }

    // TODO how to resolve block and parent relations
    public SymbolTable? Parent { get; set; }

    public SymbolTable(string name)
    {
        Name = name;
    }

    public SymbolItem? Get(string id)
    {
        return SymbolItems.FirstOrDefault(s => s.Id == id);
    }

    public void Set(string id, LuaType type)
    {
        if (Get(id) is { } item)
        {
        }
        else
        {
            SymbolItems.Add(new SymbolItem() { Id = id, Type = type });
        }
    }
}

public class SymbolTableManager
{
    private const int MAX_LAYER = 10;
    public Dictionary<string, SymbolTable> SymbolTables { get; } = new();

    public SymbolTable? CurrentTable { get; private set; }

    private List<int> TableCountInLayer = Enumerable.Repeat(0, MAX_LAYER).ToList();

    public SymbolTable InitializeScope()
    {
        int layer = 0, id_in_layer = 0;
        string name = "0";
        if (CurrentTable is { })
        {
            layer = CurrentTable.Layer + 1;
            id_in_layer = TableCountInLayer[layer];
            TableCountInLayer[layer] = id_in_layer + 1;
            name = CurrentTable.Name + $"{layer}{(char)('a' + id_in_layer)}";
        }

        var new_table = new SymbolTable(name)
            { Layer = layer, IdInLayer = id_in_layer, Parent = CurrentTable };
        CurrentTable = new_table;
        Debug.Assert(CurrentTable != null);
        return new_table;
    }

    public void PopScope()
    {
        if (CurrentTable is null) return;
        CurrentTable = CurrentTable.Parent;
    }


    public (SymbolTable?, SymbolItem?) Get(string id)
    {
        var cur_table = CurrentTable;
        SymbolItem? item = cur_table.Get(id);
        cur_table = cur_table.Parent;
        while (cur_table is not null && item is null)
        {
            item = cur_table.Get(id);
            cur_table = cur_table.Parent;
        }

        return (cur_table, item);
    }

    public void Set(string id, LuaType type)
    {
        CurrentTable.Set(id, type);
        Console.WriteLine($"Set <{id}, {type}> in {CurrentTable.Name}");
    }
}