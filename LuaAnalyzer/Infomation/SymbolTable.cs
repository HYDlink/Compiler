namespace LuaAnalyzer.Infomation;

public class SymbolTable
{
    public List<SymbolItem> SymbolItems { get; set; }
    // TODO how to resolve block and parent relations
    public SymbolTable Parent { get; set; }

    public SymbolItem? Get(string id)
    {
        return SymbolItems.FirstOrDefault(s => s.Id == id);
    }
}