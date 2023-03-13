using LuaAnalyzer.Infomation;

namespace LuaAnalyzer.Syntax;

public record AST
{
    
}

public record ParamList(List<SymbolItem> Params);
public record ArgList(List<Expression> Arguments);