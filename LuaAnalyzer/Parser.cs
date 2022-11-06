using LuaAnalyzer.Syntax;
using sly.parser;
using sly.parser.generator;

namespace LuaAnalyzer;

public static class Parser
{
    public static Parser<LexToken, AST> GetParser(string rootRule = "Block")
    {
        var parserInstance = new SyntaxParser();
        var builder = new ParserBuilder<LexToken, AST>();
        var parserBuilder = builder.BuildParser(parserInstance,
            ParserType.EBNF_LL_RECURSIVE_DESCENT, rootRule);
        if (parserBuilder.IsError)
        {
            var message = string.Join("\n", parserBuilder.Errors.Select(e => e.Message));
            Console.WriteLine(message);
            throw new System.Exception(message);
        }

        var parser = parserBuilder.Result;

        return parser;
    }

    public static Block Parse(string script)
    {
        var r = GetParser().Parse(script);
        if (r.IsError)
        {
            var msg = "Parser Error";
            if (r.Errors != null && r.Errors.Any())
            {
                msg += "\n" + string.Join("\n\t", r.Errors.Select(r => r.ErrorMessage));
            }

            throw new System.Exception(msg);
        }
        else
        {
            return (Block)r.Result;
        }
    }
}