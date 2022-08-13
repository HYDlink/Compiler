namespace SyntaxAnalyzer;

public class LuaParser : ParserBase
{
    public LuaParser(string input) : base(input)
    {
    }

    public override void Parse()
    {
        Block();
    }

    private bool Block()
    {
        while (Statement()) ;
        return true;
    }

    private bool Statement()
    {
        var success = Match(";")
               || Match("if", false) && If()
               || Match("while", false) && While();
        if (success)
        {
            Console.WriteLine("Statement");
        }
        return success;
    }

    private bool If()
    {
        var success = Match("if") && Expression() && Match("then") 
                      && Block() && Match("end");
        
        if (success)
        {
            Console.WriteLine("If");
        }
        return success;
    }

    private bool Expression()
    {
        // TODO implement
        Console.WriteLine("Expression");
        return true;
    }

    private bool While()
    {
        var success = Match("while") && Expression() && Match("do") && Block() && Match("end");
        if (!success)
        {
            throw new SyntaxException(CurIndex, "");
        }
        else
        {
            Console.WriteLine("While");
        }
        return success;
    }

    public override bool Match(string str, bool increase = true)
    {
        var to_restore = CurIndex;
        ConsumeWhitespace();
        var success = base.Match(str, increase);
        if (!success)
            CurIndex = to_restore;
        return success;
    }
}