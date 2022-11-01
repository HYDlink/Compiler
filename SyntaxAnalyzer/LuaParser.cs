using System.Text.RegularExpressions;
using SyntaxAnalyzer.Exceptions;

namespace SyntaxAnalyzer;

/// 这是 C#
public class LuaParser : ParserBase
{
    public List<Token> Tokens { get; set; } = new();
    public LuaParser(string input) : base(input)
    {
    }

    public static List<string> Operators { get; } = new()
    {
        "<", ">", "=", "<=", ">=", "=="
    };
    
    public static Dictionary<string, OperatorToken> OperatorTokens { get; } 
        = Operators.ToDictionary(k => k, v => new OperatorToken(v));
    
    public static List<string> Keywords { get; } = new()
    {
        "if", "while", "do", "then", "end", "begin"
    };

    public static Dictionary<string, Keyword> KeyWordTokens { get; } 
        = Keywords.ToDictionary(k => k, v => new Keyword(v));

    private void Consume(Func<char, bool> predicate)
    {
        for (; CurIndex < Input.Length && predicate(Input[CurIndex])
             ; ++CurIndex) ;
    }

    public Token? RegexMatch()
    {
        // 优先级测试，Keyword，Operator，comment，Identifier 组合
        // regex generate
        return null;
    }

    public Token? MatchToken()
    {
        var nono = '\0';
        var to_restore = CurIndex;
        if (CurIndex >= Input.Length)
            return null;
        // Clear Whitespace
        if (Input[CurIndex] == '\n')
        {
            CurLine++;
            CurPosInLine = 1;
        }
        Consume(char.IsWhiteSpace);
        
        // Try match comment
        if (Match("#"))
        {
            Consume(ch => ch == '\n');
            // re lexical analyze
            return MatchToken();
        }
        
        // Try match operators
        foreach (var op in Operators)
        {
            if (Match(op))
                return OperatorTokens[op];
        }

        // Try match keywords
        foreach (var keyword in Keywords)
        {
            if (Match(keyword))
                return KeyWordTokens[keyword];
        }
        // match digit, if failed, return error
        if (char.IsDigit(Input[CurIndex]))
        {
            var digit_start = CurIndex;
            Consume(char.IsDigit);
            // exclusive
            var digit_end = CurIndex;
            var digit_value = Input[digit_start..digit_end];
            if (CurIndex < Input.Length && char.IsLetter(Input[CurIndex]))
            {
                throw new LexicalException(CurLine, CurIndex, $"Digit {digit_value} end with letter: {Input[CurIndex]}");
            }

            return new DigitToken(digit_value);
        }

        // match id
        var match = Regex.Match(Input[CurIndex..], @"[_a-z|A-Z]\w*");
        if (match.Success)
        {
            var id = match.Value;
            CurIndex += match.Length;
            return new IdentifierToken(match.Value);
        }
        
        // match other symbol
        
        // match failed
        return null;
    }

    public void LexicalAnalyze()
    {
        for (var token = MatchToken(); token != null;)
        {
            Tokens.Add(token);
            Console.WriteLine(token);
            token = MatchToken();
        }
    }

    public override void Parse()
    {
        LexicalAnalyze();
    }
    
    public void SyntaxDirectAnalyze() 
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