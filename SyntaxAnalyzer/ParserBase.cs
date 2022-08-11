namespace SyntaxAnalyzer;

public abstract class ParserBase
{
    public string Input { get; set; }

    /// <summary>
    /// current parsing index
    /// </summary>
    public int CurIndex { get; protected set; } = 0;

    public abstract void Parse();

    public ParserBase(string input)
    {
        Input = input;
    }

    public void ConsumeWhitespace()
    {
        for (; CurIndex < Input.Length && Char.IsWhiteSpace(Input[CurIndex]); CurIndex++) ;
    }
}