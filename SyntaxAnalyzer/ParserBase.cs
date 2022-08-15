namespace SyntaxAnalyzer;

public abstract class ParserBase
{
    public string Input { get; set; }

    /// <summary>
    /// current parsing index
    /// </summary>
    public int CurIndex { get; protected set; } = 0;
    public int CurLine { get; protected set; } = 1;
    public int CurPosInLine { get; protected set; } = 1;

    public abstract void Parse();

    public ParserBase(string input)
    {
        Input = input;
    }

    /// <summary>
    /// Test string <see cref="Input"/> start at <see cref="CurIndex"/> could match <paramref name="str"/> or not
    /// if <paramref name="increase"/> set and matched, would increase CurIndex to consume <paramref name="str"/>
    /// </summary>
    /// <param name="str"></param>
    /// <param name="increase"></param>
    /// <returns></returns>
    public virtual bool Match(string str, bool increase = true)
    {
        var success = CurIndex + str.Length <= Input.Length
               && Input.Substring(CurIndex, str.Length) == str;
        if (success && increase)
            CurIndex += str.Length;
        return success;
    }

    public void ConsumeWhitespace()
    {
        for (; CurIndex < Input.Length && Char.IsWhiteSpace(Input[CurIndex]); CurIndex++) ;
    }
}