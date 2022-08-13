namespace SyntaxAnalyzer.Exceptions;

public class LexicalException: Exception
{
    public int Line { get; set; }
    public int Pos { get; set; }

    public LexicalException(int pos, string message) : base(message) {
        Pos = pos;
    }

    public LexicalException(int line, int pos, string message) : base(message) {
        Line = line;
        Pos = pos;
    }
}