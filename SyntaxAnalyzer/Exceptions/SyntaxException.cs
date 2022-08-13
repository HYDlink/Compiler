namespace SyntaxAnalyzer.Exceptions;

public class SyntaxException : Exception
{
    public int Line { get; set; }
    public int Pos { get; set; }

    public SyntaxException(int pos, string message) : base(message) {
        Pos = pos;
    }

    public SyntaxException(int line, int pos) {
        Line = line;
        Pos = pos;
    }
    
    
    public SyntaxException(SyntaxNode syntaxNode, string message) : base(message) {
        Pos = syntaxNode.Index;
    }
}