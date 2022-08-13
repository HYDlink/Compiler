namespace SyntaxAnalyzer;

public record class Token(string Tag)
{
	public const string DIGIT_TAG = "Digit";
	public const string KEYWORD_TAG = "Keyword";
	public const string IDENTIFIER_TAG = "Identifier";
	public const string TYPE_TAG = "Type";
	public const string OP_TAG = "Operator";
	public const string SYMBOL_TAG = "Symbol";
}

public record class Keyword(string Word) : Token(KEYWORD_TAG)
{
}

public record class OperatorToken(string Op) : Token(OP_TAG)
{
}

public record class DigitToken(string Value) : Token(DIGIT_TAG)
{
}

public record class IdentifierToken(string Value): Token(DIGIT_TAG)
{
}