using sly.lexer;

namespace LuaAnalyzer;

public enum LexToken
{
    #region keywords 0 -> 19

    [Keyword("if")]
    IF = 1,

    [Keyword("then")]
    THEN = 2,

    [Keyword("else")]
    ELSE = 3,

    [Keyword("while")]
    WHILE = 4,

    [Keyword("do")]
    DO = 5,

    [Keyword( "skip")]
    SKIP = 6,

    [Keyword("true")]
    TRUE = 7,

    [Keyword( "false")]
    FALSE = 8,

    [Keyword("not")]
    NOT = 9,

    [Keyword("and")]
    AND = 10,

    [Keyword("or")]
    OR = 11,

    [Keyword("print")]
    PRINT = 12,

    #endregion

    #region literals 20 -> 29

    [AlphaId] IDENTIFIER = 20,

    [String] STRING = 21,

    [Int] INT = 22,

    #endregion

    #region operators 30 -> 49

    [Sugar( ">")] GREATER = 30,

    [Sugar( "<")] LESSER = 31,

    [Sugar( "==")]
    EQUALS = 32,

    [Sugar( "!=")]
    DIFFERENT = 33,

    [Sugar( ".")] CONCAT = 34,

    [Sugar( "=")]
    ASSIGN = 35,

    [Sugar( "+")] PLUS = 36,

    [Sugar( "-")] MINUS = 37,

    [Sugar( "*")] TIMES = 38,

    [Sugar( "/")] DIVIDE = 39,

    #endregion

    #region sugar 50 ->

    [Sugar( "(")] LPAREN = 50,

    [Sugar( ")")] RPAREN = 51,

    [Sugar( ";")] SEMICOLON = 52,

    // EOF = 0

    #endregion
}