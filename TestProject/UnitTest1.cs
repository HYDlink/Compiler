using System.Text.RegularExpressions;
using FluentAssertions;
using SyntaxAnalyzer;
using SyntaxAnalyzer.Exceptions;

namespace TestProject;

public class Tests
{
    [SetUp]
    public void Setup()
    {
    }

    [Test]
    public void Test1()
    {
        var infix_parser = new InfixParser("9-5+2");
        infix_parser.Parse();
        Assert.Pass();
    }

    [Test]
    public void TestSimpleLua()
    {
        var str = @"
if a == 32 then
    a = 5
    while do
    end
end";
        var lua_parser = new LuaParser(str);
        lua_parser.Parse();
    }
    
    [Test]
    public void TestSimpleLuaLexical()
    {
        var str = @"32 a53 64a";
        var lua_parser = new LuaParser(str);
        Action action = () => lua_parser.Parse();
        action.Should().Throw<LexicalException>("64a is not digit or identifier");
    }

    [Test]
    public void TestRegexGroup()
    {
        var regex = new Regex(@"(?<id>[_a-zA-Z]\w*)|(?<k>if)");
        var match_if = regex.Match("if");
        Console.WriteLine(match_if.Groups["k"]);
        Console.WriteLine(regex.Match("_haha"));
        Console.WriteLine(regex.Match("4l"));
        Console.WriteLine(regex.Match("l4"));
    }
}