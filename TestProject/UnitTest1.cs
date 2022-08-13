using SyntaxAnalyzer;

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
        var str = @";
if then
    while do
    end
end";
        var lua_parser = new LuaParser(str);
        lua_parser.Parse();
    }
}