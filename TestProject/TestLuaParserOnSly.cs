using LuaAnalyzer;

namespace TestProject;

public class TestLuaParserOnSly
{
    [Test]
    public void TestSly()
    {
        var block = Parser.Parse(@"a = 3
b = true
c = ""Hello""");
        Console.WriteLine(string.Join('\n', block.Statements));
    }
}