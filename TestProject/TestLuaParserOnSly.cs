using LuaAnalyzer;

namespace TestProject;

public class TestLuaParserOnSly
{
    const string simpleAssign = @"a = 3
b = true
c = ""Hello""";

    const string simple_if = @"a = 3
if true then
    a = true
end
";

    [Test]
    [TestCase(simpleAssign)]
    [TestCase(simple_if)]
    public void TestSly(string script)
    {
        var block = Parser.Parse(script);
        Console.WriteLine(string.Join('\n', block.Statements));
    }
}