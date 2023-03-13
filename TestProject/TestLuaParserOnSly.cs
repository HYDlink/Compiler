using LuaAnalyzer;

namespace TestProject;

public class TestLuaParserOnSly
{
    const string simpleAssign = @"a = 3
b = true
c = ""Hello""
print(c)";

    const string simple_if = @"if false then
    a = true
    print(""Never Reach"")
    print(a)
end
";

    [Test]
    [TestCase(simpleAssign)]
    [TestCase(simple_if)]
    [TestCase("print(\"hello world\")")]
    [TestCase(@"function hello()
    print(""32"")
end
hello()")]
    [TestCase(@"local function hello()
    print(""32"")
end
hello()")]
    [TestCase(@"hello = function()
    print(""32"")
end
hello()")]
    public void TestSly(string script)
    {
        var block = Parser.Parse(script);
        Console.WriteLine(string.Join('\n', block.Statements));
        Console.WriteLine("====== Execution Result =======");
        new ExecuteMachine().Execute(block);
    }
}