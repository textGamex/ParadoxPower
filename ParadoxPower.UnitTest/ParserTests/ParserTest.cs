namespace ParadoxPower.UnitTest.ParserTests;

using ParadoxPower.UnitTest;

[TestFixture]
public sealed class SharedParserTest
{
    [Test]
    public void RgbParseTest()
    {
        const string text = """
            has_subject = RGB
            OR = {
                has_subject = RGB
                has_subject = rgb
            }
            """;

        var rootNode = ParserHelper.Parse(text);
        var leaf = rootNode.GetLeaf("has_subject");

        Assert.That(rootNode, Is.Not.Null);
        Assert.That(leaf, Is.Not.Null);
        Assert.That(leaf!.Value.ValueText, Is.EqualTo("RGB"));
    }

    [Test]
    public void HsvParseTest()
    {
        const string text = """
            has_subject = HSV
            OR = {
                has_subject = hsv
                has_subject = HSV
            }
            """;

        var rootNode = ParserHelper.Parse(text);
        var leaf = rootNode.GetLeaf("has_subject");

        Assert.That(rootNode, Is.Not.Null);
        Assert.That(leaf, Is.Not.Null);
        Assert.That(leaf!.Value.ValueText, Is.EqualTo("HSV"));
    }
}
