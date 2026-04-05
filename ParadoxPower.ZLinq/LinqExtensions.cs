using System.Runtime.CompilerServices;
using ParadoxPower.Process;
using ZLinq;
using ZLinq.Linq;

namespace ParadoxPower.ZLinq;

public static class LinqExtensions
{
    public static ValueEnumerable<ArrayWhereSelect<Child, Node>, Node> NodesValue(this Node node)
    {
        return node
            .AllArray.AsValueEnumerable()
            .Where(static x => x.IsNodeChild)
            .Select(static x => Unsafe.As<Child.NodeChild>(x).Item);
    }

    public static ValueEnumerable<ArrayWhereSelect<Child, Leaf>, Leaf> LeavesValue(this Node node)
    {
        return node
            .AllArray.AsValueEnumerable()
            .Where(static x => x.IsLeafChild)
            .Select(static x => Unsafe.As<Child.LeafChild>(x).Item);
    }

    public static ValueEnumerable<ArrayWhereSelect<Child, LeafValue>, LeafValue> LeafValuesValue(
        this Node node
    )
    {
        return node
            .AllArray.AsValueEnumerable()
            .Where(static x => x.IsLeafValueChild)
            .Select(static x => Unsafe.As<Child.LeafValueChild>(x).Item);
    }

    public static ValueEnumerable<ArrayWhereSelect<Child, Comment>, Comment> CommentsValue(
        this Node node
    )
    {
        return node
            .AllArray.AsValueEnumerable()
            .Where(static x => x.IsCommentChild)
            .Select(static x => Unsafe.As<Child.CommentChild>(x).Item);
    }
}
