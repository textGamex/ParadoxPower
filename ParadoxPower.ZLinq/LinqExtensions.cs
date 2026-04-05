using System.Runtime.CompilerServices;
using ParadoxPower.Process;
using ZLinq;
using ZLinq.Linq;

namespace ParadoxPower.ZLinq;

public static class LinqExtensions
{
    extension(Node node)
    {
        public ValueEnumerable<ArrayWhereSelect<Child, Node>, Node> NodesValue => node
            .AllArray.AsValueEnumerable()
            .Where(static x => x.IsNodeChild)
            .Select(static x => Unsafe.As<Child.NodeChild>(x).Item);

        public ValueEnumerable<ArrayWhereSelect<Child, Leaf>, Leaf> LeavesValue =>
            node
                .AllArray.AsValueEnumerable()
                .Where(static x => x.IsLeafChild)
                .Select(static x => Unsafe.As<Child.LeafChild>(x).Item);

        public ValueEnumerable<ArrayWhereSelect<Child, LeafValue>, LeafValue> LeafValuesValue => node
            .AllArray.AsValueEnumerable()
            .Where(static x => x.IsLeafValueChild)
            .Select(static x => Unsafe.As<Child.LeafValueChild>(x).Item);

        public ValueEnumerable<ArrayWhereSelect<Child, Comment>, Comment> CommentsValue =>
            node
                .AllArray.AsValueEnumerable()
                .Where(static x => x.IsCommentChild)
                .Select(static x => Unsafe.As<Child.CommentChild>(x).Item);
    }
}
