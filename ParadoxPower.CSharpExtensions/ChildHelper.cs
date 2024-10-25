﻿using ParadoxPower.Parser;
using ParadoxPower.Process;

namespace ParadoxPower.CSharpExtensions;

public static class ChildHelper
{
    public static Child Leaf(string key, int value, Types.Operator op = Types.Operator.Equals)
    {
        return Child.NewLeafChild(new Leaf(key, Types.Value.NewInt(value), op));
    }
    
    public static Child Leaf(string key, bool value, Types.Operator op = Types.Operator.Equals)
    {
        return Child.NewLeafChild(new Leaf(key, Types.Value.NewBool(value), op));
    }
    
    public static Child Leaf(string key, decimal value, Types.Operator op = Types.Operator.Equals)
    {
        return Child.NewLeafChild(new Leaf(key, Types.Value.NewFloat(value), op));
    }

    public static Child LeafString(string key, string value, Types.Operator op = Types.Operator.Equals)
    {
        return Child.NewLeafChild(new Leaf(key, Types.Value.NewStringValue(value), op));
    }
    
    public static Child LeafQString(string key, string value, Types.Operator op = Types.Operator.Equals)
    {
        return Child.NewLeafChild(new Leaf(key, Types.Value.NewQStringValue(value), op));
    }
    
    public static Child Node(string key)
    {
        return Child.NewNodeChild(new Node(key));
    }
}