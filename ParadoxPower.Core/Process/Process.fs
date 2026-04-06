namespace ParadoxPower.Process

open System
open System.Collections.Generic
open System.Diagnostics
open ParadoxPower.Common
open ParadoxPower.Parser
open ParadoxPower.Utilities.Position
open ParadoxPower.Utilities.Utils

module List =
    let replace f sub xs =
        let rec finish acc =
            function
            | [] -> acc
            | x :: xs -> finish (x :: acc) xs

        let rec search acc =
            function
            | [] -> None
            | x :: xs ->
                if f x then
                    Some(finish ((sub x) :: xs) acc)
                else
                    search (x :: acc) xs

        search [] xs

    let replaceOrAdd f sub add xs =
        let result = replace f sub xs

        match result with
        | Some ls -> ls
        | None -> add :: xs

type IKeyPos =
    abstract member Key: string
    abstract member Position: Range

type IClause =
    inherit IKeyPos
    abstract member Nodes: Node seq
    abstract member LeafValues: LeafValue seq
    abstract member Leaves: Leaf seq
    abstract member ValueClauses: ValueClause seq
    abstract member AllArray: Child array
    abstract member Clauses: IClause seq
    abstract member ClauseList: IClause list
    abstract member Tag: string -> Value option
    abstract member TagText: string -> string

and [<DebuggerDisplay("{Key}={ValueText}"); Sealed>] Leaf =
    val mutable Value: Value
    val mutable Key: string
    val mutable Position: Range
    val mutable Parent: Node|null
    val mutable Operator: Operator

    member this.ValueText = this.Value.ToRawString()
    static member op_Implicit(leaf: Leaf) : Child =
        Child.LeafChild leaf

    member this.ToRaw =
        KeyValue(PosKeyValue(this.Position, KeyValueItem(Key(this.Key), this.Value, this.Operator)))

    override this.ToString() = $"{this.Key}={this.ValueText}"

    new(key: string, value: Value, pos: Range, op: Operator, parent: Node|null) =
        { Key = key
          Value = value
          Position = pos
          Operator = op
          Parent = parent }

    new(key: string, value: Value, op: Operator) = Leaf(key, value, Range.Zero, op, null)

    new(keyvalueitem: KeyValueItem, ?pos: Range) =
        let (KeyValueItem(Key(key), value, op)) = keyvalueitem
        Leaf(key, value, pos |> Option.defaultValue Range.Zero, op, null)

    static member Create key value = LeafChild(Leaf(key, value))

    interface IKeyPos with
        member this.Key = this.Key
        member this.Position = this.Position

and [<DebuggerDisplay("{Key}"); Sealed>] LeafValue(value: Value, ?pos: Range) =
    member val Value = value with get, set

    member this.ValueText = this.Value.ToRawString()

    member this.Key = this.ValueText

    override this.ToString() = this.Key

    member val Position = defaultArg pos Range.Zero
    member this.ToRaw = Value(this.Position, this.Value)
    static member Create value = LeafValue value
    static member op_Implicit(leafValue: LeafValue) : Child =
        Child.LeafValueChild leafValue

    interface IKeyPos with
        member this.Key = this.Key
        member this.Position = this.Position

and Child =
    // 方便测试代码
    #if RELEASE
    internal
    #endif
    | NodeChild of Node
    | LeafChild of Leaf
    | CommentChild of Comment
    | LeafValueChild of LeafValue
    | ValueClauseChild of ValueClause

    static member Create node = NodeChild node
    static member Create leaf = LeafChild leaf
    static member Create comment = CommentChild comment
    static member Create leafValue = LeafValueChild leafValue
    static member Create valueClause = ValueClauseChild valueClause

    member this.Position =
        match this with
        | NodeChild n -> n.Position
        | LeafChild l -> l.Position
        | CommentChild c -> c.Position
        | LeafValueChild lv -> lv.Position
        | ValueClauseChild vc -> vc.Position

and [<Sealed>] ValueClause(keys: Value[], pos: Range) =
    let mutable _keys: string array = keys |> Array.map (fun v -> v.ToString())

    let bothFind (x: string) =
        function
        | NodeChild n when n.Key == x -> true
        | LeafChild l when l.Key == x -> true
        | _ -> false

    let mutable all: Child array = Array.empty

    new() = ValueClause([||], Range.Zero)
    
    static member op_Implicit(valueClause: ValueClause) : Child =
        Child.ValueClauseChild valueClause

    member val Position = pos
    member val Scope: Scope = scopeManager.AnyScope with get, set
    member _.AllChildren = all |> ResizeArray<Child>

    member _.AllChildren
        with set (value: ResizeArray<Child>) =
            all <- (value |> Seq.toArray)

    member _.AllArray = all

    member _.AllArray
        with set value =
            all <- value

    member this.All = all |> List.ofSeq

    member this.All
        with set (value: Child list) =
            all <- (value |> List.toArray)

    member this.Nodes =
        all
        |> Seq.choose (function
            | NodeChild n -> Some n
            | _ -> None)

    member this.Children = this.Nodes |> List.ofSeq

    member this.Leaves =
        all
        |> Seq.choose (function
            | LeafChild l -> Some l
            | _ -> None)

    member this.Values = this.Leaves |> List.ofSeq

    member this.Comments =
        all
        |> Seq.choose (function
            | CommentChild c -> Some c
            | _ -> None)

    member this.LeafValues =
        all
        |> Seq.choose (function
            | LeafValueChild lv -> Some lv
            | _ -> None)

    member this.ValueClauses =
        all
        |> Seq.choose (function
            | ValueClauseChild vc -> Some vc
            | _ -> None)

    member this.Clauses =
        all
        |> Seq.choose (function
            | ValueClauseChild vc -> Some(vc :> IClause)
            | NodeChild n -> Some(n :> IClause)
            | _ -> None)

    member this.Has x = all |> (Seq.exists (bothFind x))

    member this.Tag x =
        this.Leaves
        |> Seq.tryPick (function
            | l when l.Key == x -> Some l.Value
            | _ -> None)

    member this.Leafs x =
        this.Leaves
        |> Seq.choose (function
            | l when l.Key == x -> Some l
            | _ -> None)

    member this.Tags x =
        this.Leaves
        |> Seq.choose (function
            | l when l.Key == x -> Some l.Value
            | _ -> None)
        
    member this.TagText x =
        this.Tag x
        |> function
            | Some(QString s) -> s
            | Some s -> s.ToString()
            | None -> ""

    member this.TagsText x =
        this.Tags x
        |> Seq.map (function
            | QString s -> s
            | s -> s.ToString())

    member this.SetTag x v =
        this.All <- this.AllChildren |> List.ofSeq |> List.replaceOrAdd (bothFind x) (fun _ -> v) v

    member this.Child x =
        this.Nodes
        |> Seq.tryPick (function
            | c when c.Key == x -> Some c
            | _ -> None)

    member this.Childs x =
        this.Nodes
        |> Seq.choose (function
            | c when c.Key == x -> Some c
            | _ -> None)

    member this.FirstKey = if _keys.Length > 0 then Some(_keys[0]) else None

    member this.FirstKeyId = if _keys.Length > 0 then Some _keys[0] else None

    member this.SecondKey = if _keys.Length > 0 then Some(_keys[1]) else None

    member _.Keys = _keys

    member _.Keys
        with set value = _keys <- value

    member this.ToRaw: Statement list =
        this.All
        |> List.collect (function
            | NodeChild n -> [ n.ToRaw ]
            | LeafValueChild lv -> [ lv.ToRaw ]
            | LeafChild l -> [ l.ToRaw ]
            | ValueClauseChild vc ->
                let keys =
                    vc.Keys
                    |> Array.map (fun k -> Value(Range.Zero, Value.String(k)))
                    |> List.ofArray

                keys @ [ Value(vc.Position, Value.Clause vc.ToRaw) ]
            | CommentChild({ Position = r; Comment = c }) -> [ (CommentStatement({ Position = r; Comment = c })) ])

    static member Create() = ValueClause()

    interface IKeyPos with
        member this.Key = this.FirstKey |> Option.defaultValue "clause"

        member this.Position = this.Position

    interface IClause with
        member this.Nodes = this.Nodes
        member this.Leaves = this.Leaves
        member this.LeafValues = this.LeafValues
        member this.ValueClauses = this.ValueClauses
        member this.AllArray = this.AllArray
        member this.Clauses = this.Clauses
        member this.ClauseList = this.Clauses |> List.ofSeq
        member this.TagText x = this.TagText x
        member this.Tag x = this.Tag x

and [<DebuggerDisplay("{Key}"); Sealed>] Node(key: string, pos: Range) =
    let bothFind (key: string) =
        function
        | NodeChild n when n.Key == key -> true
        | LeafChild l when l.Key == key -> true
        | _ -> false

    let mutable all: Child array = Array.empty

    new(key: string) = Node(key, Range.Zero)
    static member op_Implicit(node: Node) : Child =
        Child.NodeChild node

    member val Key: string = key

    member val Position = pos
    member val Parent: Node|null = null with get, set
    member internal _.AllChildren = all |> ResizeArray<Child>

    member internal _.AllChildren
        with set (value: ResizeArray<Child>) =
            all <- (value |> Seq.toArray)

    /// 返回包含所有子元素的底层数组
    member _.AllArray = all

    member _.AllArray
        with set value =
            all <- value

    member internal this.All = all |> List.ofSeq

    member internal this.All
        with set (value: Child list) =
            all <- (value |> List.toArray)

    member this.Nodes: Node seq =
        seq {
            for child in all do
                match child with
                | NodeChild n -> yield n
                | _ -> ()
        }

    member this.Leaves: Leaf seq = 
        seq {
            for child in all do
                match child with
                | LeafChild leaf -> yield leaf
                | _ -> ()
        }

    member this.Comments: Comment seq =
        seq {
            for child in all do
                match child with
                | CommentChild n -> yield n
                | _ -> ()
        }

    member this.LeafValues: LeafValue seq =
        seq {
            for child in all do
                match child with
                | LeafValueChild n -> yield n
                | _ -> ()
        }

    member this.ValueClauses: ValueClause seq =
        seq {
            for child in all do
                match child with
                | ValueClauseChild n -> yield n
                | _ -> ()
        }

    member this.Clauses: IClause seq =
        seq {
            for child in all do
                match child with
                | ValueClauseChild vc -> yield vc :> IClause
                | NodeChild n -> yield n :> IClause
                | _ -> ()
        }

    member this.Has key = all |> (Array.exists (bothFind key))

    member this.Tag key =
        this.Leaves
        |> Seq.tryPick (function
            | l when l.Key == key -> Some l.Value
            | _ -> None)

    member this.GetLeaves key =
        this.Leaves
        |> Seq.choose (function
            | l when l.Key == key -> Some l
            | _ -> None)

    member this.GetLeaf(key: string) =
        this.Leaves |> Seq.tryFind (fun (item: Leaf) -> item.Key == key)

    /// 获取拥有指定 key 的所有 Leaf 的值
    member this.Tags key =
        this.Leaves
        |> Seq.choose (function
            | l when l.Key == key -> Some l.Value
            | _ -> None)

    member this.TagText key =
        this.Tag key
        |> function
            | Some(QString s) -> s
            | Some s -> s.ToString()
            | None -> ""

    member this.TagsText key =
        this.Tags key
        |> Seq.map (function
            | QString s -> s
            | s -> s.ToString())

    member this.ReplaceOrAdd key value =
        this.All <- all |> List.ofArray |> List.replaceOrAdd (bothFind (key)) (fun _ -> value) value

    /// <summary>
    /// 设置拥有指定<c>key</c>的第一个<c>value</c>
    /// </summary>
    member this.SetValueFirst (key: string) (value: Child) =
        match Array.tryFindIndex (bothFind (key)) this.AllArray with
        | Some index ->
            Array.set this.AllArray index value
        | None -> ()

    member this.SetValues key value =
        for i in 0 .. this.AllArray.Length - 1 do
            if bothFind (key) (this.AllArray[i]) then
                Array.set this.AllArray i value

    /// <summary>
    /// 便捷设置 <see cref="Leaf"/>, 使用 '=' 作为 <see cref="Operator"/>, 如果<c>key</c>不存在则添加
    /// </summary>
    member this.SetLeafValue key leafValue =
        this.All <-
            let leaf =
                Child.LeafChild(Leaf(key, Value.String(leafValue), Range.Zero, Operator.Equals, this))

            all |> List.ofSeq |> List.replaceOrAdd (bothFind key) (fun _ -> leaf) leaf

    member this.AddChild(child: Child) =
        let newArray = Array.zeroCreate (all.Length + 1)
        Array.Copy(all, newArray, all.Length)
        newArray[newArray.Length - 1] <- child
        all <- newArray

    member this.AddChild(child: Node) = this.AddChild(Child.NodeChild(child))

    member this.AddChild(child: Comment) =
        this.AddChild(Child.CommentChild(child))

    member this.AddChild(child: Leaf) = this.AddChild(Child.LeafChild(child))

    member this.AddChild(child: LeafValue) =
        this.AddChild(Child.LeafValueChild(child))

    member this.AddChild(child: ValueClause) =
        this.AddChild(Child.ValueClauseChild(child))

    member this.AddChildren(children: Child IReadOnlyList) =
        let newArray = Array.zeroCreate (all.Length + children.Count)
        Array.Copy(all, newArray, all.Length)

        for i in 0 .. children.Count - 1 do
            newArray[all.Length + i] <- children[i]

        this.AllArray <- newArray

    member this.AddChildren(children: Child seq) =
        match children with
        | :? IReadOnlyList<Child> as children -> this.AddChildren(children)
        | _ ->
            let resizeArray = this.AllChildren
            resizeArray.AddRange(children)
            this.AllArray <- resizeArray.ToArray()

    member this.GetChild(key: string) =
        Seq.tryPick
            (function
            | (item: Node) when item.Key == key -> Some item
            | _ -> None)
            this.Nodes

    /// 获取拥有指定 key 的 Node, 大小写不敏感
    member this.GetChildren key =
        this.Nodes
        |> Seq.choose (function
            | c when c.Key == key -> Some c
            | _ -> None)

    member this.ToRaw: Statement =
        let children = ResizeArray<Statement>(this.AllArray.Length)

        for child in this.AllArray do
            match child with
            | CommentChild c -> children.Add(CommentStatement c)
            | NodeChild n -> children.Add(n.ToRaw)
            | LeafValueChild lv -> children.Add(lv.ToRaw)
            | LeafChild l -> children.Add(l.ToRaw)
            | ValueClauseChild vc ->
                let keys = vc.Keys |> Array.map (fun k -> Value(Range.Zero, Value.String(k)))
                children.AddRange(keys)
                children.Add(Value(vc.Position, Value.Clause vc.ToRaw))

        KeyValue(PosKeyValue(this.Position, KeyValueItem(Key this.Key, Clause(List.ofSeq children), Operator.Equals)))

    /// 深克隆节点, 包括所有 Child, 但不包括 Parent 引用
    member this.Clone() : Node =
            let rec mapChild parent =
                function
                | NodeChild node ->
                    let newNode = Node(node.Key, node.Position)
                    newNode.Parent <- parent
                    newNode.AllArray <- node.AllArray |> Array.map (mapChild node)
                    NodeChild newNode
                | LeafChild l -> LeafChild(Leaf(l.Key, l.Value, l.Position, l.Operator, parent))
                | LeafValueChild lv -> LeafValueChild(LeafValue(lv.Value, lv.Position))
                | ValueClauseChild vc ->
                    let newVC = ValueClause([||], vc.Position)
                    // TODO: 如果以后 ValueClause 添加了 Parent 属性, 这里需要修改为传入 vc 而不是 parent
                    newVC.AllArray <- vc.AllArray |> Array.map (mapChild parent)
                    newVC.Keys <- vc.Keys
                    ValueClauseChild newVC
                | CommentChild c -> CommentChild c

            let newNode = Node(key, pos)
            newNode.AllArray <- all |> Array.map (mapChild newNode)
            newNode
    
    static member Create key = Node(key)

    interface IKeyPos with
        member this.Key = this.Key
        member this.Position = this.Position

    interface IClause with
        member this.Nodes = this.Nodes
        member this.Leaves = this.Leaves
        member this.LeafValues = this.LeafValues
        member this.ValueClauses = this.ValueClauses
        member this.AllArray = this.AllArray
        member this.Clauses = this.Clauses
        member this.ClauseList = this.Clauses |> List.ofSeq
        member this.TagText x = this.TagText x
        member this.Tag x = this.Tag x
   

module ProcessCore =

    let processNode (postinit: Node -> Node) inner (key: string) (pos: Range) (sl: Statement list) : Node =
        let node = Node(key, pos) |> postinit
        let children = sl |> List.map inner
        node.All <- children
        node

    type LookupContext =
        { complete: bool
          parents: string list
          scope: string
          previous: string
          entityType: EntityType }

    let processNodeSimple _ = processNode id

    type BaseProcess() =
        let rec nodeWindowFun context (backtwo: Statement option, backone: Statement option, acc) (next: Statement) =
            //eprintfn "%A %A %A" backtwo backone next
            match backtwo, backone, next with
            | Some(Value(_, Clause _)), _, _
            | Some(Value _), Some(KeyValue(PosKeyValue(_, KeyValueItem(Key _, Clause _, _)))), Value(_, Clause _)
            | _, Some(Value(_, Clause _)), _ -> backone, Some next, (processNodeInner context next) :: acc
            | Some(Value(_, v2)), Some(Value(_, v1)), Value(pos, Clause sl) ->
                None, None, (lookupValueClause pos context sl [| v2; v1 |]) :: (acc |> List.skip 2)
            | Some(Value(_, v2)), Some(KeyValue(PosKeyValue(_, KeyValueItem(Key(k), v1, _)))), Value(pos, Clause sl) ->
                let node: Node = lookupNode k pos context sl
                None, None, (NodeChild node) :: (acc |> List.skip 2)
            | _, Some(Value(pos, v2)), KeyValue(PosKeyValue(pos2, KeyValueItem(Key(k), Clause sl, _))) when
                pos.StartLine = pos2.StartLine
                ->
                let node = lookupNode k pos2 context sl
                None, None, (NodeChild node) :: (acc |> List.skip 1)
            | _ -> backone, Some next, (processNodeInner context next) :: acc

        and lookupNode =
            (fun (key: string) (pos: Range) (context: LookupContext) (sl: Statement list) ->
                let node = Node(key, pos)

                let children =
                    sl
                    |> List.fold (nodeWindowFun context) (None, None, [])
                    |> (fun (_, _, ls) -> ls |> List.rev)

                for child in children do
                    match child with
                    | NodeChild nodeChild -> nodeChild.Parent <- node
                    | LeafChild leafChild -> leafChild.Parent <- node
                    | _ -> ()

                node.All <- children
                node)

        and lookupValueClause =
            (fun (pos: Range) (context: LookupContext) (sl: Statement list) keys ->
                let vc = ValueClause(keys, pos)

                let children =
                    sl
                    |> List.fold (nodeWindowFun context) (None, None, [])
                    |> (fun (_, _, ls) -> ls |> List.rev)

                vc.All <- children
                ValueClauseChild vc)

        and processNodeInner (c: LookupContext) statement =
            // printfn "%s" (System.String.Join(", ", c.parents))
            match statement with
            | KeyValue(PosKeyValue(pos, KeyValueItem(Key(k), Clause(sl), _))) -> NodeChild(lookupNode k pos c sl)
            | KeyValue(PosKeyValue(pos, kv)) -> LeafChild(Leaf(kv, pos))
            | CommentStatement({ Position = r; Comment = c }) -> CommentChild({ Position = r; Comment = c })
            | Value(pos, Value.Clause sl) -> lookupValueClause pos c sl [||]
            | Value(pos, v) -> LeafValueChild(LeafValue(v, pos))

        member _.ProcessNode() =
            fun key pos ->
                lookupNode
                    key
                    pos
                    { complete = false
                      parents = []
                      scope = ""
                      previous = ""
                      entityType = EntityType.Other }

        member _.ProcessNode(entityType: EntityType) =
            fun key pos ->
                lookupNode
                    key
                    pos
                    { complete = false
                      parents = []
                      scope = ""
                      previous = ""
                      entityType = entityType }

    let simpleProcess = BaseProcess()
