namespace SnmpMib


module Tree = 
  open Types
  open System.Collections.Generic

  type MibObject =
    | Oid of ObjectIdentifier
    | Object of ObjectType
   
  type MibNode = Node of MibObject * List<MibNode>

  type MibTree = {
    Root: MibNode
    DataTypes: Dictionary<string, DataType>
  }
  
  let bfs (tree: MibTree) (oidName: string) =
    let mutable list = []
    let rec bfsRec node oidName: unit =
      let (Node (mibObject, children)) = node
      match mibObject with
      | Oid oid ->
        if oid.Name = oidName  then list <- node :: list
        for child in children do 
          bfsRec child oidName
      | Object obj ->
        if obj.Name = oidName  then list <- node :: list
        for child in children do 
          bfsRec child oidName
    bfsRec tree.Root oidName
    list

  let withChildren treeNode f =
    let (Node (mibObject, children)) = treeNode
    f children
      

  let printMibTree (tree: MibTree) =
    let rec printTree (tNode: MibNode) lvl =
      let space = new string [| for _ in 0..lvl -> '|'|]
      let (Node (mibObject, children)) = tNode
      match mibObject with
      | Oid oid -> 
        printf "%s├oid [%i] %s\n" space (oid.OidPart.Value) oid.Name
        for node in children do
          printTree node (lvl+1)
      | Object obj ->
        printf "%s├obj [%i] %s\n" space obj.OidPart obj.Name
        //printf "%s|    Access: %s\n" space (obj.Access.ToString())
        //printf "%s|    Status: %s\n" space (obj.Status.ToString())
        //printf "%s|    Syntax: %s\n" space (obj.Syntax.ToString())
        //printf "%s     Description: %s" space (obj.Description)
        for node in children do
          printTree node (lvl+1)
    printTree tree.Root -1



