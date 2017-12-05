namespace SnmpMib


module Tree = 
  open Types
  open System.Collections.Generic
  
   
  type MibTree = 
    | OidNode of ObjectIdentifier * List<MibTree>
    | ObjectNode of ObjectType * List<MibTree>
  
  let bfs (tree: MibTree) (oidName: string) =
    let mutable list = []
    let rec bfsRec t oidName: unit =
      match t with
      | OidNode (oid, children) -> 
        if oid.Name = oidName  then list <- t :: list
        for child in children do 
          bfsRec child oidName
      | ObjectNode (obj, children) -> 
        if obj.Name = oidName  then list <- t :: list
        for child in children do 
          bfsRec child oidName
    
    bfsRec tree oidName
    list

  let withChildren tree f =
    match tree with
    | OidNode (_, children)  -> f children 
    | ObjectNode (_, children)  -> f children
      

  let printMibTree (tree: MibTree) =
    let rec printTree (tree: MibTree) lvl =
      let space = new string [| for _ in 0..lvl -> '|'|]
      match tree with
      | OidNode (oid, t) -> 
        printf "%s├oid [%i] %s\n" space (oid.OidPart.Value) oid.Name
        for node in t do
          printTree node (lvl+1)
      | ObjectNode (obj, t) ->
        printf "%s├obj [%i] %s\n" space obj.OidPart obj.Name
        //printf "%s|    Access: %s\n" space (obj.Access.ToString())
        //printf "%s|    Status: %s\n" space (obj.Status.ToString())
        //printf "%s|    Syntax: %s\n" space (obj.Syntax.ToString())
        //printf "%s     Description: %s" space (obj.Description)
        for node in t do
          printTree node (lvl+1)
    printTree tree -1



