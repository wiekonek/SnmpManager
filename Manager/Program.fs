open SnmpMib.Parsing
open SnmpMib.Tree

[<EntryPoint>]
let main argv = 
  
  let tree = parseToTree "RFC1213-MIB"
  printMibTree tree

  System.Console.Read() |> ignore
  0
