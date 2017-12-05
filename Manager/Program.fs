// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SnmpMib.Parsing
open System.Globalization
open SnmpMib.Types
open SnmpMib.Tree
open System.Collections.Generic
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv = 
  let emph str = printf "################################ %s ################################\n" str
        
  let parsingResult = parseToTree "RFC1213-MIB"
  emph "Final tree:"
  printMibTree parsingResult.Tree   

  System.Console.Read() |> ignore
  0
