namespace SnmpMib

open System.IO
open System.Text.RegularExpressions
open System

module Parsing =

  let private defaultPath = @"C:\\Users\wieko\Documents\mibs\"

  [<Struct>]
  type RawObjectIdentifier = {
    Name: string;
    Oid: string;
  }

  [<Struct>]
  type RawObjectType = {
    Name: string;
    Syntax: string;
    Access: string;
    Status: string;
    Description: string;
    Index: string option;
    Reference: string option;
    Defval: string option;
    Oid: string;
  }

  let private getOid obj =
    obj.Oid

  let private fileObjectIdentifiers filePath =
    let fileContent = File.ReadAllText(filePath)
    let matches = Regex.Matches(fileContent, @"(?m)^(?'name'[\w-]+)\s*OBJECT IDENTIFIER ::= { (?'oid'.*) }")
    matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> { Name = m.Groups.["name"].Value; Oid = m.Groups.["oid"].Value})

  let private fileObjectTypes filePath =
    let toRawObjectType (m: Match) =
      { 
        Name = m.Groups.["obj"].Value;
        Syntax = m.Groups.["syntax"].Value;
        Access = m.Groups.["access"].Value;
        Status = m.Groups.["status"].Value;
        Description = m.Groups.["description"].Value;
        Index = None(*m.Groups.Item("index")*);
        Reference = None(*m.Groups.Item("index")*);
        Defval = None(*m.Groups.Item("index")*);
        Oid = m.Groups.["oid"].Value;
      }
    let fileContetn = File.ReadAllText(filePath)
    let matches = Regex.Matches(
                    fileContetn,
                    @"^(?'obj'[\w-]+) OBJECT-TYPE$\s+SYNTAX\s+(?'syntax'.*?)$\s+ACCESS\s+(?'access'.*?)$\s+STATUS\s+(?'status'.*?)$\s+DESCRIPTION\s+\""(?'description'.*?)\""\s+(INDEX\s+{ (?'index'.*?) }\s+)?(REFERENCE\s+\""(?'reference'.*?)\""\s+)?(DEFVAL\s+{ (?'defval'.*?) }\s+)?::= { (?'oid'.+?) }",
                    RegexOptions.Multiline ||| RegexOptions.Singleline) 
    matches
    |> Seq.cast<Match>
    |> Seq.map toRawObjectType

  type Parser(mibsPath) =
    member private this.mibsPath = mibsPath

    member public this.Test() =
      printf "Hello"