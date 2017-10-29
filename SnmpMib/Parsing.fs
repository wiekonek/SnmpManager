namespace SnmpMib

open System.IO
open System.Text.RegularExpressions
open System
open Types


module Parsing =
  open Microsoft.FSharp.Core.LanguagePrimitives


  let defaultPath = @"C:\\Users\wieko\Documents\mibs\"

  type RawImports = {
    Imports: string;
    FileName: string;
  }

  type RawDataType = {
    Name: string;
    Application: int option;
    Implicit: bool option;
    Definition: string;
  }

  type RawObjectIdentifier = {
    Name: string;
    Oid: string;
  }
    
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

  // remove comments: --.* 
  let rawFileWithoutComments filePath =
    Regex.Replace(File.ReadAllText(filePath), @"--.*", "")

  let testFile =
    rawFileWithoutComments (defaultPath + "RFC1213-MIB")

  // Imports
  // ^[ ]?IMPORTS.*?;
  // ^[ ]+(?'imports'[\w\s, -]+?)\s+FROM (?'file_name'.*?)[\n;]
  let fileImports fileContent = 
    let matches = Regex.Matches(fileContent, @"^[ ]+(?'imports'[\w\s, -]+?)\s+FROM (?'file_name'.*?)[\n;]", RegexOptions.Multiline)
    matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> { Imports = m.Groups.["imports"].Value; FileName = m.Groups.["file_name"].Value})

  // Data Types \gsm
  // ^[ ]*?(?'name'[\w-]+)? ::=$\s+(\[APPLICATION (?'application'\d?)\])?\s+(?'impllicit'IMPLICIT)?\s+?((CHOICE {\s+(?'choice'.*?)\s+})|(SEQUENCE {\s+(?'sequence'.*?)\s+})|(?'definition'.*?))\s+^$
  // https://regex101.com/r/yrXWzs/5
  // for definitions: \gsm
  // https://regex101.com/r/p8Ope4/1
  // (?'type'INTEGER|OCTET STRING|OBJECT IDENTIFIER|NULL)\s+?((\(((SIZE \((?'size'\d+)\))|(?'range'[-\d]+\.\.[-\d]+))\))|({\s+(?'enum'.*?)\s+}))?
  //
  // ^[ ]*?(?'name'[\w-]+)? ::=$\s+(\[APPLICATION (?'application'\d?)\])?\s+(?'impllicit'IMPLICIT)?\s+?((?'definition'.*?))\s+^$
  // https://regex101.com/r/yrXWzs/6
  // for definitons or syntax???:
  // (CHOICE {\s+(?'choice'.*?)\s+})|(SEQUENCE {\s+(?'sequence'.*?)\s+})|(?'type'INTEGER|OCTET STRING|OBJECT IDENTIFIER|NULL)\s+?((\(((SIZE \((?'size'\d+)\))|(?'range'[-\d]+\.\.[-\d]+))\))|({\s+(?'enum'.*?)\s+}))?

  let fileDataTypes fileContent =
    let toRawDataType (m: Match) =
      {
        Name = m.Groups.["name"].Value;
        Application = match m.Groups.Item("application").Success with
                      | true -> Some (ParseInt32 (m.Groups.Item("application").Value))
                      | false -> None;
        Implicit = match m.Groups.Item("implicit").Success with
                      | true -> Some (m.Groups.Item("implicit").Value = "IMPLICIT")
                      | false -> None;
        Definition = m.Groups.["definition"].Value;
      }
    let matches = Regex.Matches(fileContent,
                                @"^[ ]*?(?'name'[\w-]+)? ::=$\s+(\[APPLICATION (?'application'\d?)\])?\s+(?'implicit'IMPLICIT)?\s+?((?'definition'.*?))\s+^$", 
                                RegexOptions.Multiline ||| RegexOptions.Singleline)
    matches
    |> Seq.cast<Match>
    |> Seq.map toRawDataType
 

  // ObjectIdentifiers
  // https://regex101.com/r/yrXWzs/1
  // ^(?'name'[\w-]+)\s*OBJECT IDENTIFIER ::= { (?'oid'.*) }
  let fileObjectIdentifiers fileContent =
    let matches = Regex.Matches(fileContent, @"^(?'name'[\w-]+)\s*OBJECT IDENTIFIER ::= { (?'oid'.*) }",  RegexOptions.Multiline)
    matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> { Name = m.Groups.["name"].Value; Oid = m.Groups.["oid"].Value})
  
  // Objects
  let fileObjectTypes fileContent =
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
    let matches = Regex.Matches(
                    fileContent,
                    @"^(?'obj'[\w-]+) OBJECT-TYPE$\s+SYNTAX\s+(?'syntax'.*?)$\s+ACCESS\s+(?'access'.*?)$\s+STATUS\s+(?'status'.*?)$\s+DESCRIPTION\s+\""(?'description'.*?)\""\s+(INDEX\s+{ (?'index'.*?) }\s+)?(REFERENCE\s+\""(?'reference'.*?)\""\s+)?(DEFVAL\s+{ (?'defval'.*?) }\s+)?::= { (?'oid'.+?) }",
                    RegexOptions.Multiline ||| RegexOptions.Singleline) 
    matches
    |> Seq.cast<Match>
    |> Seq.map toRawObjectType

  type Parser(mibsPath) =
    member private this.mibsPath = mibsPath

    member this.ParseFile(fileName) =
      ()