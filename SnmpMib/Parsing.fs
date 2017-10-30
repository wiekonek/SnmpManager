namespace SnmpMib

open System.IO
open System.Text.RegularExpressions
open System
open Types


module Parsing =
  open Microsoft.FSharp.Core.LanguagePrimitives
  open System.Collections.Generic

  let defaultPath = @"C:\\Users\wieko\Documents\mibs\"

  // remove comments: --.* 
  let rawFileWithoutComments filePath =
    Regex.Replace(File.ReadAllText(filePath), @"--.*", "")

  let testFile =
    rawFileWithoutComments (defaultPath + "RFC1213-MIB")

  let testFile2 =
    rawFileWithoutComments (defaultPath + "RFC1381-MIB")

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
  // (CHOICE {\s+(?'choice'.*?)\s+})|(SEQUENCE {\s+(?'sequence'.*?)\s+})|(?'type'INTEGER|OCTET STRING|OBJECT IDENTIFIER|NULL)\s?((\(((SIZE \((?'size'\d+)\))|(?'range'[-\d]+\.\.[-\d]+))\))|({\s+(?'enum'.*?)\s+}))?

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
        RawDefinition = m.Groups.["definition"].Value;
      }
    let matches = Regex.Matches(fileContent,
                                @"^[ ]*?(?'name'[\w-]+)? ::=$\s+(\[APPLICATION (?'application'\d?)\])?\s+(?'implicit'IMPLICIT)?\s+?((?'definition'.*?))\s+^$", 
                                RegexOptions.Multiline ||| RegexOptions.Singleline)
    matches
    |> Seq.cast<Match>
    |> Seq.map toRawDataType

  let parseDictionary str =
    let matches = Regex.Matches(str, "(?'key'\w+)\s+(?'value'[\w\s]+)")
    let pairs = new Dictionary<string, string>()
    for pairMatch in matches do
      pairs.Add(pairMatch.Groups.Item("key").Value, pairMatch.Groups.Item("value").Value)
    pairs

  let parseEnum (str: string) =
    let matches = Regex.Matches(str, "(?'name'[\w]+)\((?'value'[\d+])\)")
    let enumDefs = new Dictionary<string, int>()
    for pairMatch in matches do
      enumDefs.Add(pairMatch.Groups.Item("name").Value, pairMatch.Groups.Item("value").Value |> ParseInt32)
    enumDefs

  let parseRange (str: string) =
    let rangeArr = str.Replace("..", " ").Split(' ')
    { Min = ParseInt32 rangeArr.[0]; Max = ParseInt32 rangeArr.[1] }
  
  let parseRawDataType (raw: RawDataType) =
    let parseRawDefinition rawDefinition =

      let regex input pattern =
        let m = Regex.Match(input, pattern, RegexOptions.Multiline ||| RegexOptions.Singleline)
        if m.Success then Some m else None

      let (|ChoiceMatch|_|) input =
        regex input @"CHOICE {\s+(?'choice'.*?)\s+}"
      let (|SequenceMatch|_|) input =
        regex input @"SEQUENCE {\s+(?'sequence'.*?)\s+}"
      let (|SequenceOfMatch|_|) input =
        regex input @"SEQUENCE OF (?'sequence_of'\w+)"

      match rawDefinition with
      | ChoiceMatch m -> DataTypeDefinition.Choice { Values = parseDictionary (m.Groups.Item("choice").Value) }
      | SequenceMatch m -> DataTypeDefinition.Sequence { Values = parseDictionary (m.Groups.Item("sequence").Value) }
      | SequenceOfMatch m -> DataTypeDefinition.SequenceOfSimpleTypes (m.Groups.Item("sequence_of").Value)
      | _ ->
        let regMatch = Regex.Match(rawDefinition,
                                 @"(?'type'INTEGER|OCTET STRING|OBJECT IDENTIFIER|NULL)\s?((\(((SIZE \((?'size'\d+)\))|(?'range'[-\d]+\.\.[-\d]+))\))|({\s+(?'enum'.*?)\s+}))?",
                                 RegexOptions.Singleline ||| RegexOptions.Multiline)
        Simple { 
          Type = regMatch.Groups.Item("type").Value;
          Constraint = match regMatch.Groups.Item("Constraint").Success with
                       | true ->
                          Some (match regMatch.Groups.Item("size").Success, regMatch.Groups.Item("range").Success, regMatch.Groups.Item("enum").Success with
                                | true, false, false -> Size (ParseInt32 (regMatch.Groups.Item("size").Value))
                                | false, true, false -> Range (parseRange (regMatch.Groups.Item("range").Value))
                                | false, false, true -> EnumValues (parseEnum (regMatch.Groups.Item("Constraint").Value))
                                | _, _, _ -> Unrecognized (regMatch.Groups.Item("Constraint").Value)
                               )
                       | false -> None
        }
    {
      Name = raw.Name;
      Application = raw.Application;
      Implicit = raw.Implicit;
      Definition = parseRawDefinition raw.RawDefinition
    }

  let getDataTypes fileContent =
    fileDataTypes fileContent |> Seq.map parseRawDataType

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
        //Index = match m.Groups.Item("index").Success with
        //              | true -> Some (m.Groups.Item("index").Value)
        //              | false -> None;
        Oid = m.Groups.["oid"].Value;
      }
    let matches = Regex.Matches(
                    fileContent,
                    """^(?'obj'[\w-]+) OBJECT-TYPE$\s+SYNTAX\s+(?'syntax'.*?)$\s+ACCESS\s+(?'access'.*?)$\s+STATUS\s+(?'status'.*?)$\s+DESCRIPTION\s+"(?'description'.*?)".*?\s+(INDEX\s+{ (?'index'.*?) }\s+)?::= { (?'oid'.+?) }""",
                    RegexOptions.Multiline ||| RegexOptions.Singleline) 
    matches
    |> Seq.cast<Match>
    |> Seq.map toRawObjectType

  type Parser(mibsPath) =
    member private this.mibsPath = mibsPath

    member this.ParseFile(fileName) =
      ()