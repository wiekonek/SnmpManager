namespace SnmpMib

open System.IO
open System.Text.RegularExpressions
open System
open Types


module Parsing =
  open Microsoft.FSharp.Core.LanguagePrimitives
  open System.Collections.Generic
  open Tree

  let defaultPath = @"C:\\Users\wieko\Documents\mibs\"

  // remove comments: --.* 
  let rawFileWithoutComments filePath =
    Regex.Replace(File.ReadAllText(filePath), @"--.*", "")

  let testFile =
    rawFileWithoutComments (defaultPath + "RFC1213-MIB")

  let testFile2 =
    rawFileWithoutComments (defaultPath + "RFC1155-SMI")

  // Imports
  // ^[ ]?IMPORTS.*?;
  // ^[ ]+(?'imports'[\w\s, -]+?)\s+FROM (?'file_name'.*?)[\n;]
  let fileImports fileContent = 
    let matches = Regex.Matches(fileContent, @"^[ ]+(?'imports'[\w\s, -]+?)\s+FROM (?'file_name'.*?)[\n;]", RegexOptions.Multiline)
    matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> { Imports = m.Groups.["imports"].Value; FileName = m.Groups.["file_name"].Value})
    |> Seq.filter (fun i -> i.FileName <> "RFC-1212")

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
        Visibility = 
          if not (m.Groups.Item("visibility").Success) then ContextSpecific else 
          match m.Groups.Item("visibility").Value with
          | "APPLICATION" -> Application
          | "PRIVATE" -> Private
          | "UNIVERSAL" -> Visibility.Universal
          | _ -> Visibility.Universal
        Tag = if m.Groups.Item("tag").Success then
                Some (ParseInt32 (m.Groups.Item("tag").Value))
              else
                None
        Conversion =
          if m.Groups.Item("implicit").Success then Conversion.Universal else
          match m.Groups.Item("implicit").Value with
          | "IMPLICI" -> Implicit
          | _ -> Explicit
        RawDefinition = m.Groups.["definition"].Value;
      }
    let matches = Regex.Matches(fileContent,
                                @"^[ ]*?(?'name'[\w-]+)? ::=$\s+(\[((?'visibility'(APPLICATION)|(PRIVATE)|(UNIVERSAL)) )?(?'tag'\d?)\])?\s+(?'implicit'(IMPLICIT)|(EXPLICIT))?\s+?((?'definition'.*?))\s+^$", 
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
    { Min = ParseInt64 rangeArr.[0]; Max = ParseInt64 rangeArr.[1] }

  //let parseSyntax str =
  //  let m = Regex.Match(str,
  //                      @"(?'OID'OBJECT IDENTIFIER)|(?'type'INTEGER|OCTET STRING|OBJECT IDENTIFIER|NULL)|(?'name'(\w+)).*",
  //                      RegexOptions.Singleline)
    

  
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
    let (|SimpleMatch|_|) input =
      regex input @"(?'type'INTEGER|OCTET STRING|OBJECT IDENTIFIER|NULL)\s?(?'constraint'(\(((SIZE \((?'size'\d+)\))|(?'range'[-\d]+\.\.[-\d]+))\))|({\s+(?'enum'.*?)\s+}))?"

    match rawDefinition with
    | ChoiceMatch m -> Choice { Values = parseDictionary (m.Groups.Item("choice").Value) }
    | SequenceMatch m -> Sequence { Values = parseDictionary (m.Groups.Item("sequence").Value) }
    | SequenceOfMatch m -> SequenceOfSimpleTypes (m.Groups.Item("sequence_of").Value)
    | SimpleMatch m -> 
      Simple { 
        Type = m.Groups.Item("type").Value;
        Constraint = match m.Groups.Item("constraint").Success with
                      | true ->
                        Some (match m.Groups.Item("size").Success, m.Groups.Item("range").Success, m.Groups.Item("enum").Success with
                              | true, false, false -> Size (ParseInt32 (m.Groups.Item("size").Value))
                              | false, true, false -> Range (parseRange (m.Groups.Item("range").Value))
                              | false, false, true -> EnumValues (parseEnum (m.Groups.Item("Constraint").Value))
                              | _, _, _ -> Unrecognized (m.Groups.Item("Constraint").Value)
                              )
                      | false -> None
      }
    | raw -> DataTypeReference raw

      // https://regex101.com/r/5L5QYq/1

  let parseRawDataType (raw: RawDataType) =
    {
      Name = Some raw.Name;
      Visibility = raw.Visibility;
      Conversion = raw.Conversion;
      Tag = raw.Tag; //TODO check this
      Definition = Some (parseRawDefinition raw.RawDefinition);
    }

  let getDataTypes fileContent =
    fileDataTypes fileContent |> Seq.map parseRawDataType

  // ObjectIdentifiers
  // https://regex101.com/r/yrXWzs/1
  // ^(?'name'[\w-]+)\s*OBJECT IDENTIFIER ::= { (?'oid'.*) }
  let fileObjectIdentifiers fileContent =
    let matches = Regex.Matches(fileContent, @"(?'name'[\w-]+)\s*OBJECT IDENTIFIER ::= { (?'oid'.*) }",  RegexOptions.Multiline)
    matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> { Name = m.Groups.Item("name").Value; Oid = m.Groups.Item("oid").Value})
  

  let parseRawObjectIdentifier (raw: RawObjectIdentifier) =
    let m = Regex.Match(raw.Oid, @"(?'parents'.*) (?'child'\d+)")
    let childOidPart = m.Groups.Item("child").Value;
    let matches = Regex.Matches(m.Groups.Item("parents").Value, @"(?'name'[\w-\d]+?)(\((?'number'\d+)\))?(\s|$)")

    let sequence = matches
                   |> Seq.cast<Match>
                   |> Seq.map (fun m -> { Name = m.Groups.Item("name").Value; OidPart = if m.Groups.Item("number").Success then Some (ParseInt32 (m.Groups.Item("number").Value)) else None })
    
    Seq.append sequence [{ Name = raw.Name; OidPart = Some (ParseInt32 childOidPart)}]

  let getObjectIdentifiers fileContent =
    fileObjectIdentifiers fileContent
    |> Seq.map parseRawObjectIdentifier

  // Objects
  //type Status = Mandatory | Optional | Obsolete | Unrecognized
  let parseObjectStatus statusString =
    match statusString with
    | "mandatory" -> Mandatory 
    | "optional" -> Optional 
    | "obsolete" -> Obsolete
    | _ -> Status.Unrecognized

  //  type Access = ReadOnly | ReadWrite | WriteOnly | NotAccessible | Unrecognized
  let parseObjectAccess accessString =
    match accessString with
    | "read-only" -> ReadOnly 
    | "read-write" -> ReadWrite 
    | "write-only" -> WriteOnly
    | "not-accessible" -> NotAccessible
    | _ -> Access.Unrecognized


  let fileObjectTypes fileContent =
    let toRawObjectType (m: Match) =
      { 
        Name = m.Groups.["obj"].Value;
        //Syntax = fileDataTypes (m.Groups.["syntax"].Value) |> Seq.last;
        Syntax = parseRawDefinition m.Groups.["syntax"].Value;
        Access = m.Groups.["access"].Value;
        Status = m.Groups.["status"].Value;
        Description = m.Groups.["description"].Value;
        Oid = m.Groups.["oid"].Value;
      }
    let matches = Regex.Matches(
                    fileContent,
                    """^(?'obj'[\w-]+) OBJECT-TYPE$\s+SYNTAX\s+(?'syntax'.*?)$\s+ACCESS\s+(?'access'.*?)$\s+STATUS\s+(?'status'.*?)$\s+DESCRIPTION\s+"(?'description'.*?)".*?\s+(INDEX\s+{ (?'index'.*?) }\s+)?::= { (?'oid'.+?) }""",
                    RegexOptions.Multiline ||| RegexOptions.Singleline) 
    matches
    |> Seq.cast<Match>
    |> Seq.map toRawObjectType
  
  let parseToTree fileName = 
    let rootNode: MibNode = Node ( Oid ( {Name = "iso"; OidPart = Some(1)}), new List<MibNode>())
    let dataTypesBase = new Dictionary<string, DataType>() 
    let tree: MibTree = ({ Root = rootNode; DataTypes = dataTypesBase })
    let rec insertOid (oidList: ObjectIdentifier list) treeNode =
      match oidList |> Seq.toList with
      | h :: t -> 
        let child = Node ( Oid ({ Name = h.Name; OidPart = h.OidPart }), new List<MibNode>())
        withChildren treeNode (fun (children: List<MibNode>) -> children.Add(child))
        insertOid t child
      | [] -> ()

    let definitionToDataType (def: DataTypeDefinition) =
      match def with
      | DataTypeReference ref -> 
        let m = Regex.Match(ref, @"(?'name'\w*)? ?(?'constraint'.*)?")
        let name = m.Groups.Item("name").Value
        dataTypesBase.Item(name)
      | other ->
        { Name = None; Visibility = ContextSpecific; Conversion = Explicit; Tag = None; Definition = Some other}

    let parseRawObjectType (raw: RawObjectType) =
      let oidList = parseRawObjectIdentifier { Name = raw.Name; Oid = raw.Oid} |> Seq.toList
      let objectOidPart = (oidList |> Seq.last).OidPart.Value
      match oidList with
      | parent :: child when child.Length = 1 -> 
        let fullParent = bfs tree parent.Name |> Seq.last
        let obj = {
          Name = raw.Name
          Syntax = definitionToDataType raw.Syntax
          Access = Some (parseObjectAccess raw.Access)
          Status = Some (parseObjectStatus raw.Status)
          Description = raw.Description
          OidPart = child.Item(0).OidPart.Value
        }
        withChildren fullParent (fun (children: List<MibNode>) -> children.Add( Node (Object(obj), new List<MibNode>())))
        obj
      | _ -> failwith ": /"
    let rec parseFile fileName tree (dataTypesDict: Dictionary<string, DataType>) =
      let filePath = rawFileWithoutComments (defaultPath + fileName)
      let imports = fileImports filePath
      let objectIdentifiers = getObjectIdentifiers filePath
      let dataTypes = getDataTypes filePath
      let objectTypes = fileObjectTypes filePath

      for import in imports do
        parseFile import.FileName tree dataTypesDict

      for oiList in objectIdentifiers do
        let oidList = oiList |> Seq.toList
        match oidList with
        | first :: tail -> 
          let matches = bfs tree first.Name

          match matches with
          | [] -> failwith "Can't find expected OID"
          | one :: [] -> insertOid tail one
          | _ -> failwith "To much matching OIDs"

        | _ -> ()

      for dt in dataTypes do
        dataTypesDict.Add(dt.Name.Value, dt);

      for obj in objectTypes |> Seq.map parseRawObjectType do
        ()
     
    parseFile fileName tree dataTypesBase
    tree
