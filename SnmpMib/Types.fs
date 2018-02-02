namespace SnmpMib

module Types =
  open System.Collections.Generic

  type Access = ReadOnly | ReadWrite | WriteOnly | NotAccessible | Unrecognized
  
  type Status = Mandatory | Optional | Obsolete | Unrecognized

  type Visibility = Universal | Application | ContextSpecific | Private

  type Conversion = Implicit | Explicit

  type Range = {
    Min: int64;
    Max: int64;
  }

  type Constraint =
    | Size of int
    | Range of Range
    /// key = EnumName; value = EnumId
    | EnumValues of Dictionary<string, int>
    | Unrecognized of string

  type SimpleType = {
    Type: string;
    Constraint: Constraint option;
  }
  
  type SequenceType = {
    /// key = ObjectTypeName; value = DataTypeName
    Values: Dictionary<string, string>
  }

  type ChoiceType = {
    /// key = ObjectIdentifierName; value = DataTypeName
    Values: Dictionary<string, string>
  }

  type DataTypeDefinition =
    | Choice of ChoiceType
    | Sequence of SequenceType
    /// It's a name of another DataType
    | SequenceOfSimpleTypes of string
    | Simple of SimpleType
    /// It's a name of another DataType
    | DataTypeReference of string

  type DataType = {
    Name: string option;
    Visibility: Visibility;
    Tag: int option;
    Conversion: Conversion;
    Definition: DataTypeDefinition option;
  }

  type RawDataType = {
    Name: string;
    Tag: int option;
    Visibility: Visibility;
    Conversion: Conversion;
    RawDefinition: string;
  }

  type RawImports = {
    Imports: string;
    FileName: string;
  }

  //type Imports = {
  //  FileName: string;
  //  Imports: string[];
  //}

  type RawObjectIdentifier = {
    Name: string;
    Oid: string;
  }

  type ObjectIdentifier = {
    OidPart: int option;
    Name: string;
  }

  type ObjectType = {
    OidPart: int;
    Name: string;
    Syntax: DataType;
    Access: Access option;
    Status: Status option;
    Description: string;
  }

  type RawObjectType = {
    Name: string;
    Syntax: DataTypeDefinition;
    Access: string;
    Status: string;
    Description: string;
    Oid: string;
  }
