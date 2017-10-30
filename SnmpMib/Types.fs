namespace SnmpMib

module Types =
  open System.Collections.Generic
  
  type Access = ReadOnly = 1 | ReadWrite = 2 | WriteOnly = 3 | NotAccessible = 4 | Unrecognized = 0
  
  type Status = Mandatory = 1 | Optional = 2 | Obsolete = 3 | Unrecognized = 0

  type Range = {
    Min: int;
    Max: int;
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

  type DataType = {
    Name: string;
    Application: int option;
    Implicit: bool option;
    Definition: DataTypeDefinition;
  }

  type RawDataType = {
    Name: string;
    Application: int option;
    Implicit: bool option;
    RawDefinition: string;
  }


  type ObjectType = {
    OidPart: int;
    Name: string;
    Syntax: DataType;
    Access: Access;
    Status: Status;
    Index: string option;
  }

  type RawObjectType = {
    Name: string;
    Syntax: string;
    Access: string;
    Status: string;
    Description: string;
    Index: string option;
    Oid: string;
  }
  

  type RawImports = {
    Imports: string;
    FileName: string;
  }

  type RawObjectIdentifier = {
    Name: string;
    Oid: string;
  }
    

