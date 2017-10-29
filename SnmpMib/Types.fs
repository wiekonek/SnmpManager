namespace SnmpMib

module Types =
  
  type Access = ReadOnly = 1 | ReadWrite = 2 | WriteOnly = 3 | NotAccessible = 4 | Unrecognized = 0
  
  type Status = Mandatory = 1 | Optional = 2 | Obsolete = 3 | Unrecognized = 0

  type ObjectType = {
    Syntax: string;
    Access: Access;
    Status: Status;
  }

  type ComplexObjectType = {
    Syntax: string;
    Access: Access;
    Status: Status;
    Index: string option;
  }
