namespace Tests

open System
open Xunit

module Encoding =
  open SnmpMib.Types
  open SnmpMib.Encoding
  
  let simpleTypeDefinition = {
    Type = "INTEGER"
    Constraint = None
  }
  let dataType = {
    Name = None
    Visibility = Visibility.Universal
    Tag = None
    Conversion = Conversion.Explicit // nie wiem jakie conversion tutaj daæ xD
    Definition = Some (Simple (simpleTypeDefinition))
  }

  [<Fact>]
  let ``Hello world test`` () =
      Assert.True(true)
  
  [<Fact>]
  let ``Simlpe positive integer`` () =
    let result = encodeObject dataType "16"
    Assert.Equal("020110", result)

  [<Fact>]
  let ``Simlpe negative integer`` () =
    let result = encodeObject dataType "16"
    Assert.Equal("020110", result)
