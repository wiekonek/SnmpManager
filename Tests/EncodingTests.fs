namespace Tests

open System
open Xunit

module Encoding =
  open SnmpMib.Types
  open SnmpMib.Encoding
  
  //[<Fact>]
  //let ``Should encode simple INTEGER when value is negative`` () =
  //  let simpleTypeDefinition = {
  //    Type = "INTEGER"
  //    Constraint = None
  //  }
  //  let dataType = {
  //    Name = None
  //    Visibility = Visibility.Universal
  //    Tag = None
  //    Conversion = Conversion.Universal
  //    Definition = Some (Simple (simpleTypeDefinition))
  //  }
  //  let testMethod = encodeObject dataType
  //  Assert.Equal("0201F0", testMethod "-16")

  [<Fact>]
  let ``Should encode simple INTEGER when value is positive`` () =
    let simpleTypeDefinition = {
      Type = "INTEGER"
      Constraint = None
    }
    let dataType = {
      Name = None
      Visibility = Visibility.Universal
      Tag = None
      Conversion = Conversion.Universal
      Definition = Some (Simple (simpleTypeDefinition))
    }
    let testMethod = encodeObject dataType
    Assert.Equal("020100", testMethod "0")
    Assert.Equal("020110", testMethod "16")
    Assert.Equal("0202008A", testMethod "138")
    Assert.Equal("02087FFFFFFFFFFFFFFF", testMethod (Int64.MaxValue.ToString()))

  [<Fact>]
  let ``Should throw error when INTEGER not in specified range`` () =
    let simpleTypeDefinition = {
      Type = "INTEGER"
      Constraint = Some (Range {Min=0L; Max=10L})
    }
    let dataType = {
      Name = None
      Visibility = Visibility.Universal
      Tag = None
      Conversion = Conversion.Universal
      Definition = Some (Simple (simpleTypeDefinition))
    }
    let testMethod = encodeObject dataType
    Assert.ThrowsAny<ConstraintException>(fun () -> testMethod "11" |> ignore) |> ignore
    Assert.ThrowsAny<ConstraintException>(fun () -> testMethod "-1" |> ignore) |> ignore

  [<Fact>]
  let ``Should encode INTEGER when conversion is IMPLICIT and visibility is APPLICATION`` () =
    let simpleTypeDefinition = {
      Type = "INTEGER"
      Constraint = None
    }
    let dataType = {
      Name = None
      Visibility = Visibility.Application
      Tag = Some 3
      Conversion = Conversion.Implicit
      Definition = Some (Simple (simpleTypeDefinition))
    }
    let testMethod = encodeObject dataType
    Assert.Equal("430100", testMethod "0")
    Assert.Equal("430110", testMethod "16")
    Assert.Equal("4302008A", testMethod "138")

  [<Fact>]
  let ``Should encode INTEGER when conversion is EXPLICIT and visibility is APPLICATION`` () =
    let simpleTypeDefinition = {
      Type = "INTEGER"
      Constraint = None
    }
    let dataType = {
      Name = None
      Visibility = Visibility.Application
      Tag = Some 3
      Conversion = Conversion.Explicit
      Definition = Some (Simple (simpleTypeDefinition))
    }
    let testMethod = encodeObject dataType
    Assert.Equal("6303020100", testMethod "0")
    Assert.Equal("6303020110", testMethod "16")
    Assert.Equal("63040202008A", testMethod "138")


  [<Fact>]
  let ``Should encode simple OCTET STRING when length < 127`` () =
    let simpleTypeDefinition = {
      Type = "OCTET STRING"
      Constraint = None
    }
    let dataType = {
      Name = None
      Visibility = Visibility.Universal
      Tag = None
      Conversion = Conversion.Universal
      Definition = Some (Simple (simpleTypeDefinition))
    }
    let testMethod = encodeObject dataType
    Assert.Equal("0400", testMethod "")
    Assert.Equal("040100", testMethod "00")
    Assert.Equal("040100", testMethod "0")
    Assert.Equal("04080123456789ABCDEF", testMethod "0123456789ABCDEF")
    Assert.Equal("04080123456789ABCDEF", testMethod "0123456789abcdef")

  [<Fact>]
  let ``Should encode simple OCTET STRING when length > 127`` () =
    let simpleTypeDefinition = {
      Type = "OCTET STRING"
      Constraint = None
    }
    let dataType = {
      Name = None
      Visibility = Visibility.Universal
      Tag = None
      Conversion = Conversion.Universal
      Definition = Some (Simple (simpleTypeDefinition))
    }
    let testMethod = encodeObject dataType
    let testVal len =
      seq { for _ in 1..len -> "10"}
      |> String.concat String.Empty
    Assert.Equal("048180" + testVal 0x80, testMethod (testVal 0x80))
    Assert.Equal("048182" + testVal 0x82, testMethod (testVal 0x82))
    Assert.Equal("0483123FFF" + testVal 0x123FFF, testMethod (testVal 0x123FFF))

