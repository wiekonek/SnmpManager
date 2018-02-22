namespace SnmpMib

module Encoding =
  open Types
  open System

  let visibilityCodes = 
    Map.ofList[
      (Visibility.Universal, 0b00);
      (Application, 0b01);
      (ContextSpecific, 0b10);
      (Private, 0b11);
    ]

  let shiftVisibility vis =
    vis <<< 6

  type DefaultTags = 
    | Integer = 2
    | OcetetString  = 4
    | Null = 5
    | ObjectIdentifier = 6
    | Sequence = 16
  
  let defaultTagCodes =
    Map.ofList[
      ("INTEGER", DefaultTags.Integer)
      ("OCTET STRING", DefaultTags.OcetetString)
      //("SEQUENCE", DefaultTags.Sequence)
      //("SEQUENCE OF", DefaultTags.Sequence)
    ]

  type BerIdentifier = {
    Class: Visibility;
    /// primitive / constructed
    Primitive: bool;
    Tag: int;
  }
    
  module ValueEncoders =
    let getLengthInHex (value: string) =
      let length = value.Length / 2
      String.Format("{0:X2}", length)

    let checkLongConstraints value constraints =
      match constraints with
      | Some cons ->
        match cons with
        | Range range ->
          if value >= range.Min && value <= range.Max
          then ()
          else failwith "Value not in range"
        | _ -> failwith "NOT IMPLEMENTED YET"
      | None -> ()

    let encodeLongValue (intString: string) constraints =
      // TODO: Consider support for negative numbers ;)
      let i = int64 intString
      checkLongConstraints i constraints 
      let hexValue = String.Format("{0:X}", i)
      let u2HexValue =
        if hexValue.Length % 2 = 1
        then "0" + hexValue
        else hexValue
      let baseLength = u2HexValue.Length;
      let u4HexValue =
        // przepraszam xD, ale to sprawdza czy jest jedynka na początku kolejnych ósemek w zapisie binarnym
        // i dodaje jeszcze 00 z przodu jak jest 1, potrzebne do u2, nazwy zmiennych z czapy
        // czyli jak mamy 1000 0000 to dodamy jeszcze zera w hex z przodu, żeby było tak jak w mibach
        if (baseLength % 2) = 0 && ((i &&& (1L <<< (4*baseLength-1))) >>> (4*baseLength-1)) = 1L 
        then "00" + u2HexValue
        else u2HexValue
      getLengthInHex u4HexValue + u4HexValue

    let encodeStringValue (octetString: string) =
      ()
    

  let encodeBerIdentifier (identifer: BerIdentifier) =
    let berTagValue =
      shiftVisibility (visibilityCodes.Item(identifer.Class)) +
      (if identifer.Primitive then 0 else 1 <<< 5) + 
      identifer.Tag
    String.Format("{0:X2}", berTagValue) 
    

  let encodeObject (dataType: DataType) value =
    let encodedValue =
      match dataType.Definition with
      | None -> failwith "Data type definition required for encoding!"
      | Some def ->
        match def with
        | Simple simple ->
          match defaultTagCodes.Item(simple.Type) with
          | DefaultTags.Integer -> ValueEncoders.encodeLongValue value simple.Constraint
          | _ -> failwith "NOT IMPLEMENTED YET"
        | _ -> failwith "NOT IMPLEMENTED YET"
    let tag =
      match dataType.Tag with
      | Some t -> t
      | None ->
        match dataType.Definition.Value with
        | Sequence _ | SequenceOfSimpleTypes _ -> int DefaultTags.Sequence
        | Simple simple ->
          if defaultTagCodes.ContainsKey(simple.Type)
          then int (defaultTagCodes.Item(simple.Type))
          else failwith "Default tag wasn't recognized."
        | _ -> failwith "NOT IMPLEMENTED YET"
    let identifer = {
      Class = dataType.Visibility;
      Tag = tag
      Primitive = tag <= 31;
    }
    
    match dataType.Conversion with
    | Conversion.Universal | Implicit  ->
      let identifer = {
        Class = dataType.Visibility
        Tag = tag
        Primitive = tag <= 31
      }
      encodeBerIdentifier identifer + encodedValue 
    | Explicit ->
      let rootTypeTag =
        match dataType.Definition.Value with
          | Sequence _ | SequenceOfSimpleTypes _ -> int DefaultTags.Sequence
          | Simple simple ->
            if defaultTagCodes.ContainsKey(simple.Type)
            then int (defaultTagCodes.Item(simple.Type))
            else failwith "Default tag wasn't recognized."
          | _ -> failwith "NOT IMPLEMENTED YET"
      let rootIdentifier = {
        Class = Visibility.Universal // tak jak w playground chyba
        Tag = rootTypeTag
        Primitive = tag <= 31
      }
      let root = encodeBerIdentifier rootIdentifier + encodedValue
      let identifer = {
        Class = dataType.Visibility;
        Tag = tag
        Primitive = false;
      }

      encodeBerIdentifier identifer + ValueEncoders.getLengthInHex root + root



    

