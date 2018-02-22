namespace SnmpMib

module Encoding =
  open Types
  open System

  exception ConstraintException of string

  let uppercase (x: string) = x.ToUpper()

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
    open System.Text.RegularExpressions

    let getLengthInHex (value: string) =
      let length = value.Length / 2
      String.Format("{0:X2}", length)

    let checkLongConstraints constraints value  =
      match constraints with
      | Some cons ->
        match cons with
        | Range range ->
          if value >= range.Min && value <= range.Max
          then ()
          else raise (ConstraintException "Value not in range")
        | _ -> failwith "NOT IMPLEMENTED YET"
      | None -> ()
      value

    let encodeLongValue (intString: string) constraints =
      int64 intString
      |> checkLongConstraints constraints
      |> fun i -> String.Format("{0:X2}", i), i
      ||> fun str i ->
        let len = str.Length
        match len % 2 with
        | 1 -> "0" + str
        | 0 when i >>> 4 * len - 1 = 1L -> "00" + str
        | _ -> str
      |> fun str -> getLengthInHex str + str
      

    let encodeStringValue (octetString: string) =
      Regex.Replace(octetString, "[\s]+", "")
      |> uppercase
      |> fun s -> if s.Length % 2 = 0 then s else "0" + s
      |> fun s -> 
        if Regex.IsMatch(s, "^[A-F0-9]*$", RegexOptions.Multiline)
        then s, s.Length / 2
        else failwith "Not an OCTET STRING"
      ||> fun s len ->
        match len with
        | _ when len < 127 -> String.Format("{0:X2}{1}", len, s)
        | _ when len = 127 -> failwith "Not shure what I should do with this length : /"
        | _ when len > 127 ->
          len
          |> fun _ -> String.Format("{0:X2}", len)
          |> fun lenStr ->
            let l = lenStr.Length
            match l % 2 with
            | 1 -> "0" + lenStr
            | _ -> lenStr
          |> fun res -> String.Format("{0:X2}{1}{2}", (res.Length / 2) ||| 0x80, res, s)
        | _ -> failwith "NOT IMPLEMENTED YET"
      
    

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
          | DefaultTags.OcetetString -> ValueEncoders.encodeStringValue value
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



    

