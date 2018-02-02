namespace SnmpMib

module Encoding =
  open Types
  open System

  let visibilityCodes = 
    Map.ofList[
      (Universal, int 00uy);
      (Application, int 01uy);
      (ContextSpecific, int 10uy);
      (Private, int 11uy);
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
    Primitive: bool;
    Tag: int;
  }
    
  module ValueEncoders =
    let encodeLongValue (intString: string) =
      // TODO: Consider support for negative numbers ;)
      let i = int32 intString
      let hexValue = String.Format("{0:X}", i)
      let u2HexValue =
        if hexValue.Length % 2 = 1
        then "0" + hexValue
        else hexValue
      let length = u2HexValue.Length / 2
      String.Format("{0:X2}", length) + u2HexValue
    

  let encodeBerIdentifier (identifer: BerIdentifier) =
    let berTagValue =
      shiftVisibility (visibilityCodes.Item(identifer.Class)) +
      (if identifer.Primitive then 0 else 1 <<< 5) + 
      identifer.Tag
    String.Format("{0:X2}", berTagValue) 
    

  let encodeObject (dataType: DataType) value =
    let identifer = {
      Class = dataType.Visibility;
      Primitive = dataType.Conversion = Conversion.Explicit;
      Tag = match dataType.Tag with
            | Some tag -> tag
            | None ->
              match dataType.Definition with
              | None -> failwith "Data type definition required for encoding!"
              | Some definition ->
                match definition with
                | Sequence _ | SequenceOfSimpleTypes _ -> int DefaultTags.Sequence
                | Simple simple ->
                  if defaultTagCodes.ContainsKey(simple.Type)
                  then int (defaultTagCodes.Item(simple.Type))
                  else failwith "Default tag wasn't recognized."
                | _ -> failwith "NOT IMPLEMENTED YET"
    }
    let encodeValue =
      match dataType.Definition.Value with
      | Simple simple ->
        match defaultTagCodes.Item(simple.Type) with
        | DefaultTags.Integer -> ValueEncoders.encodeLongValue value
        | _ -> failwith "NOT IMPLEMENTED YET"
      | _ -> failwith "NOT IMPLEMENTED YET"
    let berTag = encodeBerIdentifier identifer

    berTag + encodeValue 
    //string 12
    //"020110"

    

