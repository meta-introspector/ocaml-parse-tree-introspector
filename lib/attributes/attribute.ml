module type AbstractAst = sig

  type payload
  type 'a loc
  type attribute = {
    attr_name : string loc;
    attr_payload : payload;
    attr_loc : Location.t;
    }
                     [@@deriving  yojson]
  and extension = string loc * payload
                                 [@@deriving  yojson]
  and attributes = attribute list
                     [@@deriving  yojson]
end
