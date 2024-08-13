module type Pattern = sig
  type 'a loc
  type label
  type constant
  type closed_flag
  type location_stack
  type object_field
  type arg_label
  type attribute
  type attributes
  type class_declaration
  type class_expr
  type class_type
  type class_type_declaration 
  type core_type
  type expression
  type extension
  type include_declaration
  type module_binding
  type module_exp
  type module_type
  type module_type_declaration
  type open_declaration
  type package_type
  type payload
  type rec_flag
  type row_field
  type structure
  type type_declaration
  type type_exception
  type type_extension
  type value_binding
  type value_binding_list
  type value_description
  
type pattern =
    {
     ppat_desc: pattern_desc;
     ppat_loc: Location.t;
     ppat_loc_stack: location_stack;
     ppat_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }
      [@@deriving  yojson]

and pattern_desc =
  | Ppat_any  (** The pattern [_]. *)
  | Ppat_var of string loc  (** A variable pattern such as [x] *)
  | Ppat_alias of pattern * string loc
      (** An alias pattern such as [P as 'a] *)
  | Ppat_constant of constant
      (** Patterns such as [1], ['a'], ["true"], [1.0], [1l], [1L], [1n] *)
  | Ppat_interval of constant * constant
      (** Patterns such as ['a'..'z'].

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple of pattern list
      (** Patterns [(P1, ..., Pn)].

           Invariant: [n >= 2]
        *)
  | Ppat_construct of Longident.t loc * (string loc list * pattern) option
      (** [Ppat_construct(C, args)] represents:
            - [C]               when [args] is [None],
            - [C P]             when [args] is [Some ([], P)]
            - [C (P1, ..., Pn)] when [args] is
                                           [Some ([], Ppat_tuple [P1; ...; Pn])]
            - [C (type a b) P]  when [args] is [Some ([a; b], P)]
         *)
  | Ppat_variant of label * pattern option
      (** [Ppat_variant(`A, pat)] represents:
            - [`A]   when [pat] is [None],
            - [`A P] when [pat] is [Some P]
         *)
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
      (** [Ppat_record([(l1, P1) ; ... ; (ln, Pn)], flag)] represents:
            - [{ l1=P1; ...; ln=Pn }]
                 when [flag] is {{!Asttypes.closed_flag.Closed}[Closed]}
            - [{ l1=P1; ...; ln=Pn; _}]
                 when [flag] is {{!Asttypes.closed_flag.Open}[Open]}

           Invariant: [n > 0]
         *)
  | Ppat_array of pattern list  (** Pattern [[| P1; ...; Pn |]] *)
  | Ppat_or of pattern * pattern  (** Pattern [P1 | P2] *)
  | Ppat_constraint of pattern * core_type  (** Pattern [(P : T)] *)
  | Ppat_type of Longident.t loc  (** Pattern [#tconst] *)
  | Ppat_lazy of pattern  (** Pattern [lazy P] *)
  | Ppat_unpack of string option loc
      (** [Ppat_unpack(s)] represents:
            - [(module P)] when [s] is [Some "P"]
            - [(module _)] when [s] is [None]

           Note: [(module P : S)] is represented as
           [Ppat_constraint(Ppat_unpack(Some "P"), Ptyp_package S)]
         *)
  | Ppat_exception of pattern  (** Pattern [exception P] *)
  | Ppat_extension of extension  (** Pattern [[%id]] *)
  | Ppat_open of Longident.t loc * pattern  (** Pattern [M.(P)] *)
[@@deriving  yojson]


end
