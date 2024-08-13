module type CoreType = sig
  type 'a loc
  type label
  type closed_flag
  type object_field
  type arg_label
  type attribute
  type attributes
  type class_declaration
  type class_expr
  type class_type
  type class_type_declaration 
  type expression
  type extension
  type include_declaration
  type location_stack
  type module_binding
  type module_exp
  type module_type
  type module_type_declaration
  type open_declaration
  type package_type
  type pattern
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
                        
type core_type =
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_loc_stack: location_stack;
     ptyp_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }
      [@@deriving  yojson]

and core_type_desc =
  | Ptyp_any  (** [_] *)
  | Ptyp_var of string  (** A type variable such as ['a] *)
  | Ptyp_arrow of arg_label * core_type * core_type
      (** [Ptyp_arrow(lbl, T1, T2)] represents:
            - [T1 -> T2]    when [lbl] is
                                     {{!Asttypes.arg_label.Nolabel}[Nolabel]},
            - [~l:T1 -> T2] when [lbl] is
                                     {{!Asttypes.arg_label.Labelled}[Labelled]},
            - [?l:T1 -> T2] when [lbl] is
                                     {{!Asttypes.arg_label.Optional}[Optional]}.
         *)
  | Ptyp_tuple of core_type list
      (** [Ptyp_tuple([T1 ; ... ; Tn])]
          represents a product type [T1 * ... * Tn].

           Invariant: [n >= 2].
        *)
  | Ptyp_constr of Longident.t loc * core_type list
      (** [Ptyp_constr(lident, l)] represents:
            - [tconstr]               when [l=[]],
            - [T tconstr]             when [l=[T]],
            - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].
         *)
  | Ptyp_object of object_field list * closed_flag
      (** [Ptyp_object([ l1:T1; ...; ln:Tn ], flag)] represents:
            - [< l1:T1; ...; ln:Tn >]     when [flag] is
                                       {{!Asttypes.closed_flag.Closed}[Closed]},
            - [< l1:T1; ...; ln:Tn; .. >] when [flag] is
                                           {{!Asttypes.closed_flag.Open}[Open]}.
         *)
  | Ptyp_class of Longident.t loc * core_type list
      (** [Ptyp_class(tconstr, l)] represents:
            - [#tconstr]               when [l=[]],
            - [T #tconstr]             when [l=[T]],
            - [(T1, ..., Tn) #tconstr] when [l=[T1 ; ... ; Tn]].
         *)
  | Ptyp_alias of core_type * string  (** [T as 'a]. *)
  | Ptyp_variant of row_field list * closed_flag * label list option
      (** [Ptyp_variant([`A;`B], flag, labels)] represents:
            - [[ `A|`B ]]
                      when [flag]   is {{!Asttypes.closed_flag.Closed}[Closed]},
                       and [labels] is [None],
            - [[> `A|`B ]]
                      when [flag]   is {{!Asttypes.closed_flag.Open}[Open]},
                       and [labels] is [None],
            - [[< `A|`B ]]
                      when [flag]   is {{!Asttypes.closed_flag.Closed}[Closed]},
                       and [labels] is [Some []],
            - [[< `A|`B > `X `Y ]]
                      when [flag]   is {{!Asttypes.closed_flag.Closed}[Closed]},
                       and [labels] is [Some ["X";"Y"]].
         *)
  | Ptyp_poly of string loc list * core_type
      (** ['a1 ... 'an. T]

           Can only appear in the following context:

           - As the {!core_type} of a
          {{!pattern_desc.Ppat_constraint}[Ppat_constraint]} node corresponding
             to a constraint on a let-binding:
            {[let x : 'a1 ... 'an. T = e ...]}

           - Under {{!class_field_kind.Cfk_virtual}[Cfk_virtual]} for methods
          (not values).

           - As the {!core_type} of a
           {{!class_type_field_desc.Pctf_method}[Pctf_method]} node.

           - As the {!core_type} of a {{!expression_desc.Pexp_poly}[Pexp_poly]}
           node.

           - As the {{!label_declaration.pld_type}[pld_type]} field of a
           {!label_declaration}.

           - As a {!core_type} of a {{!core_type_desc.Ptyp_object}[Ptyp_object]}
           node.

           - As the {{!value_description.pval_type}[pval_type]} field of a
           {!value_description}.
         *)
  | Ptyp_package of package_type  (** [(module S)]. *)
  | Ptyp_extension of extension  (** [[%id]]. *)
                        [@@deriving  yojson]
end
