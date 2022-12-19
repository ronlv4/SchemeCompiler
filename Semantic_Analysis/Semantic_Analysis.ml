#use "../Tag_Parser/Tag_Parser.ml";;

type app_kind = Tail_Call | Non_Tail_Call;;

type lexical_address =
  | Free
  | Param of int
  | Bound of int * int;;

type var' = Var' of string * lexical_address;;

type expr' =
  | ScmConst' of sexpr
  | ScmVarGet' of var'
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmOr' of expr' list
  | ScmVarSet' of var' * expr'
  | ScmVarDef' of var' * expr'
  | ScmBox' of var'
  | ScmBoxGet' of var'
  | ScmBoxSet' of var' * expr'
  | ScmLambda' of string list * lambda_kind * expr'
  | ScmApplic' of expr' * expr' list * app_kind;;

module type SEMANTIC_ANALYSIS = sig
  val annotate_lexical_address : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val auto_box : expr' -> expr'
  val semantics : expr -> expr'
end;; (* end of signature SEMANTIC_ANALYSIS *)

module Semantic_Analysis : SEMANTIC_ANALYSIS = struct
module Semantic_Analysis = struct

  let rec lookup_in_rib name = function
    | [] -> None
    | name' :: rib ->
       if name = name'
       then Some(0)
       else (match (lookup_in_rib name rib) with
             | None -> None
             | Some minor -> Some (minor + 1));;

  let rec lookup_in_env name = function
    | [] -> None
    | rib :: env ->
       (match (lookup_in_rib name rib) with
        | None ->
           (match (lookup_in_env name env) with
            | None -> None
            | Some(major, minor) -> Some(major + 1, minor))
        | Some minor -> Some(0, minor));;

  let tag_lexical_address_for_var name params env =
    match (lookup_in_rib name params) with
    | None ->
       (match (lookup_in_env name env) with
        | None -> Var' (name, Free)
        | Some(major, minor) -> Var' (name, Bound (major, minor)))
    | Some minor -> Var' (name, Param minor);;

  (* run this first *)
  let annotate_lexical_address =
    let rec run expr params env =
      match expr with
      | ScmConst sexpr -> ScmConst'(sexpr)
      | ScmVarGet (Var str) -> ScmVarGet'(tag_lexical_address_for_var str params env)
      | ScmIf (test, dit, dif) -> ScmIf'(run test params env, run dit params env, run dif params env)
      | ScmSeq exprs -> ScmSeq'(List.map (fun exp -> run exp params env) exprs)
      | ScmOr exprs -> ScmOr'(List.map (fun exp -> run exp params env) exprs)
      | ScmVarSet(Var v, expr) -> ScmVarSet'(tag_lexical_address_for_var v params env, run expr params env)
      (* this code does not [yet?] support nested define-expressions *)
      | ScmVarDef(Var v, expr) -> ScmVarDef' (tag_lexical_address_for_var v params env, run expr params env)
      | ScmLambda (params', Simple, expr) -> ScmLambda'(params', Simple, run expr params env)
      | ScmLambda (params', Opt opt, expr) -> ScmLambda'(params', Opt opt, run expr params env)
      | ScmApplic (proc, args) ->
         ScmApplic' (run proc params env,
                     List.map (fun arg -> run arg params env) args,
                     Non_Tail_Call)
    in
    fun expr ->
    run expr [] [];;

  (* run this second *)
  let annotate_tail_calls =
    let rec run in_tail = function
      | (ScmConst' _) as orig -> orig
      | (ScmVarGet' _) as orig -> orig
      | ScmIf' (test, dit, dif) -> ScmIf'(run false test, run in_tail dit, run in_tail dif)
      | ScmSeq' [] -> ScmSeq' []
      | ScmSeq' (expr :: exprs) -> ScmSeq'(runl in_tail expr exprs)
      | ScmOr' [] -> ScmOr' []
      | ScmOr' (expr :: exprs) -> ScmOr'(runl in_tail expr exprs)
      | ScmVarSet' (var', expr') -> ScmVarSet'(var', run false expr')
      | ScmVarDef' (var', expr') -> ScmVarDef'(var', run false expr')
      | (ScmBox' _) as expr' -> expr'
      | (ScmBoxGet' _) as expr' -> expr'
      | ScmBoxSet' (var', expr') -> ScmVarSet'(var',run false expr')
      | ScmLambda' (params, Simple, expr) -> ScmLambda'(params,Simple, run true expr)
      | ScmLambda' (params, Opt opt, expr) -> ScmLambda'(params,Opt opt, expr)
      | ScmApplic' (proc, args, app_kind) ->
         if in_tail
         then ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Tail_Call)
         else ScmApplic' (run false proc,
                          List.map (fun arg -> run false arg) args,
                          Non_Tail_Call)
    and runl in_tail expr = function
      | [] -> [run in_tail expr]
      | expr' :: exprs -> (run false expr) :: (runl in_tail expr' exprs)
    in
    fun expr' -> run false expr';;

  (* auto_box *)

  let copy_list = List.map (fun si -> si);;

  let combine_pairs =
    List.fold_left
      (fun (rs1, ws1) (rs2, ws2) -> (rs1 @ rs2, ws1 @ ws2))
      ([], []);;


(*find_reads_and_writes : string * expr' * string list * string list list *)
  let find_reads_and_writes =
    let rec run name expr params env =
      match expr with
      | ScmConst' _ -> ([], [])
      | ScmVarGet' (Var' (_, Free)) -> ([], [])
      | ScmVarGet' (Var' (name', _) as v) ->
         if name = name'
         then ([(v, env)], [])
         else ([], [])
      | ScmBox' _ -> ([], [])
      | ScmBoxGet' _ -> ([], [])
      | ScmBoxSet' (_, expr) -> run name expr params env
      | ScmIf' (test, dit, dif) ->
         let (rs1, ws1) = (run name test params env) in
         let (rs2, ws2) = (run name dit params env) in
         let (rs3, ws3) = (run name dif params env) in
         (rs1 @ rs2 @ rs3, ws1 @ ws2 @ ws3)
      | ScmSeq' exprs ->
         combine_pairs
           (List.map
              (fun expr -> run name expr params env)
              exprs)
      | ScmVarSet' (Var' (_, Free), expr) -> run name expr params env
      | ScmVarSet' ((Var' (name', _) as v), expr) ->
         let (rs1, ws1) =
           if name = name'
           then ([], [(v, env)])
           else ([], []) in
         let (rs2, ws2) = run name expr params env in
         (rs1 @ rs2, ws1 @ ws2)
      | ScmVarDef' (_, expr) -> run name expr params env
      | ScmOr' exprs ->
         combine_pairs
           (List.map
              (fun expr -> run name expr params env)
              exprs)
      | ScmLambda' (params', Simple, expr) ->
         if (List.mem name params')
         then ([], [])
         else run name expr params' ((copy_list params) :: env)
      | ScmLambda' (params', Opt opt, expr) ->
         let params' = params' @ [opt] in
         if (List.mem name params')
         then ([], [])
         else run name expr params' ((copy_list params) :: env)
      | ScmApplic' (proc, args, app_kind) ->
         let (rs1, ws1) = run name proc params env in
         let (rs2, ws2) =
           combine_pairs
             (List.map
                (fun arg -> run name arg params env)
                args) in
         (rs1 @ rs2, ws1 @ ws2)
    in
    fun name expr params ->
    run name expr params [];;

  let cross_product as' bs' =
    List.concat (List.map (fun ai ->
                     List.map (fun bj -> (ai, bj)) bs')
                   as');;

(* should_box_var: string * expr' * string list *)
  let should_box_var name expr params =
    match (find_reads_and_writes name expr params) with
    | ([], _) -> false
    | (_, []) -> false
    | (reads, writes) ->
    (* if one of the reads refer to a different environment than the writes *)
       let envs = List.map (fun (_, env) -> env) writes in
       let envs' = List.map (fun (_, env) -> env) reads in
       let envs'' = cross_product envs envs' in
       let envs''' = List.filter (fun (env1, env2) -> env1 != env2) envs'' in
       match envs''' with
       | [] -> false
       | _ -> true;;
  let box_sets_and_gets name body =
    let rec run expr =
      match expr with
      | ScmConst' _ -> expr
      | ScmVarGet' (Var' (_, Free)) -> expr
      | ScmVarGet' (Var' (name', _) as v) ->
         if name = name'
         then ScmBoxGet' v
         else expr
      | ScmBox' _ -> expr
      | ScmBoxGet' _ -> expr
      | ScmBoxSet' (v, expr) -> ScmBoxSet' (v, run expr)
      | ScmIf' (test, dit, dif) ->
         ScmIf' (run test, run dit, run dif)
      | ScmSeq' exprs -> ScmSeq' (List.map run exprs)
      | ScmVarSet' (Var' (_, Free) as v, expr') ->
         ScmVarSet'(v, run expr')
      | ScmVarSet' (Var' (name', _) as v, expr') ->
         if name = name'
         then ScmBoxSet' (v, run expr')
         else ScmVarSet' (v, run expr')
      | ScmVarDef' (v, expr) -> ScmVarDef' (v, run expr)
      | ScmOr' exprs -> ScmOr' (List.map run exprs)
      | (ScmLambda' (params, Simple, expr)) as expr' ->
         if List.mem name params
         then expr'
         else ScmLambda' (params, Simple, run expr)
      | (ScmLambda' (params, Opt opt, expr)) as expr' ->
         if List.mem name (params @ [opt])
         then expr'
         else ScmLambda' (params, Opt opt, run expr)
      | ScmApplic' (proc, args, app_kind) ->
         ScmApplic' (run proc, List.map run args, app_kind)
    in
    run body;;

  let make_sets =
    let rec run minor names params =
      match names, params with
      | [], _ -> []
      | name :: names', param :: params' ->
         if name = param
         then let v = Var' (name, Param minor) in
              (ScmVarSet' (v, ScmBox' v)) :: (run (minor + 1) names' params')
         else run (minor + 1) names params'
      | _, _ -> raise (X_this_should_not_happen
                        "no free vars should be found here")
    in
    fun box_these params -> run 0 box_these params;;

  let rec auto_box expr =
    match expr with
    | ScmConst' _ | ScmVarGet' _ | ScmBox' _ | ScmBoxGet' _ -> expr
    | ScmBoxSet' (v, expr) -> ScmBoxSet' (v, auto_box expr)
    | ScmIf' (test, dit, dif) ->
       ScmIf' (auto_box test, auto_box dit, auto_box dif)
    | ScmSeq' exprs -> ScmSeq' (List.map auto_box exprs)
    | ScmVarSet' (v, expr) -> ScmVarSet' (v, auto_box expr)
    | ScmVarDef' (v, expr) -> ScmVarDef' (v, auto_box expr)
    | ScmOr' exprs -> ScmOr' (List.map auto_box exprs)
    | ScmLambda' (params, Simple, expr') ->
       let box_these =
         List.filter
           (fun param -> should_box_var param expr' params)
           params in
       let new_body =
         List.fold_left
           (fun body name -> box_sets_and_gets name body)
           (auto_box expr')
           box_these in
       let new_sets = make_sets box_these params in
       let new_body =
         match box_these, new_body with
         | [], _ -> new_body
         | _, ScmSeq' exprs -> ScmSeq' (new_sets @ exprs)
         | _, _ -> ScmSeq'(new_sets @ [new_body]) in
       ScmLambda' (params, Simple, new_body)
    | ScmLambda' (params, Opt opt, expr') ->
         let box_these =
            List.filter
              (fun param -> should_box_var param expr' (params @ [opt]))
              (params @ [opt]) in
         let new_body =
            List.fold_left
              (fun body name -> box_sets_and_gets name body)
              (auto_box expr')
              box_these in
         let new_sets = make_sets box_these (params @ [opt]) in
         let new_body =
            match box_these, new_body with
            | [], _ -> new_body
            | _, ScmSeq' exprs -> ScmSeq' (new_sets @ exprs)
            | _, _ -> ScmSeq'(new_sets @ [new_body]) in
         ScmLambda' (params, Opt opt, new_body)
    | ScmApplic' (proc, args, app_kind) ->
       ScmApplic' (auto_box proc, List.map auto_box args, app_kind);;

  let semantics expr =
    auto_box
      (annotate_tail_calls
         (annotate_lexical_address expr));;

end;; (* end of module Semantic_Analysis *)