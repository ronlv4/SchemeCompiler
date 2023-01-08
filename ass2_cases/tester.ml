#use "/home/ronlv4/repos/compilation_assignment/ass2_cases/quadruples.ml";;
#use "/home/ronlv4/repos/compilation_assignment/Tag_Parser/Tag_Parser.ml";;

let run_test = function
  | [] -> "finished testing successfully"
  | (code, sexpr, expr, expr'):: tl ->
    let result_expr = Tag_Parser.tag_parse sexpr in
    if (result_expr = expr) then run_test tl else (Printf.printf "failed on test with %s" code)


run_test [("a", ScmSymbol "a", ScmVarGet (Var "a"), ScmVarGet' (Var' ("a", Free)))]
