#use "/home/ronlv4/repos/compilation_assignment/Code_Generator/cg_tests.ml";;

let make_make_label_tests prefix =
  let index = ref 0 in
  fun () ->
  (index := !index + 1;
    Printf.sprintf "%s_%03d" prefix !index);;

let make_file_name =
  let helper = (make_make_label_tests "./foo") in
  fun () -> helper();;

let cg_tester str expected filename =
  let _ = Printf.printf "Expected:\n%s\n--------------------------------\n" expected in
  try
    let _ = Code_Generation.compile_scheme_string (filename ^ ".asm") str in
    let _ = Sys.command("make -s " ^ filename ^ ";" ^ filename) in
    ()
  with
  | X_syntax(syntax_err) -> Printf.printf "\nTest String: %s, Result: X_syntax(%s)\n" str syntax_err
  | X_not_yet_implemented -> Printf.printf "\nTest String: %s, Result: X_not_yet_implemented\n" str
  | X_this_should_not_happen(happened) -> Printf.printf "\nTest String: %s, X_this_should_not_happen(%s)\n" str happened

let rec my_map f = function
  | [] -> ()
  | first :: rest -> let filename = make_file_name() in
                      match Unix.fork() with
                        | 0 -> f first filename; exit 0
                        | pid -> ignore(Unix.waitpid [] pid) ; my_map f rest;;
let run_cg_tests (cg_tests : cg_test list) =
  let _ = my_map (fun (t : cg_test) -> cg_tester t.test t.expected) cg_tests in
  let _ = Printf.printf "Finished Running tests for code-gen\n" in
    ();;

let cg_tests2 = [ (*run specific tests here, don't forget to change the input for 'run_cg_tests'*)
    (* {test = "((lambda (x) (x x)) (lambda (x) (x x)))"}; *) (*checks tpapplic memory leaks*)
    (* {test = "(letrec ((a (lambda (x y) (y y y x)))
              (b (lambda (x y z) (z z x))))
              (a a b)) "; expected = "?"}; *)
];;
run_cg_tests cg_tests;;

