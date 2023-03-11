#use "/home/ronlv4/repos/compilation_assignment/Reader/Reader.ml";;

open Pervasives;;
open Reader;;

let read_input_file (input_file : string) : string =
  let chan = open_in input_file in
  let contents = really_input_string chan (in_channel_length chan) in
  close_in chan;
  contents

let parse_sexpr (input_string : string) : sexpr =
  (* call the Reader module to parse the input string and return the resulting s-expression *)
  (nt_sexpr input_string 0).found;;

let convert_to_string (sexpr : sexpr) : string =
  string_of_sexpr sexpr

let read_in_chez_scheme (input_string : string) : sexpr =
  let command = Printf.sprintf "(read %s)" (String.escaped input_string) in
  let process = Unix.create_process "scheme" [| "scheme" |] Unix.stdin Unix.stdout Unix.stderr in
  let output = input_line (Unix.in_channel_of_descr process) in
  (* parse the output string to produce an s-expression *)
  (nt_sexpr output 0).found;;

let compare_sexprs (sexpr1 : sexpr) (sexpr2 : sexpr) : bool =
  (* compare the two s-expressions and return true if they are equal, false otherwise *)
  sexpr1 = sexpr2

let test_reader (input_file : string) (expected_output_file : string) : bool =
  (* read the contents of the input file *)
  let input_string = read_input_file input_file in

  (* parse the input string to produce an s-expression *)
  let sexpr = parse_sexpr input_string in

  (* convert the s-expression to a string *)
  let string = convert_to_string sexpr in

  (* read the string in Chez Scheme to produce an s-expression *)
  let sexpr_from_chez = read_in_chez_scheme string in

  (* read the expected output file and parse it to produce an s-expression *)
  let expected_output_string = read_input_file expected_output_file in
  let expected_sexpr = read_in_chez_scheme expected_output_string in

  (* compare the two s-expressions *)
  compare_sexprs sexpr_from_chez expected_sexpr

let test_compiler (input_file : string) (expected_output_file : string) : bool =
  (* run the compiler program on the input file and compare the output to the expected output file *)
  let result = Unix.system ("./compiler " ^ input_file) in
  let output =
    match result with
    | Ok status -> Unix.Exit_or_signal.to_string_hum status
    | Error exn -> Exn.to_string exn
  in
  let expected_output = read_input_file expected_output_file in
  output = expected_output

(* iterate over all files in the in_to_student and in_to *)

let () =
  let files = Sys.readdir "in_to_student" in
  Array.iter files ~f:(fun input_file ->
      let expected_output_file = "in_to_scheme/" ^ input_file in
      let input_path = "in_to_student/" ^ input_file in
      if test_reader input_path expected_output_file && test_compiler input_path expected_output_file then
        print_endline (input_file ^ ": PASS")
      else
        print_endline (input_file ^ ": FAIL"))
