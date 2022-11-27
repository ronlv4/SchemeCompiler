#require "containers";;
open CCFormat;;
set_color_default true;;

  "what is your @{<White>favorite color@}? @{<blue>blue@}! No, @{<red>red@}! Ahhhhhhh@.";;

let arguments_boolean = ["#t"; "#T"; "#f"; "#F"];;
let expected_boolean = [
                {index_from = 0; index_to = 2; found = ScmBoolean true};
                {index_from = 0; index_to = 2; found = ScmBoolean true};
                {index_from = 0; index_to = 2; found = ScmBoolean false};
                {index_from = 0; index_to = 2; found = ScmBoolean false}
];;

let arguments_strings = ["#\\a"; "#\\A"; "\"\""; "\"moshe!\""; "\"moshe!\\n\\t\\r\\f\""];;
let expected_strings = [
                {index_from = 0; index_to = 3; found = ScmChar 'a'};
                {index_from = 0; index_to = 3; found = ScmChar 'A'};
                {index_from = 0; index_to = 2; found = ScmString ""};
                {index_from = 0; index_to = 8; found = ScmString "moshe!"};
                {index_from = 0; index_to = 16; found = ScmString "moshe!\n\t\r\012"}
                ];;

let arguments_ascii = ["\"The letter 'a' can be entered as \\x61;\""; "\"The letter 'A' can be entered as \\x41;\""];;
let expected_ascii = [
                {index_from = 0; index_to = 40; found = ScmString "The letter 'a' can be entered as a"};
                {index_from = 0; index_to = 40; found = ScmString "The letter 'A' can be entered as A"}
];;

let arguments_keywords = ["lambda"; "if"; "#\\space"; "#\\return"; "#\\newline"; "#\\tab"];;
let expected_keywords = [
                {index_from = 0; index_to = 6; found = ScmSymbol "lambda"};
                {index_from = 0; index_to = 2; found = ScmSymbol "if"};
                {index_from = 0; index_to = 7; found = ScmChar ' '};
                {index_from = 0; index_to = 8; found = ScmChar '\r'};
                {index_from = 0; index_to = 9; found = ScmChar '\n'};
                {index_from = 0; index_to = 5; found = ScmChar '\t'}
];;

(*"#\\ ponpon", "#\\ gafrur"*)

(*                Exception: PC.X_no_match.,*)
(*                Exception: PC.X_no_match.*)
let arguments_numbers = [
"1234";
"00001234";
"00001234e0";
"2/3";
"2/0";
"2/6";
"1.234";
"1.234e1";
"1.234e+1";
"1.234*10^+1";
"1.234*10^1";
"1.234*10^-1";
".1234e-10";
".1234*10**-10";
".1234*10^-10";
"-.1234*10^-10"];;
let expected_numbers = [
{index_from = 0; index_to = 4; found = ScmNumber (ScmRational (1234 , 1))};
{index_from = 0; index_to = 8; found = ScmNumber (ScmRational (1234 , 1))};
{index_from = 0; index_to = 10; found = ScmNumber (ScmReal 1234.)};
{index_from = 0; index_to = 3; found = ScmNumber (ScmRational (2, 3))};
{index_from = 0; index_to = 3; found = ScmSymbol "2/0"};
{index_from = 0; index_to = 3; found = ScmNumber (ScmRational (1, 3))};
{index_from = 0; index_to = 5; found = ScmNumber (ScmReal 1.234)};
{index_from = 0; index_to = 7; found = ScmNumber (ScmReal 12.34)};
{index_from = 0; index_to = 8; found = ScmNumber (ScmReal 12.34)};
{index_from = 0; index_to = 11; found = ScmNumber (ScmReal 12.34)};
{index_from = 0; index_to = 10; found = ScmNumber (ScmReal 12.34)};
{index_from = 0; index_to = 11; found = ScmNumber (ScmReal 0.12340000000000001)};
{index_from = 0; index_to = 9; found = ScmNumber (ScmReal 1.234e-11)};
{index_from = 0; index_to = 13; found = ScmNumber (ScmReal 1.234e-11)};
{index_from = 0; index_to = 12; found = ScmNumber (ScmReal 1.234e-11)};
{index_from = 0; index_to = 13; found = ScmNumber (ScmReal ( -1.234e-11))}
];;

let arguments_pairs = ["()"; "#()"; "(1 . 2)"; "(1.2)"; "#(1.2)"; "#(1 2)"];;
let expected_pairs = [
{index_from = 0; index_to = 2; found = ScmNil};
{index_from = 0; index_to = 3; found = ScmVector []};
{index_from = 0; index_to = 7; found = ScmPair (ScmNumber (ScmRational (1, 1)), ScmNumber (ScmRational (2, 1)))};
{index_from = 0; index_to = 5; found = ScmPair (ScmNumber (ScmReal 1.2), ScmNil)};
{index_from = 0; index_to = 6; found = ScmVector [ScmNumber (ScmReal 1.2)]};
{index_from = 0; index_to = 6; found = ScmVector [ScmNumber (ScmRational (1, 1)); ScmNumber (ScmRational (2, 1))]}
];;

let arguments_pairs_2 = ["#(a b c)"; "(a b c)"; "(a b . c)"; "((a . #t) (b . #f))" ];;
let expected_pairs_2 = [
{index_from = 0; index_to = 8; found = ScmVector [ScmSymbol "a"; ScmSymbol "b"; ScmSymbol "c"]};
{index_from = 0; index_to = 7; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))};
{index_from = 0; index_to = 9; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmSymbol "c"))};
{index_from = 0; index_to = 19; found = ScmPair (ScmPair (ScmSymbol "a", ScmBoolean true), ScmPair (ScmPair (ScmSymbol "b", ScmBoolean false), ScmNil))}
];;

let arguments_pairs_3 = ["#( ) "; " ( ) "; "(define a 3)"; "\"~{(+ 2 3)}\""; "\"~{ (+ 2 3) }\""; "\"2 + 3 = ~{(+ 2 3)}\""]
let expected_pairs_3 = [
{index_from = 0; index_to = 12; found = ScmVector []};
{index_from = 0; index_to = 12; found = ScmNil};
{index_from = 0; index_to = 12; found = ScmPair (ScmSymbol "define", ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil)))};
{index_from = 0; index_to = 12; found = ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmNumber (ScmRational (2, 1)), ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))), ScmNil)))};
{index_from = 0; index_to = 19; found = ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmNumber (ScmRational (2, 1)), ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))), ScmNil)))};
{index_from = 0; index_to = 20; found = ScmPair (ScmSymbol "string -append", ScmPair (ScmString "2 + 3 = ", ScmPair (ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmNumber (ScmRational (2, 1)),ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))), ScmNil))), ScmNil)))}
];;

let arguments_quotes = ["`(,a ,@b)"; "'a"; "''a"; "'''a"; "```a"; ",@a"; ",@,@,@a"; "(( lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))"]
let expected_quotes = [
{index_from = 0; index_to = 9; found = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "a", ScmNil)), ScmPair (ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "b", ScmNil)), ScmNil)), ScmNil))};
{index_from = 0; index_to = 2; found = ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil))};
{index_from = 0; index_to = 3; found = ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil))};
{index_from = 0; index_to = 4; found = ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)), ScmNil))};
{index_from = 0; index_to = 4; found = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair  (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)), ScmNil))};
{index_from = 0; index_to = 3; found = ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "a", ScmNil ))};
{index_from = 0; index_to = 7; found = ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)), ScmNil))};
{index_from = 0; index_to = 48; found = ScmPair (ScmPair (ScmSymbol "lambda", ScmPair (ScmPair (ScmSymbol "x", ScmNil), ScmPair (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)) , ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)), ScmNil)), ScmNil)), ScmNil)), ScmNil))), ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "lambda", ScmPair (ScmPair (ScmSymbol "x", ScmNil), ScmPair (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)), ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)), ScmNil)), ScmNil)), ScmNil)), ScmNil))), ScmNil)), ScmNil))}
];;

let arguments_strings_2 = ["\"This is static: ABC and this is dynamic: ~{\"
even though the string is static *in Scheme*, it is interpolated , so
we consider it dynamic ...\"}\""; "\"static ~{'dynamic} more static ~{'(more
dynamic !)} \"";"
;;; This is a line comment!
#;\"and this is an S-expression (string) that is removed via a sexpr -
comment !\"
(a b c
mary had a little lambda!
#;#;#;#;\"I bet you didn 't realize that sexpr -comments\"
\"may be\" \"nested !\"
\"so all four strings shall be dumped and not appear in the list!\"
)
"; "(you should know {that you can have paired/
matching comments too , and that these are entered using braces !})"; "({and {that {these too }}} may be nested!)"; "(a {#\\}} b c)"; "(a {#\\{} b c)"; "(a {\"}}}}{{{{\"} b c)"
];;

let expected_strings_2 = [
{index_from = 0; index_to = 142; found = ScmPair (ScmSymbol "string-append", ScmPair (ScmString "This is static: ABC and this is dynamic: ", ScmPair (ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmString "even though the string is static *in Scheme*, it is interpolated , so we consider it dynamic ...", ScmNil))), ScmNil)))};
{index_from = 0; index_to = 53; found = ScmPair (ScmSymbol "string-append", ScmPair (ScmString "static ", ScmPair (ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "dynamic", ScmNil )), ScmNil))), ScmPair (ScmString " more static ",ScmPair(ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair(ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "more", ScmPair (ScmSymbol "dynamic!", ScmNil)), ScmNil)), ScmNil))), ScmPair (ScmString " ", ScmNil))))))};
{index_from = 0; index_to = 290; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "mary", ScmPair (ScmSymbol "had", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "little", ScmPair (ScmSymbol "lambda!", ScmNil))))))))};
{index_from = 0; index_to = 108; found = ScmPair (ScmSymbol "you", ScmPair (ScmSymbol "should", ScmPair (ScmSymbol "know", ScmNil)))};
{index_from = 0; index_to = 41; found = ScmPair (ScmSymbol "may", ScmPair (ScmSymbol "be", ScmPair (ScmSymbol "nested!", ScmNil)))};
{index_from = 0; index_to = 13; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))};
{index_from = 0; index_to = 13; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))};
{index_from = 0; index_to = 20; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}
];;

let all_arguments = [arguments_boolean; arguments_ascii; arguments_keywords; arguments_numbers; arguments_pairs; arguments_pairs_2; arguments_pairs_3; arguments_quotes; arguments_strings; arguments_strings_2]
let all_expected = [expected_boolean; expected_ascii; expected_keywords; expected_numbers; expected_pairs; expected_pairs_2; expected_pairs_3; expected_quotes; expected_strings; expected_strings_2]

let print_failed arg expected =
    begin
    	Format.printf "@{<red>test failed@}\n";
(*    	print_endline (Printf.sprintf "expected %s" string_of_sexpr(expected))*)
(*    	print_endline (Printf.sprintf "expected %s but found %s" arg string_of_sexpr(expected))*)
    end


let rec test_single_type arguments expected =
  match arguments, expected with
  | [], [] -> ()
  | arg1::rest_args, expected1::rest_expected ->
  begin
    print_endline (Printf.sprintf "testing '%s'" arg1);
    let actual = (test_string nt_sexpr (Printf.sprintf "%s" arg1) 0).found in
    if actual = expected1.found then Format.printf "@{<green>success@}\n" else (print_failed arg1 expected1.found);
    test_single_type rest_args rest_expected
  end
    | _ -> failwith "test_single_type: lists of arguments and expected values must have the same length"

let rec test_all_arguments arguments_list expected_list =
  match arguments_list, expected_list with
  | [], [] -> ()
  | arguments::rest_arguments, expected::rest_expected ->
  begin
    test_single_type arguments expected;
  	test_all_arguments rest_arguments rest_expected
  end
  | _ -> failwith "test_all_arguments: lists of arguments and expected values must have the same length"

let test = test_all_arguments all_arguments all_expected;;

