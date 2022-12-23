#use "/home/ronlv4/repos/compilation_assignment/Parsing_Combinators/PC.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen of string;;

let rec is_member a = function
  | [] -> false
  | a' :: s -> (a = a') || (is_member a s);;

let rec gcd a b =
  match (a, b) with
  | (0, b) -> Int.abs b
  | (a, 0) -> Int.abs a
  | (a, b) -> gcd b (a mod b);;

type scm_number =
  | ScmRational of (int * int)
  | ScmReal of float;;

type sexpr =
  | ScmVoid
  | ScmNil
  | ScmBoolean of bool
  | ScmChar of char
  | ScmString of string
  | ScmSymbol of string
  | ScmNumber of scm_number
  | ScmVector of (sexpr list)
  | ScmPair of (sexpr * sexpr);;

type string_part =
    | Static of string
    | Dynamic of sexpr;;

module type READER = sig
  val nt_sexpr : sexpr PC.parser
  val scheme_sexpr_list_of_sexpr_list : sexpr list -> sexpr
end;; (* end of READER signature *)

module Reader : READER = struct
  open PC;;

  let unitify nt = pack nt (fun _ -> ());;

  let rec nt_whitespace str =
    const (fun ch -> ch <= ' ') str
  and nt_end_of_line_or_file str =
    let nt1 = unitify (char '\n') in
    let nt2 = unitify nt_end_of_input in
    let nt1 = disj nt1 nt2 in
    nt1 str
  and nt_line_comment str =
    let nt1 = char ';' in
    let nt2 = diff nt_any nt_end_of_line_or_file in
    let nt2 = star nt2 in
    let nt1 = caten nt1 nt2 in
    let nt1 = caten nt1 nt_end_of_line_or_file in
    let nt1 = unitify nt1 in
    nt1 str
  and nt_paired_comment str =
    let uncomment_sexpr = disj_list [unitify (disj (word "#\\{") (word "#\\}")); unitify nt_string; unitify nt_comment] in
    let nt1 = disj uncomment_sexpr (unitify (one_of "{}")) in
    let nt1 = diff nt_any nt1 in
    let nt1 = disj (unitify nt1) uncomment_sexpr in
    let nt1 = star nt1 in
    let nt1 = unitify (caten (char '{') (caten nt1 (char '}'))) in
    nt1 str
  and nt_sexpr_comment str =
    let nt1 = word "#;" in
    let nt2 = unitify (caten nt1 nt_sexpr) in
    let nt3 = unitify(disj nt2 (unitify (caten nt1 (caten nt_sexpr_comment nt_sexpr))))  in
    nt3 str
  and nt_comment str =
    disj_list
      [nt_line_comment;
       nt_paired_comment;
       nt_sexpr_comment] str
  and nt_void str =
    let nt1 = word_ci "#void" in
    let nt1 = not_followed_by nt1 nt_symbol_char in
    let nt1 = pack nt1 (fun _ -> ScmVoid) in
    nt1 str
  and nt_skip_star str =
    let nt1 = disj (unitify nt_whitespace) nt_comment in
    let nt1 = unitify (star nt1) in
    nt1 str
  and make_skipped_star (nt : 'a parser) =
    let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
    let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
    nt1
  and nt_digit str =
    let nt1 = range '0' '9' in
    let nt1 = pack nt1
                (fun digit_char ->
                  ((int_of_char digit_char) - 48)
                ) in
    nt1 str
  and nt_hex_digit str = (* check if case sensitive *)
    let nt1 = range 'a' 'f' in
    let nt1 = pack nt1
                (fun hex_char ->
                  ((int_of_char hex_char) - 87)
                ) in
    let nt1 = disj nt1 nt_digit in
    nt1 str
  and nt_nat str =
    let nt1 = plus nt_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_left
                    (fun num digit ->
                      10 * num + digit)
                    0
                    digits) in
    nt1 str
  and nt_hex_nat str =
    let nt1 = plus nt_hex_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_left
                    (fun num digit ->
                      16 * num + digit)
                    0
                    digits) in
    nt1 str
  and nt_optional_sign str =
  let nt1 = pack (char '-') (function _ -> false) in
  let nt2 = pack (char '+') (function _ -> true) in
  let nt3 = pack nt_epsilon (fun _ -> true) in
  let nt1 = disj_list [nt1; nt2; nt3] in
  nt1 str
  and nt_int str =
    let nt1 = caten nt_optional_sign nt_nat in
    let nt1 = pack nt1
                (fun (is_positive, n) ->
                  if is_positive then n else -n) in
    nt1 str
  and nt_frac str =
    let nt1 = caten nt_int (char '/') in
    let nt1 = pack nt1 (fun (num, _) -> num) in
    let nt2 = only_if nt_nat (fun n -> n != 0) in
    let nt1 = caten nt1 nt2 in
    let nt1 = pack nt1
                (fun (num, den) ->
                  let d = gcd num den in
                  ScmRational(num / d, den / d)) in
    nt1 str
  and nt_integer_part str =
    let nt1 = plus nt_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_left
                    (fun num digit -> 10.0 *. num +. (float_of_int digit))
                    0.0
                    digits) in
    nt1 str
  and nt_mantissa str =
    let nt1 = plus nt_digit in
    let nt1 = pack nt1
                (fun digits ->
                  List.fold_right
                    (fun digit num ->
                      ((float_of_int digit) +. num) /. 10.0)
                    digits
                    0.0) in
    nt1 str
  and nt_exponent str =
    let nt1 = unitify (char_ci 'e') in
    let nt2 = word "*10" in
    let nt3 = unitify (word "**") in
    let nt4 = unitify (char '^') in
    let nt3 = disj nt3 nt4 in
    let nt2 = caten nt2 nt3 in
    let nt2 = unitify nt2 in
    let nt1 = disj nt1 nt2 in
    let nt1 = caten nt1 nt_int in
    let nt1 = pack nt1 (fun (_, n) -> Float.pow 10. (float_of_int n)) in
    nt1 str
  and make_maybe nt none_value =
    pack (maybe nt)
      (function
       | None -> none_value
       | Some(x) -> x)
  and nt_float_a str =
    let nt1 = char '.' in
    let nt2 = make_maybe nt_mantissa 0.0 in
    let nt3 = make_maybe nt_exponent 1.0 in
    let nt1 = caten nt_integer_part (caten nt1 (caten nt2 nt3)) in
    let nt1 = pack nt1 (fun (int_part, (_, (mantissa, exponent))) ->
                            (int_part +. mantissa) *. exponent
                            ) in
    nt1 str
  and nt_float_b str =
    let nt1 = char '.' in
    let nt2 = make_maybe nt_exponent 1.0 in
    let nt1 = caten nt1 (caten nt_mantissa nt2) in
    let nt1 = pack nt1 (fun (_, (mantissa, exponent)) ->
                        mantissa *. exponent) in
    nt1 str
  and nt_float_c str =
    let nt1 = caten nt_integer_part nt_exponent in
    let nt1 = pack nt1 (fun (int_part, exponent) ->
                        int_part *. exponent) in
    nt1 str
  and nt_float str =
    let nt1 = disj_list [nt_float_a; nt_float_b; nt_float_c] in
    let nt1 = caten nt_optional_sign nt1 in
    let nt1 = pack nt1
                (fun (is_positive, n) ->
                  if is_positive then n else -.n) in
    let nt1 = pack nt1 (fun num -> ScmReal(num)) in
    nt1 str
  and nt_number str =
    let nt1 = nt_float in
    let nt2 = nt_frac in
    let nt3 = pack nt_int (fun n -> ScmRational(n, 1)) in
    let nt1 = disj nt1 (disj nt2 nt3) in
    let nt1 = pack nt1 (fun r -> ScmNumber r) in
    let nt1 = not_followed_by nt1 nt_symbol_char in
    nt1 str
  and nt_boolean str =
    let nt1 = char '#' in
    let nt2 = char_ci 'f' in
    let nt2 = pack nt2 (fun _ -> ScmBoolean false) in
    let nt3 = char_ci 't' in
    let nt3 = pack nt3 (fun _ -> ScmBoolean true) in
    let nt2 = disj nt2 nt3 in
    let nt1 = caten nt1 nt2 in
    let nt1 = pack nt1 (fun (_, value) -> value) in
    let nt2 = nt_symbol_char in
    let nt1 = not_followed_by nt1 nt2 in
    nt1 str
  and nt_char_simple str =
    let nt1 = const(fun ch -> ' ' < ch) in
    let nt1 = not_followed_by nt1 nt_symbol_char in
    nt1 str
  and nt_char_named str =
    let nt1 = word_ci "nul" in
    let nt1 = pack nt1 (fun _ -> '\000') in
    let nt2 = word_ci "tab" in
    let nt2 = pack nt2 (fun _ -> '\009') in
    let nt3 = word_ci "newline" in
    let nt3 = pack nt3 (fun _ -> '\010') in
    let nt4 = word_ci "page" in
    let nt4 = pack nt4 (fun _ -> '\012') in
    let nt5 = word_ci "return" in
    let nt5 = pack nt5 (fun _ -> '\013') in
    let nt6 = word_ci "space" in
    let nt6 = pack nt6 (fun _ -> '\032') in
    let nt1 = disj_list [nt1; nt2; nt3; nt4; nt5; nt6] in
    nt1 str
  and nt_char_hex str =
    let nt1 = caten (char_ci 'x') nt_hex_nat in
    let nt1 = pack nt1 (fun (_, n) -> n) in
    let nt1 = only_if nt1 (fun n -> n < 256) in
    let nt1 = pack nt1 (fun n -> char_of_int n) in
    nt1 str
  and nt_char str =
    let nt1 = word "#\\" in
    let nt2 = disj nt_char_simple (disj nt_char_named nt_char_hex) in
    let nt1 = caten nt1 nt2 in
    let nt1 = pack nt1 (fun (_, ch) -> ScmChar ch) in
    nt1 str
  and nt_symbol_char str =
    let nt1 = one_of "!$%&*/:<=>?~_^" in
    let nt2 = one_of ".+@-" in
    let char_digit = range '0' '9' in
    let letter = range_ci 'a' 'z' in
    let letter = pack letter Char.lowercase_ascii in
    let initial = disj letter nt1 in
    let subsequent = disj initial (disj char_digit nt2) in
    let nt1 = caten initial (star subsequent) in
(*    let nt1 = pack nt1 (fun (ch, str) -> ch :: str) in*)
    nt1 str
  and nt_symbol str =
    let nt1 = plus nt_symbol_char in
    let nt1 = pack nt1 (fun chars -> ScmSymbol (string_of_list chars)) in
    nt1 str
  and nt_string_part_simple str =
    let nt1 =
      disj_list [unitify (char '"'); unitify (char '\\'); unitify (word "~~");
                 unitify nt_string_part_dynamic] in
    let nt1 = diff nt_any nt1 in
    nt1 str
  and nt_string_part_meta str =
    let nt1 =
      disj_list [pack (word "\\\\") (fun _ -> '\\');
                 pack (word "\\\"") (fun _ -> '"');
                 pack (word "\\n") (fun _ -> '\n');
                 pack (word "\\r") (fun _ -> '\r');
                 pack (word "\\f") (fun _ -> '\012');
                 pack (word "\\t") (fun _ -> '\t');
                 pack (word "~~") (fun _ -> '~')] in
    nt1 str
  and nt_string_part_hex str =
    let nt1 = word_ci "\\x" in
    let nt2 = nt_hex_nat in
    let nt2 = only_if nt2 (fun n -> n < 256) in
    let nt3 = char ';' in
    let nt1 = caten nt1 (caten nt2 nt3) in
    let nt1 = pack nt1 (fun (_, (n, _)) -> n) in
    let nt1 = pack nt1 char_of_int in
    nt1 str
  and nt_string_part_dynamic str =
    let nt_tilde = char '~' in
    let nt_space = star nt_whitespace in
    let nt_bra = char '{' in
    let nt_ket = char '}' in
    let nt1 = caten nt_tilde (caten nt_bra nt_space) in
    let nt1 = unitify nt1 in
    let nt1 = caten nt1 (caten nt_sexpr (caten nt_space nt_ket)) in
    let nt1 = pack nt1 (fun (_ , (sexpr, (_ , _))) -> Dynamic(ScmPair
    (ScmSymbol "format", ScmPair(ScmString "~a", ScmPair(sexpr, ScmNil))))) in
    nt1 str
  and nt_string_part_static str =
    let nt1 = disj_list [nt_string_part_simple;
                         nt_string_part_meta;
                         nt_string_part_hex] in
    let nt1 = plus nt1 in
    let nt1 = pack nt1 string_of_list in
    let nt1 = pack nt1 (fun str -> Static(str)) in
    nt1 str
  and nt_string_part str =
    disj nt_string_part_static nt_string_part_dynamic str
  and nt_string str =
    let nt1 = char '"' in
    let nt2 = star nt_string_part in
    let nt3 = char '"' in
    let nt1 = caten nt1 (caten nt2 nt3) in
    let nt1 = pack nt1 (fun (_, (parts, _)) -> parts) in
    let nt1 = pack nt1
                (fun parts ->
                  match parts with
                  | [] -> ScmString ""
                  | [Static(str)] -> ScmString str
                  | [Dynamic(sexpr)] -> sexpr
                  | parts ->
                     let argl =
                       List.fold_right
                         (fun car cdr ->
                           ScmPair((match car with
                                    | Static(str) -> ScmString(str)
                                    | Dynamic(sexpr) -> sexpr),
                                   cdr))
                         parts
                         ScmNil in
                     ScmPair(ScmSymbol "string-append", argl)) in
    nt1 str

  and nt_empty_vector str =
    let nt1 = word "#(" in
    let nt2 = caten nt1 (caten nt_skip_star (char ')')) in
    let nt1 = pack nt2 (fun _ -> ScmVector []) in
    nt1 str
  and nt_non_empty_vector str =
    let nt1 = word "#(" in
    let nt2 = plus nt_sexpr in
    let nt3 = char ')' in
    let nt1 = caten nt1 (caten nt2 nt3) in
    let nt1 = pack nt1 (fun (_, (sexprs, _)) -> ScmVector sexprs) in
    nt1 str
  and nt_vector str =
    let nt1 = disj nt_empty_vector nt_non_empty_vector in
    nt1 str
   and nt_proper_list str =
   let nt1 = char '(' in
   let nt2 = star nt_sexpr in
   let nt3 = char ')' in
   let nt1 = caten nt1 (caten nt2 nt3) in
   let nt1 = pack nt1 (fun (_, (sexprs, _)) -> sexprs) in
   let nt1 = pack nt1
                (fun sexprs ->
                  List.fold_right
                    (fun car cdr -> ScmPair(car, cdr))
                    sexprs
                    ScmNil) in
   nt1 str
  and nt_improper_list str =
    let nt1 = char '(' in
    let nt2 = plus nt_sexpr in
    let nt3 = char '.' in
    let nt4 = char ')' in
    let nt1 = caten nt1 (caten nt2 (caten nt3 (caten nt_sexpr nt4))) in
    let nt1 = pack nt1
                 (fun (_, (sexprs, (_, (last_sexpr, _)))) ->
                  List.fold_right
                    (fun car cdr -> ScmPair(car, cdr))
                    sexprs
                    last_sexpr) in
    nt1 str
    and nt_empty_list str =
        let nt1 = char '(' in
        let nt1 = caten nt1 (caten nt_skip_star (char ')')) in
        let nt1 = pack nt1 (fun _ -> ScmNil) in
        nt1 str
  and nt_list str =
    let nt1 = disj_list [nt_empty_list; nt_proper_list; nt_improper_list] in
    nt1 str
  and make_quoted_form nt_qf qf_name =
    let nt1 = caten nt_qf nt_sexpr in
    let nt1 = pack nt1
                (fun (_, sexpr) ->
                  ScmPair(ScmSymbol qf_name,
                          ScmPair(sexpr, ScmNil))) in
    nt1
  and nt_quoted_forms str =
    let nt1 =
      disj_list [(make_quoted_form (unitify (char '\'')) "quote");
                 (make_quoted_form (unitify (char '`')) "quasiquote");
                 (make_quoted_form
                    (unitify (not_followed_by (char ',') (char '@')))
                    "unquote");
                 (make_quoted_form (unitify (word ",@")) "unquote-splicing")] in
    nt1 str
  and nt_sexpr str =
    let nt1 =
      disj_list [nt_void; nt_number; nt_boolean; nt_char; nt_symbol;
                 nt_string; nt_vector; nt_list; nt_quoted_forms] in
    let nt1 = make_skipped_star nt1 in
    nt1 str;;

  let scheme_sexpr_list_of_sexpr_list sexprs =
    List.fold_right (fun car cdr -> ScmPair (car, cdr)) sexprs ScmNil;;

end;; (* end of struct Reader *)


let rec string_of_sexpr = function
  | ScmVoid -> "#<void>"
  | ScmNil -> "()"
  | ScmBoolean(false) -> "#f"
  | ScmBoolean(true) -> "#t"
  | ScmChar('\n') -> "#\\newline"
  | ScmChar('\r') -> "#\\return"
  | ScmChar('\012') -> "#\\page"
  | ScmChar('\t') -> "#\\tab"
  | ScmChar(' ') -> "#\\space"
  | ScmChar(ch) ->
     if (ch < ' ')
     then let n = int_of_char ch in
          Printf.sprintf "#\\x%x" n
     else Printf.sprintf "#\\%c" ch
  | ScmString(str) ->
     Printf.sprintf "\"%s\""
       (String.concat ""
          (List.map
             (function
              | '\n' -> "\\n"
              | '\012' -> "\\f"
              | '\r' -> "\\r"
              | '\t' -> "\\t"
              | '\"' -> "\\\""
              | ch ->
                 if (ch < ' ')
                 then Printf.sprintf "\\x%x;" (int_of_char ch)
                 else Printf.sprintf "%c" ch)
             (list_of_string str)))
  | ScmSymbol(sym) -> sym
  | ScmNumber(ScmRational(0, _)) -> "0"
  | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
  | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
  | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
  | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
  | ScmVector(sexprs) ->
     let strings = List.map string_of_sexpr sexprs in
     let inner_string = String.concat " " strings in
     Printf.sprintf "#(%s)" inner_string
  | ScmPair(ScmSymbol "quote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "'%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "quasiquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "`%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote-splicing",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",@%s" (string_of_sexpr sexpr)
  | ScmPair(car, cdr) ->
     string_of_sexpr' (string_of_sexpr car) cdr
and string_of_sexpr' car_string = function
  | ScmNil -> Printf.sprintf "(%s)" car_string
  | ScmPair(cadr, cddr) ->
     let new_car_string =
       Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
     string_of_sexpr' new_car_string cddr
  | cdr ->
     let cdr_string = (string_of_sexpr cdr) in
     Printf.sprintf "(%s . %s)" car_string cdr_string;;

(* print_sexpr : out_channel -> sexpr -> unit *)
let print_sexpr chan sexpr = output_string chan (string_of_sexpr sexpr);;

(* print_sexprs : out_channel -> sexpr list -> unit *)
let print_sexprs chan sexprs =
  output_string chan
    (Printf.sprintf "[%s]"
       (String.concat "; "
          (List.map string_of_sexpr sexprs)));;

(* sprint_sexpr : 'a -> sexpr -> string *)
let sprint_sexpr _ sexpr = string_of_sexpr sexpr;;

(* sprint_sexprs : 'a -> sexpr list -> string *)
let sprint_sexprs chan sexprs =
  Printf.sprintf "[%s]"
    (String.concat "; "
       (List.map string_of_sexpr sexprs));;