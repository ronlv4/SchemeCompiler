let arguments_boolean = ("#t", "#T", "#f", "#F");;
let expected_boolean = (
                {index_from = 0; index_to = 2; found = ScmBoolean true},
                {index_from = 0; index_to = 2; found = ScmBoolean true},
                {index_from = 0; index_to = 2; found = ScmBoolean false},
                {index_from = 0; index_to = 2; found = ScmBoolean false},
);;

let arguments_strings = ("#\\a", "#\\A", "\"\"", "\"moshe !\"", "\"moshe !\\n\\t\\r\\f\"");;
let expected_strings = (
                {index_from = 0; index_to = 3; found = ScmChar 'a'},
                {index_from = 0; index_to = 3; found = ScmChar 'A'},
                {index_from = 0; index_to = 1; found = ScmString ""},
                {index_from = 0; index_to = 9; found = ScmString "moshe !"},
                {index_from = 0; index_to = 16; found = ScmString "moshe !\n\t\r\012"},

);;

let arguments_ascii = ("\"The letter 'a' can be entered as \\x61;\"", "\"The letter 'A' can be entered as \\x41;\"");;
let expected_ascii = (
                {index_from = 0; index_to = 40; found = ScmString "The letter 'a' can be entered as a"},
                {index_from = 0; index_to = 40; found = ScmString "The letter 'A' can be entered as A"}
);;

let arguments_keywords = ("lambda", "if", "#\\ space", "#\\ return", "#\\ newline", "#\\ tab", "#\\ ponpon", "#\\ gafrur");;
let expected_keywords = (
                {index_from = 0; index_to = 6; found = ScmSymbol "lambda"},
                {index_from = 0; index_to = 2; found = ScmSymbol "if"},
                {index_from = 0; index_to = 7; found = ScmChar ' '},
                {index_from = 0; index_to = 8; found = ScmChar '\r'},
                {index_from = 0; index_to = 9; found = ScmChar '\n'},
                {index_from = 0; index_to = 5; found = ScmChar '\t'},
                Exception: PC.X_no_match.,
                Exception: PC.X_no_match.
);;

let arguments_slash = ("#\\\\", "#\\\"", "#\\ x41", "#\\ x20", "#\\ x61");;
let expected_slash = (
                {index_from = 0; index_to = 3; found = ScmChar '\\'},
                {index_from = 0; index_to = 3; found = ScmChar '" '},
                {index_from = 0; index_to = 5; found = ScmChar 'A'},
                {index_from = 0; index_to = 5; found = ScmChar ' '},
                {index_from = 0; index_to = 5; found = ScmChar 'a'},
);;


# test_string nt_sexpr "1234" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 4; found = ScmNumber (ScmRational (1234 , 1)
)}
# test_string nt_sexpr "00001234" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8; found = ScmNumber (ScmRational (1234 , 1)
)}
# test_string nt_sexpr "00001234 e0" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 10; found = ScmNumber (ScmReal 1234.)}
# test_string nt_sexpr "2/3" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmNumber (ScmRational (2, 3))}
# test_string nt_sexpr "2/0" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmSymbol "2/0"}
# test_string nt_sexpr "2/6" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmNumber (ScmRational (1, 3))}
# test_string nt_sexpr "1.234" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5; found = ScmNumber (ScmReal 1.234)}
# test_string nt_sexpr "1.234 e1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234e+1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234*10^+1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 11; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234*10^1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 10; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234*10^ -1" 0;;
6
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 11;
found = ScmNumber (ScmReal 0.12340000000000001)}
# test_string nt_sexpr ".1234e-10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 9; found = ScmNumber (ScmReal 1.234e-11)}
# test_string nt_sexpr ".1234*10** -10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13; found = ScmNumber (ScmReal 1.234e-11)}
# test_string nt_sexpr ".1234*10^ -10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12; found = ScmNumber (ScmReal 1.234e-11)}
# test_string nt_sexpr " -.1234*10^ -10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13; found = ScmNumber (ScmReal ( -1.234e-11)
)}
# test_string nt_sexpr "()" 0;;
- : sexpr PC.parsing_result = {index_from = 0; index_to = 2; found =
ScmNil}
# test_string nt_sexpr "#()" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmVector []}
# test_string nt_sexpr "(1 . 2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7;
found =
ScmPair (ScmNumber (ScmRational (1, 1)), ScmNumber (ScmRational (2,
1)))}
# test_string nt_sexpr "(1.2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5;
found = ScmPair (ScmNumber (ScmReal 1.2), ScmNil)}
# test_string nt_sexpr "#(1.2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 6; found = ScmVector [ScmNumber (ScmReal
1.2)]}
# test_string nt_sexpr "#(1 2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 6;
found =
ScmVector [ScmNumber (ScmRational (1, 1)); ScmNumber (ScmRational (2,
1))]}
# test_string nt_sexpr "#(a b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8;
found = ScmVector [ScmSymbol "a"; ScmSymbol "b"; ScmSymbol "c"]}
# test_string nt_sexpr "(a b c)" 0;;
- : sexpr PC.parsing_result =
7
{index_from = 0; index_to = 7;
found =
ScmPair
(ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c",
ScmNil)))}
# test_string nt_sexpr "(a b . c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 9;
found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmSymbol "c")
)}
# test_string nt_sexpr "((a . #t) (b . #f))" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 19;
found =
ScmPair
(ScmPair (ScmSymbol "a", ScmBoolean true),
ScmPair (ScmPair (ScmSymbol "b", ScmBoolean false), ScmNil))}
# test_string nt_sexpr " #( ) " 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12; found = ScmVector []}
# test_string nt_sexpr " ( ) " 0;;
- : sexpr PC.parsing_result = {index_from = 0; index_to = 12; found =
ScmNil}
# test_string nt_sexpr "(define a 3)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12;
found =
ScmPair
(ScmSymbol "define",
ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (3, 1)),
ScmNil)))}
# test_string nt_sexpr "\"~{(+ 2 3)}\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12;
found =
ScmPair
(ScmSymbol "format",
ScmPair
(ScmString "~a",
ScmPair
(ScmPair
(ScmSymbol "+",
ScmPair
(ScmNumber (ScmRational (2, 1)),
ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))),
ScmNil)))}
# test_string nt_sexpr "\"~{ (+ 2 3) }\"" 0;;
- : sexpr PC.parsing_result =
8
{index_from = 0; index_to = 19;
found =
ScmPair
(ScmSymbol "format",
ScmPair
(ScmString "~a",
ScmPair
(ScmPair
(ScmSymbol "+",
ScmPair
(ScmNumber (ScmRational (2, 1)),
ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))),
ScmNil)))}
# test_string nt_sexpr " `(,a ,@b)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 9;
found =
ScmPair
(ScmSymbol "quasiquote",
ScmPair
(ScmPair
(ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "a", ScmNil)),
ScmPair
(ScmPair
(ScmSymbol "unquote -splicing", ScmPair (ScmSymbol "b",
ScmNil)),
ScmNil)),
ScmNil))}
# test_string nt_sexpr "'a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2;
found = ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil))}
# test_string nt_sexpr "''a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3;
found =
ScmPair
(ScmSymbol "quote",
ScmPair
(ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
ScmNil))}
# test_string nt_sexpr "'''a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 4;
found =
ScmPair
(ScmSymbol "quote",
ScmPair
9
(ScmPair
(ScmSymbol "quote",
ScmPair
(ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
ScmNil)),
ScmNil))}
# test_string nt_sexpr "```a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 4;
found =
ScmPair
(ScmSymbol "quasiquote",
ScmPair
(ScmPair
(ScmSymbol "quasiquote",
ScmPair
(ScmPair (ScmSymbol "quasiquote", ScmPair (ScmSymbol "a",
ScmNil)),
ScmNil)),
ScmNil))}
# test_string nt_sexpr ",@a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3;
found =
ScmPair (ScmSymbol "unquote -splicing", ScmPair (ScmSymbol "a", ScmNil
))}
# test_string nt_sexpr ",@,@,@a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7;
found =
ScmPair
(ScmSymbol "unquote -splicing",
ScmPair
(ScmPair
(ScmSymbol "unquote -splicing",
ScmPair
(ScmPair
(ScmSymbol "unquote -splicing", ScmPair (ScmSymbol "a",
ScmNil)),
ScmNil)),
ScmNil))}
# test_string nt_sexpr "(( lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))
)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 48;
found =
ScmPair
(ScmPair
10
(ScmSymbol "lambda",
ScmPair
(ScmPair (ScmSymbol "x", ScmNil),
ScmPair
(ScmPair
(ScmSymbol "quasiquote",
ScmPair
(ScmPair
(ScmPair
(ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil))
,
ScmPair
(ScmPair
(ScmSymbol "quote",
ScmPair
(ScmPair
(ScmSymbol "unquote", ScmPair (ScmSymbol "x",
ScmNil)),
ScmNil)),
ScmNil)),
ScmNil)),
ScmNil))),
ScmPair
(ScmPair
(ScmSymbol "quote",
ScmPair
(ScmPair
(ScmSymbol "lambda",
ScmPair
(ScmPair (ScmSymbol "x", ScmNil),
ScmPair
(ScmPair
(ScmSymbol "quasiquote",
ScmPair
(ScmPair
(ScmPair
(ScmSymbol "unquote", ScmPair (ScmSymbol "x",
ScmNil)),
ScmPair
(ScmPair
(ScmSymbol "quote",
ScmPair
(ScmPair
(ScmSymbol "unquote",
ScmPair (ScmSymbol "x", ScmNil)),
ScmNil)),
ScmNil)),
ScmNil)),
11
ScmNil))),
ScmNil)),
ScmNil))}
# test_string nt_sexpr "\"2 + 3 = ~{(+ 2 3)}\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 20;
found =
ScmPair
(ScmSymbol "string -append",
ScmPair
(ScmString "2 + 3 = ",
ScmPair
(ScmPair
(ScmSymbol "format",
ScmPair
(ScmString "~a",
ScmPair
(ScmPair
(ScmSymbol "+",
ScmPair
(ScmNumber (ScmRational (2, 1)),
ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))),
ScmNil))),
ScmNil)))}
# test_string nt_sexpr "\"This is static: ABC and this is dynamic: ~{\"
even though the string is static *in Scheme*, it is interpolated , so
we consider it dynamic ...\"}\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 142;
found =
ScmPair
(ScmSymbol "string -append",
ScmPair
(ScmString "This is static: ABC and this is dynamic: ",
ScmPair
(ScmPair
(ScmSymbol "format",
ScmPair
(ScmString "~a",
ScmPair
(ScmString
"even though the string is static *in Scheme*, it is
interpolated , so we consider it dynamic ...",
ScmNil))),
ScmNil)))}
# test_string nt_sexpr "\"static ~{'dynamic} more static ~{'(more
dynamic !)} \"" 0;;
- : sexpr PC.parsing_result =
12
{index_from = 0; index_to = 53;
found =
ScmPair
(ScmSymbol "string -append",
ScmPair
(ScmString "static ",
ScmPair
(ScmPair
(ScmSymbol "format",
ScmPair
(ScmString "~a",
ScmPair
(ScmPair
(ScmSymbol "quote", ScmPair (ScmSymbol "dynamic", ScmNil
)),
ScmNil))),
ScmPair
(ScmString " more static ",
ScmPair
(ScmPair
(ScmSymbol "format",
ScmPair
(ScmString "~a",
ScmPair
(ScmPair
(ScmSymbol "quote",
ScmPair
(ScmPair
(ScmSymbol "more",
ScmPair (ScmSymbol "dynamic!", ScmNil)),
ScmNil)),
ScmNil))),
ScmPair (ScmString " ", ScmNil))))))}
# test_string nt_sexpr "
;;; This is a line comment!
#;\"and this is an S-expression (string) that is removed via a sexpr -
comment !\"
(a b c
mary had a little lambda!
#;#;#;#;\"I bet you didn 't realize that sexpr -comments\"
\"may be\" \"nested !\"
\"so all four strings shall be dumped and not appear in the list!\"
)
" 0;;
- : sexpr PC.parsing_result =
13
{index_from = 0; index_to = 290;
found =
ScmPair
(ScmSymbol "a",
ScmPair
(ScmSymbol "b",
ScmPair
(ScmSymbol "c",
ScmPair
(ScmSymbol "mary",
ScmPair
(ScmSymbol "had",
ScmPair
(ScmSymbol "a",
ScmPair
(ScmSymbol "little", ScmPair (ScmSymbol "lambda!",
ScmNil))))))))}
# test_string nt_sexpr "(you should know {that you can have paired/
matching comments too , and that these are entered using braces !})"
0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 108;
found =
ScmPair
(ScmSymbol "you",
ScmPair (ScmSymbol "should", ScmPair (ScmSymbol "know", ScmNil)))}
# test_string nt_sexpr "({and {that {these too }}} may be nested !)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 41;
found =
ScmPair
(ScmSymbol "may",
ScmPair (ScmSymbol "be", ScmPair (ScmSymbol "nested!", ScmNil)))}
# test_string nt_sexpr "(a {#\\}} b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13;
found =
ScmPair
(ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c",
ScmNil)))}
# test_string nt_sexpr "(a {#\\{} b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13;
found =
ScmPair
(ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c",
ScmNil)))}
# test_string nt_sexpr "(a {\"}}}}{{{{\"} b c)" 0;;
14
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 20;
found =
ScmPair
(ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c",
ScmNil)))}