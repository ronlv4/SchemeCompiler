#use "/home/ronlv4/repos/compilation_assignment/Reader/Reader.ml";;
open Reader;;

type 'a success_test = {str: string; start_index: int; expected_result: 'a parsing_result};;

type failure_test = {str: string; start_index: int};;

let success_tests = [
        {str = "(a { comment \" } b c)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 21; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))} };
        {str = "#t" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmBoolean true}} ;
        {str = "#T" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmBoolean true}} ;
        {str = "#f" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmBoolean false}} ;
        {str = "#F" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmBoolean false}} ;
        {str = "#\\a" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmChar 'a'}} ;
        {str = "#\\A" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmChar 'A'}} ;
        {str = "\"\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmString ""}} ;
        {str = "\"moshe!\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 8; found = ScmString "moshe!"}} ;
        {str = "\"moshe!\\n\\t\\r\\f\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 16; found = ScmString "moshe!\n\t\r\012"}} ;
        {str = "\"The letter 'a' can be entered as \\x61;\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 40; found = ScmString "The letter 'a' can be entered as a"}} ;
        {str = "\"The letter 'A' can be entered as \\x41;\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 40; found = ScmString "The letter 'A' can be entered as A"}} ;
        {str = "lambda" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 6; found = ScmSymbol "lambda"}} ;
        {str = "if" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmSymbol "if"};} ;
        {str = "#\\space" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 7; found = ScmChar ' '};} ;
        {str = "#\\return" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 8; found = ScmChar '\r'}} ;
        {str = "#\\newline" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 9; found = ScmChar '\n'}} ;
        {str = "#\\tab" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 5; found = ScmChar '\t'}} ;
        {str = "#\\\\" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmChar '\\'}} ;
        {str = "#\\\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmChar '"'}} ;
        {str = "#\\x41" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 5; found = ScmChar 'A'}} ;
        {str = "#\\x20" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 5; found = ScmChar ' '}} ;
        {str = "#\\x61" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 5; found = ScmChar 'a'}} ;
        {str = "1234" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 4; found = ScmNumber (ScmRational (1234, 1))}} ;
        {str = "00001234" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 8; found = ScmNumber (ScmRational (1234, 1))}} ;
        {str = "00001234e0" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 10; found = ScmNumber (ScmReal 1234.)}} ;
        {str = "2/3" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmNumber (ScmRational (2, 3))}} ;
        {str = "2/0" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmSymbol "2/0"}} ;
        {str = "2/6" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmNumber (ScmRational (1, 3))}} ;
        {str = "1.234" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 5; found = ScmNumber (ScmReal 1.234)}} ;
        {str = "1.234e1" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 7; found = ScmNumber (ScmReal 12.34)}} ;
        {str = "1.234e+1" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 8; found = ScmNumber (ScmReal 12.34)}} ;
        {str = "1.234*10^+1" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 11; found = ScmNumber (ScmReal 12.34)}} ;
        {str = "1.234*10^1" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 10; found = ScmNumber (ScmReal 12.34)}} ;
        {str = "1.234*10^-1" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 11; found = ScmNumber (ScmReal 0.12340000000000001)}} ;
        {str = ".1234e-10" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 9; found = ScmNumber (ScmReal 1.234e-11)}} ;
        {str = ".1234*10**-10" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 13; found = ScmNumber (ScmReal 1.234e-11)}} ;
        {str = ".1234*10^-10" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 12; found = ScmNumber (ScmReal 1.234e-11)}} ;
        {str = "-.1234*10^-10" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 13; found = ScmNumber (ScmReal (-1.234e-11))}} ;
        {str = "()" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmNil}} ;
        {str = "#()" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmVector []}} ;
        {str = "(1 . 2)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 7; found = ScmPair (ScmNumber (ScmRational (1, 1)), ScmNumber (ScmRational (2, 1)))}} ;
        {str = "(1.2)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 5; found = ScmPair (ScmNumber (ScmReal 1.2), ScmNil)}} ;
        {str = "#(1.2)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 6; found = ScmVector [ScmNumber (ScmReal 1.2)]}} ;
        {str = "#(1 2)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 6; found = ScmVector [ScmNumber (ScmRational (1, 1)); ScmNumber (ScmRational (2, 1))]}} ;
        {str = "#(a b c)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 8; found = ScmVector [ScmSymbol "a"; ScmSymbol "b"; ScmSymbol "c"]}} ;
        {str = "(a b c)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 7; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}} ;
        {str = "(a b . c)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 9; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmSymbol "c"))}} ;
        {str = "((a . #t) (b . #f))" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 19; found = ScmPair (ScmPair (ScmSymbol "a", ScmBoolean true), ScmPair (ScmPair (ScmSymbol "b", ScmBoolean false), ScmNil))}} ;
        {str = "   #(   )   " ; start_index = 0 ; expected_result = {index_from = 0; index_to = 12; found = ScmVector []}} ;
        {str = "    (   )   " ; start_index = 0 ; expected_result = {index_from = 0; index_to = 12; found = ScmNil}} ;
        {str = "(define a 3)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 12; found = ScmPair (ScmSymbol "define", ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil)))}} ;
        {str = "\"~{(+ 2 3)}\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 12; found = ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmNumber (ScmRational (2, 1)), ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))), ScmNil)))}} ; 
        {str = "\"~{   (+ 2 3)    }\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 19; found = ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmNumber (ScmRational (2, 1)), ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))), ScmNil)))}} ;
        {str = "`(,a ,@b)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 9; found = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "a", ScmNil)), ScmPair (ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "b", ScmNil)), ScmNil)), ScmNil))}} ;
        {str = "'a" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 2; found = ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil))}} ;
        {str = "''a" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil))}} ;
        {str = "'''a" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 4; found = ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)), ScmNil))}} ;
        {str = "```a" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 4; found = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)), ScmNil))}} ;
        {str = ",@a" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 3; found = ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "a", ScmNil))}} ;
        {str = ",@,@,@a" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 7; found = ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)), ScmNil))}} ;
        {str = "((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 48; found = ScmPair (ScmPair (ScmSymbol "lambda", ScmPair (ScmPair (ScmSymbol "x", ScmNil), ScmPair (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)) , ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)), ScmNil)), ScmNil)), ScmNil)), ScmNil))), ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "lambda", ScmPair (ScmPair (ScmSymbol "x", ScmNil), ScmPair (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)), ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)), ScmNil)), ScmNil)), ScmNil)), ScmNil))), ScmNil)), ScmNil))}} ;
        {str = "\"2 + 3 = ~{(+ 2 3)}\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 20; found = ScmPair (ScmSymbol "string-append", ScmPair (ScmString "2 + 3 = ", ScmPair (ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmNumber (ScmRational (2, 1)), ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))), ScmNil))), ScmNil)))}} ;
        {str = "\"This is static: ABC and this is dynamic: ~{\"even though the string is static *in Scheme*, it is interpolated, so we consider it dynamic...\"}\"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 142; found = ScmPair (ScmSymbol "string-append", ScmPair (ScmString "This is static: ABC and this is dynamic: ", ScmPair (ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmString "even though the string is static *in Scheme*, it is interpolated, so we consider it dynamic...", ScmNil))), ScmNil)))}} ;
        {str = "\"static ~{'dynamic} more static ~{'(more dynamic!)} \"" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 53; found = ScmPair (ScmSymbol "string-append", ScmPair (ScmString "static ", ScmPair (ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "dynamic", ScmNil )), ScmNil))), ScmPair (ScmString " more static ", ScmPair (ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (ScmPair (ScmSymbol "quote", ScmPair (ScmPair (ScmSymbol "more", ScmPair (ScmSymbol "dynamic!", ScmNil)), ScmNil)), ScmNil))), ScmPair (ScmString " ", ScmNil))))))}} ;
        {str = "

;;; This is a line comment!
#;\"and this is an S-expression (string) that is removed via a sexpr-
comment!\"
(a b c
mary had a little lambda!
#;#;#;#;\"I bet you didn't realize that sexpr-comments\"
\"may be\" \"nested!\"
\"so all four strings shall be dumped and not appear in the list!\"
)

" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 285; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "mary", ScmPair (ScmSymbol "had", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "little", ScmPair (ScmSymbol "lambda!", ScmNil))))))))}} ;
        {str = "(you should know {that you can have paired/matching comments too, and that these are entered using braces!})" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 108; found = ScmPair (ScmSymbol "you", ScmPair (ScmSymbol "should", ScmPair (ScmSymbol "know", ScmNil)))}} ; 
        {str = "({and {that {these too}}} may be nested!)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 41; found = ScmPair (ScmSymbol "may", ScmPair (ScmSymbol "be", ScmPair (ScmSymbol "nested!", ScmNil)))}} ; 
        {str = "(a {#\\}} b c)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 13; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}} ;
        {str = "(a {#\\{} b c)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 13; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}} ; 
        {str = "(a {\"}}}}{{{{\"} b c)" ; start_index = 0 ; expected_result = {index_from = 0; index_to = 20; found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}}
];;

let failure_tests = [
        {str = "#\\ponpon" ; start_index = 0} ;
        {str = "#\\gafrur" ; start_index = 0}
];;
