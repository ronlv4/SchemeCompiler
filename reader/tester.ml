#use "tests.ml";;

exception X_test_failed of string;;

let test str idx_from expected_success expected_result = 
        try let result = test_string nt_sexpr str idx_from in
                if expected_success then
                        if result.index_from = expected_result.index_from then
                                if result.index_to = expected_result.index_to then
                                        if result.found = expected_result.found then
                                                ()
                                        else
                                                raise (X_test_failed (Printf.sprintf "\nString: %s\nIndex_From: %d\nExpected: Success\nResult: mismatch on found\n" str idx_from))
                                else
                                        raise (X_test_failed (Printf.sprintf "\nString: %s\nIndex_From: %d\nExpected: Success\nResult: mismatch on index_to (Exp: %d, Act: %d)\n" str idx_from expected_result.index_to result.index_to))
                        else
                                raise (X_test_failed (Printf.sprintf "\nString: %s\nIndex_From: %d\nExpected: Success\nResult: mismatch on index_from (Exp: %d, Act: %d)\n" str idx_from expected_result.index_from result.index_from))
                else
                        raise (X_test_failed (Printf.sprintf "\nString: %s\nIndex_From: %d\nExpected: X_no_match\nResult: Success\n" str idx_from))
        with 
        | X_no_match -> 
                if expected_success then
                        raise (X_test_failed (Printf.sprintf "\nString: %s\nIndex_From: %d\nExpected: Success\nResult: X_no_match\n" str idx_from))
                else
                        ()
        | X_not_yet_implemented ->
                if expected_success then 
                        raise (X_test_failed (Printf.sprintf "\nString: %s\nIndex_From: %d\nExpected: Success\nResult: X_not_yet_implemented\n" str idx_from))
                else
                        raise (X_test_failed (Printf.sprintf "\nString: %s\nIndex_From: %d\nExpected: X_no_match\nResult: X_not_yet_implemented\n" str idx_from));;

let run_tests (s_tests : 'a success_test list) (f_tests : failure_test list) =
        let stub_result  = {index_from = 0; index_to = 2; found = ScmVoid} in
        try 
                let run_s = List.fold_left (fun _ (s_test : 'a success_test) -> test s_test.str s_test.start_index true s_test.expected_result) () s_tests in
                let run_f = List.fold_left (fun _ (f_test : failure_test) -> test f_test.str f_test.start_index false stub_result) () f_tests in
                let dis_warn = fun _ _ -> () in
                dis_warn run_s run_f
        with
        | X_test_failed(e) -> Printf.printf "\nError: Test Failed%s" e;;

run_tests success_tests failure_tests

