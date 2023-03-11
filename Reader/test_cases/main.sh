#!/bin/bash

dir_student="in_to_student"
dir_scheme="in_to_scheme"

process_id=$(ocaml $dir_scheme/check_script.ml $dir_student)
wait $process_id
scheme_process_id=$(test.scm $dir_student)
wait $scheme_process_id

all_passed=true
for file_name in $(ls ${dir_student} | grep -P ".out$")
do
	student_string=$(cat ${dir_student}/${file_name})
	scheme_string=$(cat ${dir_scheme}/${file_name}) 
	if [ "${scheme_string}" != "${student_string}" ]; then
		echo "Failed test"
		echo "scheme string: ${scheme_string}"
		echo -e "student string: ${student_string}\n\n"
		all_passed=false
	fi
done

if [ ${all_passed} = true ]; then
	echo "all tests passed successfuly"
fi

