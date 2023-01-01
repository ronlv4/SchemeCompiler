#!/bin/bash

ron_path="/home/ronlv4/repos/compilation_assignment"
or_path="/home/compilation_assignment_1"

if [[ $1 == "ron" ]]; then
  current_path=$ron_path
  path=$or_path
else
    current_path=$or_path
  path=$ron_path
fi

for file in $(find ${path} -name "*.ml" -type f)
do
  sed -i "s|${current_path}|${path}|g" $file
done
