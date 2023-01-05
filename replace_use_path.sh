#!/bin/bash

ron_path="/home/ronlv4/repos/compilation_assignment"
or_path="/home/spl211/compilation_assignment_1"

if [[ $1 == "ron" ]]; then
  current_path=$ron_path
  path=$or_path
else
  current_path=$or_path
  path=$ron_path
fi

for file in $(find ${current_path} -name "*.ml" -type f)
do
  sed -i "s|${path}|${current_path}|g" $file
done
