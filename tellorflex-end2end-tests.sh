#!/bin/bash

# Set the folder name
folder="tests/tellorflex/e2eTests/"

# Set the default value for the flag
flag=0

# Iterate over the arguments
for arg in "$@"
do
    # Check if the argument is the flag
    case "$arg" in
        "-t") flag=1 ;;
        "--show-trace") flag=2 ;;
        *)
        echo "Error: Invalid flag"
        exit 1
            ;;
    esac
done

# Iterate over the files in the folder
for file in $(find "$folder" -type f)
do
  # Check if the flag is set
  if [ $flag -eq 0 ]; then
    pact -r "$file"
  elif [ $flag -eq 1 ] | [ $flag -eq 2 ]; then
    pact -r "$file" -t
  fi
done
