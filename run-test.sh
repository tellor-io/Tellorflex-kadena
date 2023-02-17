#!/bin/bash -e

# Set the folder name
folder=0
# Set the default value for the flag
flag=0

if [ $# -eq 0 ]; then
    # echo "Error: Please specify a module (tellorflex, governance, or autopay)"
    echo "Running all tests ..."
    for file in $(find tests/ -type f)
    do
      pact -r "$file"

    done
exit 0
fi

# Iterate over the arguments
for arg in "$@"
do
    # Check if the argument is the flag
    case "$arg" in
        "-t") flag=1 ;;
        "--show-trace") flag=1 ;;
        "tellorflex") folder="tests/tellorflex/" ;;
        "governance") folder="tests/governance/" ;;
        "autopay") folder="tests/autopay/" ;;
        *)
        echo "Error: Invalid flag"
        exit 1
            ;;
    esac
done

for file in $(find "$folder" -type f)
do
  # Check if the flag is set
  if [ $flag -eq 0 ]; then
    pact -r "$file"
  elif [ $flag -eq 1 ] ; then
    pact -r "$file" -t
  fi
done
