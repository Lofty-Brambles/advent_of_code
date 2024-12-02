#!/bin/bash

# --- config ---

YEAR=2024

# --- /config ---

COMMANDS=("help new run")
AOC_BASE_URL="https://adventofcode.com/$YEAR/"

function help() {
  echo "--- Advent of Code - ${YEAR} ---

$ ./aoc <command>

--- Commands ---

$ ./aoc new -d <day>
    Generates the templates for that day.
$ ./aoc run -d <day> -p <part> [ --example | -e ]
    Runs the program to generate the solution for said part."
}

function new() {
  declare -A args=(
    ["day"]=$(date +%-d)
  )

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --day|-d)
        args["day"]="$2"
        shift 2
        ;;
    esac
  done

  if (( args["day"] < 0 && args["day"] > 25 )); then
    echo "Problems with the day argument: ${args["day"]}"
    exit 1
  fi

  echo "Creating a new Advent of Code template for day ${args["day"]}."
  
  local folder="day_${args["day"]}"
  mkdir -p "$folder"

  if [ -f "./.env" ]; then
    echo "found .env file..."
    export "$(grep -v '^#' .env | xargs)"
  fi

  if [ -z "$AOC_SESSION" ]; then
    echo "AOC_SESSION is not set. Please provide your Advent of Code session cookie:"
    read -r AOC_SESSION
  fi

  echo "Creating the files..."

  touch "./$folder/example.txt"

  curl -o "./$folder/input.txt" -b "session=${AOC_SESSION}" "$AOC_BASE_URL"day/"${args["day"]}"/input

  local hs_tmpl="module Main where

import System.Environment (getArgs)

type Input = String

type Solution = Int

parser :: String -> Input
parser = undefined

solve1 :: Input -> Solution
solve1 = error "Part 1 Not implemented"

solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
"

  echo "$hs_tmpl" > "./$folder/solution.hs"

  echo "Created a new Advent of Code template for day ${args["day"]}."
}

function run() {
  declare -A args=(
    ["day"]=$(date +%-d)
    ["part"]=""
    ["run_on"]="input"
  )

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --day|-d)
        args["day"]="$2"
        shift 2
        ;;
      --part|-p)
        args["part"]="$2"
        shift 2
        ;;
      --example|-e)
        args["runs_on"]="example"
        shift 2
        ;;
    esac
  done

  if (( args["day"] < 0 && args["day"] > 25 )); then
    echo "Problems with the day argument: ${args["day"]}"
    exit 1
  fi

  if (( args["part"] != 1 || args["part"] != 2 )); then
    echo "Problems with the part argument: ${args["part"]}"
    exit 1
  fi

  local folder="day_${args["day"]}"
  local input_file="./$folder/${args["runs_on"]}.txt"

  if [ ! -f $input_file ]; then
    echo "input file $input_file does not exist."
    exit 1
  fi

  echo "Running solution for day ${args["day"]}, part ${args["part"]}"
  runhaskell "./$folder/solution.hs" "${args["part"]}" "$input_file"
}

if [[ " ${COMMANDS[*]} " =~ [[:space:]]$1[[:space:]] ]]; then
  "$@"
else
  echo "Unknown command: ./aoc.sh $1
Run './aoc help' to get all details."
fi