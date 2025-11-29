# Advent of Code 2025

Solutions for the [Advent of Code 2025](https://adventofcode.com/2025)🎄 puzzles in F#.

Consists of 3 projects:

- `AoC2025.Lib` contains solutions. There is a separate module for each day. The entrypoint function for each part accepts parsed input.
- `AoC2025.Tests` contains tests. There is a separate test class for each day. It includes samples from instructions and any other tests needed during development.
- `AoC2025.CLI` is a console application that runs the solution for each part of each day on the provided input data and outputs the calculated results. It is responsible for all input parsing.
