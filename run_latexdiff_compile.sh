#!/bin/bash
# Script to create a latexdiff of article.tex and previous version, then compile it

set -e

# Load required modules
module load use.own
module load texlive

# Get current and previous article.tex paths
dir_current=$(pwd)
dir_old="../$(basename "$dir_current")-old"

file_current="$dir_current/article.tex"
file_old="$dir_old/article.tex"
file_diff="$dir_current/article-markup.tex"

# Run latexdiff
latexdiff "$file_old" "$file_current" > "$file_diff"

# Compile the diffed file (3 times, with biber after first)
pdflatex -interaction=nonstopmode "$file_diff"
biber "$(basename "$file_diff" .tex)"
pdflatex -interaction=nonstopmode "$file_diff"
pdflatex -interaction=nonstopmode "$file_diff"

echo "Diff and compilation complete: $file_diff"
