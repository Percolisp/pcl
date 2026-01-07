#!/bin/bash
# run-pcl.sh - Transpile Perl to Common Lisp and run with SBCL

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CL_DIR="$SCRIPT_DIR/cl"

usage() {
    echo "Usage: $0 <file.pl> [--transpile-only]"
    echo "   or: $0 --test   (run runtime tests)"
    echo ""
    echo "Transpiles Perl to Common Lisp and executes with SBCL."
    exit 1
}

# Check for SBCL
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL not found. Install with: sudo apt install sbcl"
    exit 1
fi

# Handle --test flag
if [ "$1" = "--test" ]; then
    echo "Running PCL runtime tests..."
    cd "$CL_DIR"
    sbcl --script test-runtime.lisp
    exit $?
fi

# Check arguments
if [ $# -lt 1 ]; then
    usage
fi

INPUT_FILE="$1"
TRANSPILE_ONLY=false

if [ "$2" = "--transpile-only" ]; then
    TRANSPILE_ONLY=true
fi

# Check input file exists
if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: File not found: $INPUT_FILE"
    exit 1
fi

# Transpile
echo ";; Transpiling $INPUT_FILE..."
CL_CODE=$("$SCRIPT_DIR/pl2cl" "$INPUT_FILE")

if [ "$TRANSPILE_ONLY" = true ]; then
    echo "$CL_CODE"
    exit 0
fi

# Create temporary file with runtime + transpiled code
TEMP_FILE=$(mktemp /tmp/pcl-XXXXXX.lisp)
trap "rm -f $TEMP_FILE" EXIT

cat > "$TEMP_FILE" << EOF
;;; PCL Generated Code
;;; Source: $INPUT_FILE

(load "$CL_DIR/pcl-runtime.lisp")
(in-package :pcl)

$CL_CODE
EOF

echo ";; Running with SBCL..."
sbcl --script "$TEMP_FILE"
