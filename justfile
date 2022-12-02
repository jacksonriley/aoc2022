default:
  just --list

# Build all days
alias b := build
build:
    dune build --profile release

# Run a specific day
run DAY: build
    #!/usr/bin/env sh
    # Zero pad e.g. "2" -> "day02.exe"
    ./_build/default/bin/day$(printf %02d {{DAY}}).exe

# Autoformat
fmt:
    dune fmt || true