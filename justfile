default:
  just --list

alias b := build
# Build all days
build:
    dune build --profile release

# Run a specific day
run DAY: build
    #!/usr/bin/env sh
    # Zero pad e.g. "2" -> "day02.exe"
    ./_build/default/bin/day$(printf %02d {{DAY}}).exe < input/$(printf %02d {{DAY}})

# Autoformat
fmt:
    dune fmt || true


alias t := test
# Run specific test
test DAY:
    #!/usr/bin/env sh
    dune build @day$(printf %02d {{DAY}})

# Run all tests
test-all:
    dune runtest