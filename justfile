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

# Set up boilerplate for a new day (still need to add to bin/dune :( )
init DAY:
    #!/usr/bin/env sh
    zp=$(printf %02d {{DAY}})
    zp_day=day${zp}
    touch bin/${zp_day}.ml
    touch input/${zp}
    mkdir -p test/${zp_day}.t
    touch test/${zp_day}.t/run.t
    touch test/${zp_day}.t/test
    echo "  \$ ${zp_day} < test" > test/${zp_day}.t/run.t
    code bin/${zp_day}.ml
