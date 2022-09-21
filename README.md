# Proost

A simple proof assistant written in Rust.


### Development environment
With `nix` installed, simply type `nix develop`. This provides an environment
with all the necessary tools, including `clippy` and `rustfmt`. There, it is
possible to run the usual `cargo build` and so on.

Please consider the syntax `nix develop --profile <a-file-of-your-choosing>`, which
will prevent the garbage collection of the development dependencies.


### Build and install
With `nix` installed, type `nix build` to perform a build and have it in the nix
store. One can also type `nix profile install` in the repo to install *proost*
to your profile and use it everywhere!
