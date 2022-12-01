<p align="center">
  <img src="https://gitlab.crans.org/loutr/proost/-/raw/48-first-release-preparations/docs/media/logo.png" width="25%"/>
</p>

# Proost

A simple proof assistant written in Rust.

The specification of the project may be found [here](docs/specs.pdf).

The documentation, generated with `rust-doc` may be found [here](doc/proost/).

### Usage 
Please see the specification for insights on how to use `proost` and `tilleul`.

### Build and install
With `nix` installed, simply type `nix run git+ssh://git@gitlab.crans.org/loutr/proost.git?ref=main` to launch `proost`. Alternatively, clone this git repository and type `nix build` to perform a build and have it in the nix store. One can also type `nix profile install` in the repo to install `proost` to one's profile and use it everywhere!

### Development environment
With `nix` installed, simply type `nix develop`. This provides an environment with all the necessary tools, including `clippy` and `rustfmt`. There, it is possible to run the usual `cargo build` and so on.

Please consider the syntax `nix develop --profile <a-file-of-your-choosing>`, which will prevent the garbage collection of the development dependencies.

### Crates dependencies
```mermaid
graph TD;
  kernel-->tilleul;
  kernel-->parser;
  parser-->tilleul; 
  parser-->proost;
  kernel-->proost;
```
