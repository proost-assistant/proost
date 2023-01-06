{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-unstable";

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, devshell, flake-utils, rust-overlay, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ devshell.overlay (import rust-overlay) ];
        pkgs = import nixpkgs { inherit overlays system; };
        rust = pkgs.rust-bin.nightly.latest;
      in rec {
        packages = {
          default = pkgs.rustPlatform.buildRustPackage {
            pname = "proost";
            version = "0.1.0";

            nativeBuildInputs = [ rust.minimal ];

            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;

            meta = with pkgs.lib; {
              description = "A simple proof assistant written in Rust";
              homepage = "https://gitlab.crans.org/loutr/proost";
              license = licenses.gpl3;
            };
          };

          docker-ci = let
            rust-ci = rust.minimal.override { extensions = [ "clippy" "rustfmt" ]; };
          in pkgs.dockerTools.buildImage {
            name = "proost-ci";

            config.Entrypoint = [ "${pkgs.dockerTools.binSh}/bin/sh" "-c" ];

            copyToRoot = pkgs.buildEnv {
              name = "proost-dependencies";
              paths = (with pkgs; [ cargo-deny coreutils gcc gnugrep gnused grcov lcov libxslt openssh rsync ])
                ++ (with pkgs.dockerTools; [ binSh caCertificates fakeNss ]) ++ [ rust-ci ];
              pathsToLink = [ "/bin" "/etc" ];
            };

            runAsRoot = "mkdir /tmp";
          };
        };

        devShell = let
          rust-dev = rust.default.override { extensions = [ "rust-src" "rust-analyzer" ]; };
        in pkgs.devshell.mkShell {
          name = "proost";

          commands = [{
            name = "coverage";
            command = let 
              excl_enum_struct = "^([[:space:]]*)(pub |pub(([[:alpha:]]|[[:space:]]|[:])+) )?(enum|struct) ";
              excl_enum_fn_struct = "^([[:space:]]*)(pub |pub(([[:alpha:]]|[[:space:]]|[:])+) )?(enum|fn|struct) ";
              excl_line = "//!|#\\[|use|unreachable!|^\\}$|${excl_enum_struct}";
              excl_start = "${excl_enum_struct}";
              excl_stop = "^\\}$";
              excl_br_line = "#\\[|assert(_eq)?!|(error|warn|info|debug|trace)!|^[[:space:]]*\\}(,)?$|${excl_enum_fn_struct}";
              excl_br_start = "#\\[no_coverage\\]|^mod tests \\{|${excl_enum_struct}";
              excl_br_stop = "^\\}$";
              env = "CARGO_INCREMENTAL=0"
                  + " RUSTFLAGS=\"-Zprofile -Ccodegen-units=1 -Copt-level=0 -Clink-dead-code -Coverflow-checks=off -Zpanic_abort_tests -Cpanic=abort\""
                  + " RUSTDOCFLAGS=\"-Cpanic=abort\"";
            in ''
              ${env} cargo test
              grcov . -s . -b ./target/debug/ --branch --llvm --ignore '*cargo*' --ignore-not-existing \
                  --excl-line "${excl_line}" --excl-start "${excl_start}" --excl-stop "${excl_stop}" \
                  --excl-br-line "${excl_br_line}" --excl-br-start "${excl_br_start}" --excl-br-stop "${excl_br_stop}" \
                  -o ./target/coverage.lcov --log /dev/null || true
              find target \( -name "*.gcda" -or -name "*.gcno" \) -delete
              genhtml --branch --no-function-coverage --precision 2 target/coverage.lcov -o coverage
            '';
            help = "Launch tests and generate HTML coverage website";
          }];

          packages = with pkgs; [ cargo-deny grcov lcov rust-dev ];
        };
      });
}
