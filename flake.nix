{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-unstable";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, flake-utils, rust-overlay, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
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
            rust-ci = rust.minimal.override { extensions = [ "clippy" "llvm-tools-preview" "rustfmt" ]; };
          in pkgs.dockerTools.buildImage {
            name = "proost-ci";

            config.Entrypoint = [ "${pkgs.dockerTools.binSh}/bin/sh" "-c" ];

            copyToRoot = pkgs.buildEnv {
              name = "proost-dependencies";
              paths = (with pkgs; [ coreutils gcc gnugrep gnused grcov openssh rust-ci ])
                ++ (with pkgs.dockerTools; [ binSh caCertificates fakeNss ]);
              pathsToLink = [ "/bin" "/etc" ];
            };

            runAsRoot = "mkdir /tmp";
          };
        };

        devShells.default = pkgs.mkShell {
          name = "proost-dev";
          packages = [ (rust.default.override { extensions = [ "rust-analyzer" ]; }) ];
        };
      });
}
