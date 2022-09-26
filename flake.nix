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
            rust-ci =
              rust.minimal.override { extensions = [ "clippy" "rustfmt" ]; };
          in pkgs.dockerTools.buildImage {
            name = "proost-ci";

            copyToRoot = pkgs.buildEnv {
              name = "proost-dependencies";
              paths = with pkgs; [ bash coreutils gcc openssh rust-ci ];
            };

            runAsRoot = "mkdir /tmp";

            config = {
              Env =
                [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
              Entrypoint = [ "${pkgs.bash}/bin/sh" "-c" ];
            };
          };
        };

        devShells.default = pkgs.mkShell {
          name = "proost-dev";
          packages = rust.default.nativeBuildInputs
            ++ (with rust; [ rustfmt clippy ]);
        };
      });
}

