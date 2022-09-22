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
        rustPackages = pkgs.rust-bin.nightly.latest;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
      in rec {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "proost";
          version = "0.1.0";

          nativeBuildInputs = [ rustPackages.default ];

          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;

          meta = with pkgs.lib; {
            description = "A simple proof assistant written in Rust";
            homepage = "https://gitlab.crans.org/loutr/proost";
            license = licenses.gpl3;
          };
        };

        devShells.default = pkgs.mkShell {
          name = "proost-dev";
          packages = packages.default.nativeBuildInputs
            ++ (with rustPackages; [ rustfmt clippy ]);
        };
      });
}

