{ inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  outputs = {
    self,
    nixpkgs,
    utils,
    rust-overlay,
  }:
    utils.lib.eachDefaultSystem (
      system: let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            rust-bin.stable."latest".default

            gcc
            pkg-config
            openssl
            openssl.dev 
          ];

          PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
        };
      }
    );
}

