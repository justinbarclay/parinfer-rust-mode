{
  description = "A very basic flake";
  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
      flake-utils.url = "github:numtide/flake-utils";
    };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            # self.overlays.default
          ];
        };

        lib = nixpkgs.lib;

        parinfer-rust-mode = pkgs.emacsPackages.trivialBuild
          {
            pname = "parinfer-rust-mode";
            version = "0.8.4";
            src = ./.;
            nativeBuildInputs = with pkgs; [ emacsPackages.eask python3 which ];
            checkPhase = ''
              echo "Running tests"
              PARINFER_RUST_TEST=true
              make test
            '';
            doCheck = true;
            meta = {
              description = "An Emacs interface for the parinfer-rust library";
              license = lib.licenses.gpl3;
              platforms = lib.platforms.all;
            };
          };
      in
      {
        packages = {
          default = parinfer-rust-mode;
        };
        devShells.default = pkgs.mkShell {
          packages = with pkgs;
            [ eask-cli ];
        };

      });
}
