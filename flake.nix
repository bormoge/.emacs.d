{
  description = "Flake for my personal Emacs config.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  };

  outputs = { self, nixpkgs }:
  let
    supportedSystems = [ "x86_64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
  in
  {
    devShells = forAllSystems (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Nix language server and formatter
            nixd
            nixfmt
            # pdf-tools build requirements
            automake
            autoconf
            pkg-config
            libpng
            zlib
            poppler
          ];
        };
      }
    );
  };
}
