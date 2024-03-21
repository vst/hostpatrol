{
  description = "Nix Development Shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = import inputs.nixpkgs { inherit system; };
      in
      {
        devShell = nixpkgs.mkShell {
          buildInputs = [
            nixpkgs.nodePackages.typescript-language-server
            nixpkgs.nodejs_20
            nixpkgs.typescript
          ];
        };
      }
    );
}
