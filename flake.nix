{
  description = "A Nix-flake-based OCaml development environment";

  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            ocaml
            ocamlformat
          ] ++
            (with pkgs.ocamlPackages; [
              dune_3
              ocaml-lsp
              findlib
              ppx_deriving
            ]);

          env = {
            NIX_ENFORCE_NO_NATIVE = 0;
          };
        };
      });
    };
}