{
# Use this repo as the `nixpkgs` URL
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkg.legacyPackages.${"YOUR_SYSTEM_STRING"};
    in
    {
      
    };
}
