{
  description = "Logo language interpreter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, easy-purescript-nix, flake-utils, nixpkgs, ... }:
    let
      name = "mlogo";

      supportedSystems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        easy-ps = import easy-purescript-nix { inherit pkgs; };
        format-check = pkgs.stdenvNoCC.mkDerivation {
          checkPhase = ''
            purs-tidy check {src,test}
          '';
          doCheck = true;
          dontBuild = true;
          installPhase = ''
            mkdir "$out"
          '';
          name = "format-check";
          nativeBuildInputs = with easy-ps; [ purs-tidy ];
          src = ./.;
        };
      in
      {
        checks = { inherit format-check; };
        devShell = pkgs.mkShell {
          inherit name;
          buildInputs = with pkgs; [ esbuild git imagemagick nodejs ]
            ++ (with easy-ps; [ psa purs purs-backend-es purs-tidy spago ]);

          shellHook = ''
            PS1="\[\e[33m\][\[\e[m\]\[\e[34;40m\]${name}:\[\e[m\]\[\e[36m\]\w\[\e[m\]\[\e[33m\]]\[\e[m\]\[\e[32m\]\\$\[\e[m\] "
          '';
        };
      }
    );
}
