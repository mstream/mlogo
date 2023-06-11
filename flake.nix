{
  description = "Logo language interpreter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix?rev=a90bd941297497c83205f0a64f30c5188a2a4fda";
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
        nixpkgsConfig = {
          permittedInsecurePackages = [
            "nodejs-14.21.3"
            "openssl-1.1.1u"
          ];
        };

        pkgs = import nixpkgs {
          inherit system; config = nixpkgsConfig;
        };

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
