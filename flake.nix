{
  description = "nft-marketplace-server";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ inputs.haskell-nix.overlay ];
        inherit (inputs.haskell-nix) config;
      };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs.git
              pkgs.fd
              pkgs.haskellPackages.cabal-fmt
              pkgs.nixpkgs-fmt
              pkgs.haskellPackages.fourmolu
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL=pure
          cd ${self}
          make format_check
          mkdir $out
        ''
      ;

      lintCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs.git
              pkgs.fd
              pkgs.hlint
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL=pure
          cd ${self}
          make lint_check
          mkdir $out
        ''
      ;

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.haskell-nix.cabalProject' {
          name = "nft-marketplace-server";
          compiler-nix-name = "ghc925";
          src = ./.;
          modules = [{
            reinstallableLibGhc = true;
          }];

          shell = {
            withHoogle = true;
            exactDeps = true;
            # tools.haskell-language-server = { };
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.nixpkgs-fmt
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskellPackages.apply-refact
              pkgs.haskellPackages.fourmolu
              pkgs.haskellPackages.hlint
            ];
          };
        };
    in
    {
      inherit nixpkgsFor;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages // {
        upload-image = (nixpkgsFor system).callPackage ./nix/upload-image.nix {};
      });
      checks = perSystem (system:
        self.flake.${system}.checks
        // self.flake.${system}.packages
        // {
          formatCheck = formatCheckFor system;
          lintCheck = lintCheckFor system;
        }
      );
      hydraJobs.x86_64-linux = self.checks.x86_64-linux;
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system};
          } "touch $out"
      );
      apps = perSystem (system: self.flake.${system}.apps // {
        upload-image = {
          type = "app";
          program = "${self.packages.${system}.upload-image}/bin/upload-image";
        };
      });
      devShell = perSystem (system: self.flake.${system}.devShell);
      overlays = {
        nft-marketplace-server = pkgsSelf: _: {
          nft-marketplace-server = self.packages.${pkgsSelf.system}."nft-marketplace-server:exe:nft-marketplace-server";
        };
        default = self.overlays.nft-marketplace-server;
      };
      nixosModules = {
        nft-marketplace-server = {
          imports = [ ./nix/nft-marketplace-server.nix ];
          nixpkgs.overlays = [ self.overlays.nft-marketplace-server ];
        };
        default = self.nixosModules.nft-marketplace-server;
      };
    };
}
