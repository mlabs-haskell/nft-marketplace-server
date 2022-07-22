{
  description = "nft-marketplace-server";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    haskell-nix.url = "github:input-output-hk/haskell.nix/fa2fa131fe15e630c91ab4078d12eb32c41f934b";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    fourmolu = {
      url = "github:fourmolu/fourmolu?ref=v0.6.0.0";
      flake = false;
    };
    # hlint = {
    #   url = "github:ndmitchell/hlint?ref=v3.4";
    #   flake = false;
    # };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
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
          compiler-nix-name = "ghc923";
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

      packages = perSystem (system: self.flake.${system}.packages);
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
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
