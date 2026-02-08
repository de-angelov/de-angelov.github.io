{
  description = "A development shell for de-angelov.github.io";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; # Using nixos-unstable for latest packages
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux"; # Assuming x86_64-linux for devcontainer
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true; # Allow unfree packages if needed
        };
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = hfinal: hprev: {
          # Specify GHC version if necessary, otherwise use default from haskellPackages
          # ghc = hprev.ghc.withPackages (hp: [
          #   hp.cabal-install
          #   hp.haskell-language-server
          # ]);
          haskell-language-server = hprev.haskell-language-server;
        };
      };

    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          # Haskell tools
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.fourmolu # from original devcontainer.json
          haskellPackages.pandoc   # from original devcontainer.json

          # Other tools
          go-task # go-task
          entr
          postgresql # Provides libpq-dev
          nodejs     # for any JS needs
          wget       # for the previous script, good to have it in the devShell
          gnupg      # Often needed for various setups, good general utility
          libpq      # Explicitly add libpq for development headers
        ];

        # Set up a PATH for cabal-install and other tools if necessary
        shellHook = ''
          export PATH=$HOME/.cabal/bin:$PATH
          echo "Welcome to the Nix-powered Haskell development environment!"
          echo "GHC version: $(ghc --version)"
          echo "Cabal version: $(cabal --version)"
          echo "Haskell Language Server version: $(haskell-language-server --version)"
          echo "Go-Task version: $(task --version)"
        '';
      };
    };
}
