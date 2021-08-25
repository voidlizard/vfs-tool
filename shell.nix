let 

  pinnedPkgs = builtins.fetchGit {
    url   = "https://github.com/NixOS/nixpkgs";
    rev   = "2032339e5bbce8a57655b8742190ecb32b7af863";
  };

  pkgs = import pinnedPkgs {};

  ghc = "ghc884";

  myHaskellPackages = pkgs.haskell.packages.${ghc}.override {
    overrides = hself: hsuper: {
    };
  };

in myHaskellPackages.shellFor {
  packages = p: [
  ];
  buildInputs = with pkgs.haskellPackages; [
    myHaskellPackages.cabal-install
    hlint
    brittany
    ghcid
    fast-tags
  ];
  withHoogle = true;
}

