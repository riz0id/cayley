{ gitignore }:

final: prev:

let
  hsLib = final.haskell.lib.compose;

  ghcVersions = ["982"];

  defaultGHCVersion = "982";

  localHsPackages = {
    # Libraries
    "cayley"      = ../../.;
  };

  mkLocalDerivation = hspkgs: name: path:
    let
      pkg = hspkgs.callCabal2nix name (gitignore.lib.gitignoreSource path) {};
    in
      haskell.lib.overrideCabal pkg (old: {
        doHaddock = true;
        doCheck = true;
      });

  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "ghc${defaultGHCVersion}" = prev.haskell.packages."ghc${defaultGHCVersion}".override {
        overrides = hfinal: hprev: (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
          prim-bool = hfinal.callPackage ../haskell-packages/prim-bool.nix {};

          prim-char = hfinal.callPackage ../haskell-packages/prim-char.nix {};

          prim-compat = hfinal.callPackage ../haskell-packages/prim-compat.nix {};

          prim-int = hfinal.callPackage ../haskell-packages/prim-int.nix {};

          prim-ord = hfinal.callPackage ../haskell-packages/prim-ord.nix {};

          utf8-text = hfinal.callPackage ../haskell-packages/utf8-text.nix {};

          source-locations = hfinal.callPackage ../haskell-packages/source-locations.nix {};

          # Need specific versions for benchmarking
          servant = hfinal.callPackage ../haskell-packages/servant.nix {};

          servant-auth-server = hsLib.dontCheck (hfinal.callPackage ../haskell-packages/servant-auth-server.nix {});

          # Test failures
          bsb-http-chunked = hsLib.dontCheck hprev.bsb-http-chunked;
        };
      };
    };
  };

  mkDevShell = ghcVersion:
    let hsPkgs = haskell.packages."ghc${defaultGHCVersion}";

        haskell-language-server = hsPkgs.haskell-language-server {
          supportedGhcVersions = [ ghcVersion ];
        };

        shell = hsPkgs.shellFor {
          name = "cayley-dev-ghc${ghcVersion}";

          doBenchmark = true;

          packages = pkgs: map (name: pkgs.${name}) (builtins.attrNames localHsPackages);

          buildInputs = [
            final.cabal-install
            final.cabal2nix
            hsPkgs.ghc
            final.haskell-language-server
            final.hlint
            final.stack
            final.newman
          ] ++ final.lib.optionals (ghcVersion == defaultGHCVersion) [
            haskell.packages."ghc${defaultGHCVersion}".stylish-haskell
          ];

          src = null;
        };
    in { ${shell.name} = shell; };

  # Shell used to upload packages to hackage; contains a minimal set
  # of dependencies
  hackageUploadShell =
    let hsPkgs = haskell.packages."ghc${defaultGHCVersion}";
        name = "cayley-hackage-upload-shell";
    in {
      ${name} = hsPkgs.shellFor {
        inherit name;
        doBenchmark = false;

        packages = pkgs: map (name: pkgs.${name}) (builtins.attrNames localHsPackages);

        buildInputs = [
          final.cabal-install
          final.cabal2nix
          final.curl
          final.findutils
          hsPkgs.ghc
        ];

        src = null;
      };
    };
in {
  inherit ghcVersions defaultGHCVersion;

  inherit localHsPackages haskell mkDevShell hackageUploadShell;
}