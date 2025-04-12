
final: prev:

let
  haskell-packages-path = ../../../../packages;

  haskell-overrides-path = ../../../haskell-packages;

  make-package-override = name: type:
    if type == "directory"
      then throw ''
        the directory ${haskell-overrides-path} may only contain files, but the path:

          '${haskell-overrides-path}/${name}'

        is a sub-directory.
      '';
    else let
      path = "${haskell-packages-path}/${name}";
    in final.callCabal2nix name path { };
in mapAttrs make-package-override (builtins.readDir haskell-overrides-path);