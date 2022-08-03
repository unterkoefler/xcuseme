let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.19.0";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            haskell-to-elm
            tokenize
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
            elmPackages.elm
            yarn
            nodejs-16_x
        ];
        projectPath = ./.;
    };
in
    haskellEnv
