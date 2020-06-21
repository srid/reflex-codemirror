{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
}:
with nixpkgs;
let
  reflexPlatformSrc = fetchGit {
    url = https://github.com/reflex-frp/reflex-platform;
    rev = "d25152d30a76001d8a22ae96ee55f500180dd063";
  };

  reflexUtilsSrc = fetchGit {
    url = https://github.com/atidot/reflex-utils;
    rev = "9ffda7ef04c3d538fdf2ae8d6f3eddcd456de8ab";
  };

  reflex-platform = import reflexPlatformSrc {};
in
reflex-platform.project({ pkgs, ... }: {
  packages = {
    reflex-codemirror = ../reflex-codemirror;
    reflex-utils      = reflexUtilsSrc;
  };

  shells = {
    ghcjs = [ "reflex-codemirror"
              "reflex-utils"
            ];
  };
})
