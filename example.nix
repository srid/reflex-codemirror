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
    rev = "26b8a099159ba798b5696e73ba0d0f022a027bb9";
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
