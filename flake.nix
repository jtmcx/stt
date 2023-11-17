{
  description = "A playground for set-theoretic types";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.haskell-flake.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      perSystem = { config, self', inputs', pkgs, system, ... }: {
        haskellProjects.default = {
          devShell = {
            # By default, the haskell-flake module populates `devShells.default`
            # for us. While it's nice that haskell-flake has magically defined a
            # devShell, it's difficult to extend. To work around this, we
            # "disable" the devShell here (i.e., don't let haskell-flake set
            # `devShells.default`), and then define our own explicit `devShell`
            # below, which derives its inputs from this one.
            enable = false;

            # Disable flake check that verifies HLS is working.
            hlsCheck.enable = false;
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = [
            pkgs.just

            # There is an unusual locale issue when running GHC under Nix. If
            # 'pkgs.glibcLocales' is not included as a 'nativeBuildInput', 'stt'
            # will abort when it tries to print a unicode character with:
            #
            #    <stdout>: commitBuffer: invalid argument (invalid character)
            #    <stdout>: hPutChar: invalid argument (invalid character)
            #
            # I still need to understand the root cause of this, and understand
            # whether or not including glibcLocales is the "correct" solution.
            pkgs.glibcLocales
          ];
        };
      };
    };
}
