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
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        haskellProjects.default = {
          devShell = {
            # By default, the haskell-flake module populates `devShells.default`
            # for us. While it's nice that haskell-flake has magically defined a
            # devShell, it's difficult to customize and extend. To work around
            # this, we "disable" the devShell here (i.e., don't let
            # haskell-flake set `devShells.default`), and then define our own
            # explicit `devShell` below, which derives its inputs from this one.
            enable = false;

            # Disable flake check to verify that HLS works.
            hlsCheck.enable = false;
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = [
            pkgs.just
          ];
        };
      };

      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
      };
    };
}
