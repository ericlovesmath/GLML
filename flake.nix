{
  description = "GLML nix flake";

  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";

    janestreet-repo = {
      url = "github:janestreet/opam-repository";
      flake = false;
    };
  };

  outputs = { self, flake-utils, opam-nix, janestreet-repo, nixpkgs }@inputs:
    let
      # Uses <package>.opam to solve dependencies from
      package = "GLML";

      # Opam packages used in developer mode
      devOpamPackagesQuery = {
        utop = "*";
        ocaml-lsp-server = "*";
        merlin = "*";
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        # ocaml-base-compiler = "*" for any version
        opamPackagesQuery = devOpamPackagesQuery // {
          ocaml-base-compiler = "5.2.0";
        };

        # OCaml Project Scope
        scope = on.buildDuneProject {
          repos = [
            janestreet-repo
            opam-nix.inputs.opam-repository
          ];
        } package ./. opamPackagesQuery;

        # Prevent the ocaml dependencies from leaking into dependent environments
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (_: {
            doNixSupport = false;
          });
        };
        scope' = scope.overrideScope overlay;

        # Expose OCaml packages defined in [devOpamPackagesQuery] to devshell
        devOpamPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devOpamPackagesQuery) scope');
        main = scope'.${package};
      in
      {
        legacyPackages = scope';
        packages.default = main;

        devShells = {
          # Shell with opam packages
          default = pkgs.mkShell {
            inputsFrom = [ main ];
            packages = [
              devOpamPackages
              pkgs.glsl_analyzer
            ];
          };
        };
      }
    );
}
