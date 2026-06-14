{
  description = "GLML Nix Flake";

  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };

  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
    }@inputs:
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
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        opamPackagesQuery = devOpamPackagesQuery // {
          ocaml-base-compiler = "*";
        };

        # OCaml Project Scope
        scope = on.buildDuneProject {
          repos = [ opam-nix.inputs.opam-repository ];
        } package ./. opamPackagesQuery;

        # Prevent the ocaml dependencies from leaking into dependent environments
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (_: {
            doNixSupport = false;
          });
        };
        scope' = scope.overrideScope overlay;

        # Expose OCaml packages defined in [devOpamPackagesQuery] to devshell
        devOpamPackages = builtins.attrValues (
          pkgs.lib.getAttrs (builtins.attrNames devOpamPackagesQuery) scope'
        );
        main = scope'.${package}.overrideAttrs (old: {
          meta = (old.meta or { }) // {
            description = "GLML (OpenGL Meta Language): A functional DSL that compiles to GLSL fragment shaders";
            homepage = "https://github.com/glml-lang/GLML";
            license = pkgs.lib.licenses.mit;
            mainProgram = "glml";
          };
        });
      in
      {
        legacyPackages = scope';
        packages.default = main;
        packages.glml = main;

        apps.glml = {
          type = "app";
          program = "${main}/bin/glml";
        };
        apps.default = self.apps.${system}.glml;

        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [ main ];
            packages =
              devOpamPackages
              ++ (with pkgs; [
                glsl_analyzer
                glslang
                nodejs
              ]);
          };
        };
      }
    );
}
