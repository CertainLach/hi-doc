{
  description = "Diagnostics library";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
    };
    shelly = {
      url = "github:CertainLach/shelly";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;}
    {
      imports = [inputs.shelly.flakeModule];
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      perSystem = {
        self',
        system,
        pkgs,
        lib,
        ...
      }: let
        rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        craneLib = (inputs.crane.mkLib pkgs).overrideToolchain rust;
      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [inputs.rust-overlay.overlays.default];
        };
        shelly.shells.default = {
          factory = craneLib.devShell;
          packages = with pkgs; [
            rust
            cargo-edit
            asciinema

              cmake
          ];
          environment.NIX_FMT = lib.getExe self'.formatter;
        };
        formatter = pkgs.alejandra;
      };
    };
}
