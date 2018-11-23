{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    dimensional = pkgs.fetchFromGitHub {
      owner = "bjornbm";
      repo = "dimensional";
      rev = "8e1aa6ebd23cdd4b515f1ea44a9820f96ec71083";
      sha256 = "1g6l128fc5grnivqjll74ppr24jw66yhvi0hbiyp66zpgs9a65bx";
    };

    geodetic-types = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "geodetic-types";
      rev = "28475c8a3d2ae2aeefa9f37c5fd23344d5c2bd8c";
      sha256 = "1myq5x5i4nvhxy2728f1y3w8myaha36iid6hqgn2a3qy3xpwagpx";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      hedgehog       = self.callHackage "hedgehog" "0.6" {};
      tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
      concurrent-output = pkgs.haskell.lib.doJailbreak super.concurrent-output;
      dimensional = super.callCabal2nix "dimensional" "${sources.dimensional}" {};
      geodetic-types = import sources.geodetic-types { inherit nixpkgs compiler; };
    };
  };

  vfr-waypoints = modifiedHaskellPackages.callPackage ./vfr-waypoints.nix {};
in
  vfr-waypoints
