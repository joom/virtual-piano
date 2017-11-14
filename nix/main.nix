{ system ? builtins.currentSystem }:

rec {
  fetchNixpkgs = import ./fetch-nixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev    = "19879836d10f64a10658d1e2a84fc54b090e2087";
    sha256 = "0x7xa9xgdraqxhhz548nng5gcs37193zvq8v9ngbqd2lzn2dm4hd";
    inherit system;
  };
  
  pkgs = import nixpkgs {
    config = { allowUnfree = true; overrides = []; };
    inherit system;
  };

  filterPath = path: (
    with {
      sf = name: type: let bn = baseNameOf (toString name); in !(
        (type == "directory" && (bn == ".git"))
        || pkgs.lib.hasSuffix "~" bn
        || pkgs.lib.hasSuffix ".o" bn
        || pkgs.lib.hasSuffix ".so" bn
        || pkgs.lib.hasSuffix ".nix" bn
        || (type == "symlink" && pkgs.lib.hasPrefix "result" bn)
      );
    };
    builtins.filterSource sf path);

  source = filterPath ./..;
  
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      PortMidi      = self.callHackage "PortMidi" "0.1.5.2" {};
      virtual-piano = self.callCabal2nix "virtual-piano" source {};
    };
  };
}
