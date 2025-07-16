final: prev:
let
  # Get rust-overlay from the flake.lock file
  rustNodes = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.rust-overlay.locked;
  rust-overlay = final.fetchFromGitHub {
    inherit (rustNodes) owner repo rev;
    hash = rustNodes.narHash;
  };

  overlain = final.extend (import rust-overlay);
in
{
  c2rust = overlain.callPackage ./nix { };
}
