{
  pkgs ? (
    import <nixpkgs> {
      overlays = [
        (import ./.)
      ];
    }
  ),
}:
pkgs.mkShell.override
  {
    stdenv = pkgs.clangStdenv;
  }
  {
    name = "c2rust-devshell";
    inputsFrom = [
      pkgs.c2rust.unwrapped
    ];

    packages = [
      (pkgs.c2rust.unwrapped.toolchain.override (old: {
        extensions = old.extensions ++ [
          "rustfmt"
          "miri"
          "rust-analyzer"
        ];
      }))

      (pkgs.python3.withPackages (p: [
        p.bencode-py
        p.cbor
        p.colorlog
        p.mako
        p.pip
        p.plumbum
        p.psutil
        p.pygments
        p.typing
        # p.scan-build TODO: Once merged to nixpkgs, add
        p.pyyaml
        p.toml
      ]))

      pkgs.openssl
    ];
  }
