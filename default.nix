{ mkDerivation, aeson, async, base, bytestring, concurrent-output
, dataenc, digest-pure, directory, entropy, filepath, hinotify, lib
, process, systemd, unix, unordered-containers
}:
mkDerivation {
  pname = "nixos-multi-spawn";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring concurrent-output dataenc digest-pure
    directory entropy filepath hinotify process systemd unix
    unordered-containers
  ];
  license = lib.licenses.mit;
}
