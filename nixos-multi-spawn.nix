{ mkDerivation, aeson, async, base, bytestring, concurrent-output
, dataenc, digest-pure, directory, entropy, filepath
, process, stdenv, systemd, unix, unordered-containers, hinotify
}:
mkDerivation {
  pname = "nixos-multi-spawn";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring concurrent-output dataenc digest-pure
    directory entropy filepath process systemd
    unix unordered-containers hinotify
  ];
  license = stdenv.lib.licenses.mit;
}
