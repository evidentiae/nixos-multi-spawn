{ mkDerivation, aeson, async, base, bytestring, concurrent-output
, dataenc, digest-pure, directory, entropy, filepath, hashids
, process, stdenv, tailfile-hinotify, unix, unordered-containers
}:
mkDerivation {
  pname = "nixos-multi-spawn";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring concurrent-output dataenc digest-pure
    directory entropy filepath hashids process tailfile-hinotify unix
    unordered-containers
  ];
  license = stdenv.lib.licenses.mit;
}
