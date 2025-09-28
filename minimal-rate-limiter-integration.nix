{ pkgs ? import <nixpkgs> {} }:

let
  haskellSourceFilter = pkgs.lib.sourceFilesBySuffices ./. [
    ".cabal" ".hs" ".c" "LICENSE"
  ];
  port = 81;
  hp = pkgs.haskellPackages.override {
    overrides = self: super:
      let
        httpReverseProxyMin = "0.6.2.0";
        httpReverseProxyPkg =
          if pkgs.lib.versionAtLeast super.http-reverse-proxy.version httpReverseProxyMin
          then super.http-reverse-proxy
          else super.callHackageDirect {
            pkg = "http-reverse-proxy";
            ver = httpReverseProxyMin;
            sha256 = "sha256-cknEOvB2t2Qcyv5yFKCEWFvC4gjkCU0k7AFAA4VQ3yA=";
          } {};
        rateLimiterPkg =
          (super.callHackageDirect {
            pkg = "keter-rate-limiting-plugin";
            ver = "0.2.0.1";
            sha256 = "sha256-wQs9AZf2UlrTCvuhV/MC81Y9F2C7IZxq2cwTT+lN0EI=";
          } {}).overrideAttrs (old: {
            librarySystemDepends = (old.librarySystemDepends or []) ++ [ pkgs.zlib ];
            buildInputs = (old.buildInputs or []) ++ [ pkgs.zlib ];
          });
      in {
        http-reverse-proxy = httpReverseProxyPkg;
        keter-rate-limiting-plugin = rateLimiterPkg;
        keter = self.callCabal2nix "keter" haskellSourceFilter { };
      };
  };

  ghc = hp.ghcWithPackages (p: [
    p.wai
    p.warp
    p.http-types
    p.text
    p.bytestring
  ]);

  bundleFile = pkgs.runCommand "rl-bundle.keter" {
    buildInputs = [
      pkgs.gnutar
      pkgs.gzip
      pkgs.coreutils
      pkgs.bash
    ];
    nativeBuildInputs = [ ghc pkgs.util-linux ];
  } ''
    set -euo pipefail
    work="$TMPDIR/rl-bundle"
    mkdir -p "$work/config" "$work/bin"

    echo 'main = putStrLn "Compilation test OK"' > /tmp/test.hs
    ${ghc}/bin/runghc /tmp/test.hs >&2

    cat > "$work/bin/backend.hs" <<'HS'
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "=== Backend starting ==="
  mp <- lookupEnv "PORT"
  let p = maybe 8081 id (mp >>= readMaybe)
  hPutStrLn stderr $ "Backend will listen on port " ++ show p
  hFlush stderr
  run p app

app :: Application
app _ respond =
  respond $ responseLBS status200 [("Content-Type","text/plain")] "ok from rate-limited backend"
HS

    ${ghc}/bin/ghc -O2 -threaded -rtsopts -with-rtsopts=-N -o "$work/bin/backend" "$work/bin/backend.hs"
    strip -s "$work/bin/backend" || true
    rm -f "$work/bin/backend.o" "$work/bin/backend.hi"

    cat > "$work/config/keter.yaml" <<'YAML'
stanzas:
  - type: webapp
    exec: ../bin/backend
    hosts: ["rl.test", "xff.test"]
    ensure-alive-time-bound: 3000000
    middleware:
      - rate-limiter:
          zone_by: "default"
          throttles:
            - name: "ip"
              limit: 4
              period: 5
              algorithm: "FixedWindow"
              identifier_by: "ip"
YAML

    mkdir -p "$TMPDIR/bundle-root"
    cp -r "$work/config" "$TMPDIR/bundle-root/"
    cp -r "$work/bin" "$TMPDIR/bundle-root/"

    (cd "$TMPDIR/bundle-root" && tar -czf ../bundle.tar.gz .)
    mv "$TMPDIR/bundle.tar.gz" "$out"
    ln -s "$out" "$out.keter"
  '';
in
pkgs.testers.runNixOSTest {
  name = "keter-rate-limiter-stanza-config";

  nodes.machine = { config, pkgs, ... }: {
    networking.extraHosts = ''
      127.0.0.1 localhost rl.test xff.test
    '';
    environment.systemPackages = with pkgs; [
      siege
      curl
      jq
      coreutils
      gawk
      gnused
      procps
      lsof
      gzip
      gnutar
      ghc
    ];
    services.keter = {
      enable = true;
      package = hp.keter;
      globalKeterConfig = {
        cli-port = 123;
        listeners = [ { host = "*4"; inherit port; } ];
        ip-from-header = true;
        healthcheck-path = "/keter-health";
      };
      bundle = {
        appName = "dummy";
        domain = "dummy.local";
        executable = pkgs.writeScript "dummy" "exit 0";
      };
    };
    systemd.services.load-keter-bundle.serviceConfig.ExecStart = pkgs.lib.mkForce "${pkgs.coreutils}/bin/true";
  };
  testScript = ''
    machine.start()
    machine.wait_for_unit("keter.service")

    print("=== Create incoming directory and deploy our bundle ===")
    machine.succeed("mkdir -p /var/lib/keter/incoming && chmod 755 /var/lib/keter/incoming")
    machine.succeed("cp ${bundleFile}.keter /var/lib/keter/incoming/rl-bundle.keter || cp ${bundleFile} /var/lib/keter/incoming/rl-bundle.keter")

    print("=== Wait for bundle processing ===")
    machine.wait_for_open_port(${toString port})
    machine.succeed("sleep 6")

    print("=== Check keter logs for stanza processing ===")
    _, logs = machine.execute("journalctl -u keter.service --no-pager -n 120")
    print("Keter logs:", logs)

    print("=== Check what processes are running ===")
    _, processes = machine.execute("ps aux | grep -E '(backend)' | grep -v grep || echo 'no backend processes'")
    print("Backend processes:", processes)

    print("=== Test basic connectivity ===")
    out = machine.succeed("curl -sS -H 'Host: rl.test' -w '%{http_code}' http://127.0.0.1:${toString port}/")
    print("Basic test response: {0}".format(out))
    assert out.endswith("200"), "Expected 200 from rl.test, got: {0}".format(out)

    def run_siege(host, seconds, concurrency, xff=None, path="/"):
      hdr = "-H 'Host: {0}'".format(host)
      xhdr = "" if xff is None else "-H 'X-Forwarded-For: {0}'".format(xff)
      cmd = "bash -lc \"siege -q -t {0}s -c {1} {2} {3} http://127.0.0.1:${toString port}{4} 2>&1\"".format(seconds, concurrency, hdr, xhdr, path)
      status, output = machine.execute(cmd)
      print(output)
      return output

    print("=== Reset window before manual requests ===")
    machine.succeed("sleep 6")

    print("=== Test rate limiting with manual requests ===")
    responses = []
    for i in range(6):
        status, output = machine.execute("curl -sS -H 'Host: rl.test' -w '%{http_code}' http://127.0.0.1:${toString port}/ || echo 'request_failed'")
        if 'request_failed' not in output:
            responses.append(output.strip()[-3:])
        machine.succeed("sleep 0.3")
    success_count = responses.count("200")
    failure_count = responses.count("429")
    print("Manual requests - Success: {0}, Rate-limited: {1}".format(success_count, failure_count))
    assert success_count == 4, "Expected exactly 4 successful requests, got {0}".format(success_count)
    assert failure_count >= 1, "Expected at least 1 rate-limited request, got {0}".format(failure_count)

    print("=== Test X-Forwarded-For handling ===")
    machine.succeed("sleep 6")
    for ip in ["192.168.1.100", "192.168.1.101"]:
        responses = []
        for i in range(5):
            status, output = machine.execute("curl -sS -H 'X-Forwarded-For: {0}' -H 'Host: rl.test' -w '%{{http_code}}' http://127.0.0.1:${toString port}/ || echo 'request_failed'".format(ip))
            if 'request_failed' not in output:
                responses.append(output.strip()[-3:])
            machine.succeed("sleep 0.3")
        success_count = responses.count("200")
        print("IP {0} - Success: {1}".format(ip, success_count))
        assert success_count == 4, "Expected 4 successes for IP {0}, got {1}".format(ip, success_count)

    print("=== Test burst scenario with siege ===")
    machine.succeed("sleep 6")
    run_siege("rl.test", 5, 15)

    print("=== Verify rate limiting is active ===")
    status, series = machine.execute("bash -lc 'for i in $(seq 1 20); do curl -s -o /dev/null -w \"%{http_code}\\n\" -H \"Host: rl.test\" http://127.0.0.1:${toString port}/; sleep 0.1; done'")
    print("Burst test responses: {0}".format(series))
    assert '429' in series, "Expected 429 responses in burst test, got: {0}".format(series)

    print("=== Test xff.test host ===")
    machine.succeed("sleep 6")
    run_siege("xff.test", 3, 2)

    print("=== Testing concurrent requests with different IPs ===")
    machine.succeed("sleep 6")
    for j in range(1, 3):
      for i in range(1, 5):
          ip = "192.168.2.{0}".format(j)
          output = machine.succeed("curl -H 'X-Forwarded-For: {0}' -H 'Host: rl.test' --write-out '%{{http_code}}' http://127.0.0.1:${toString port}/".format(ip))
          status_code = output[-3:].strip()
          assert status_code == "200"
          machine.succeed("sleep 1")

    print("=== Testing rate limit reset ===")
    machine.succeed("sleep 6")
    output = machine.succeed("curl -H 'Host: rl.test' --write-out '%{http_code}' http://127.0.0.1:${toString port}/")
    status_code = output[-3:].strip()
    assert status_code == "200", "Rate limit should have reset, got: {0}".format(status_code)

    print("All stanza configuration and rate limiting tests passed!")
  '';
}
