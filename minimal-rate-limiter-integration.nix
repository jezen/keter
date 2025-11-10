{ pkgs ? import <nixpkgs> {} }:

let
  # --------------------------------------------------------------
  #  Source filter for the whole repository (unchanged)
  # --------------------------------------------------------------
  haskellSourceFilter = pkgs.lib.sourceFilesBySuffices ./. [
    ".cabal" ".hs" ".c" "LICENSE"
  ];

  port = 81;

  # --------------------------------------------------------------
  #  Haskell package set with the required overrides (unchanged)
  # --------------------------------------------------------------
  hp = pkgs.haskellPackages.override {
    overrides = self: super: let
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
          ver = "0.2.0.2";
          sha256 = "sha256-ngoymeitp7dsjvCI4uIrFhY8+BdaCsYdMVxd5solb5M=";
        } {}).overrideAttrs (old: {
          librarySystemDepends = (old.librarySystemDepends or []) ++ [ pkgs.zlib ];
          buildInputs        = (old.buildInputs        or []) ++ [ pkgs.zlib ];
        });
    in {
      http-reverse-proxy           = httpReverseProxyPkg;
      keter-rate-limiting-plugin  = rateLimiterPkg;
      keter = self.callCabal2nix "keter" haskellSourceFilter { };
    };
  };

  # --------------------------------------------------------------
  #  GHC with the minimal set of libraries needed for the backend
  # --------------------------------------------------------------
  ghc = hp.ghcWithPackages (p: [
    p.wai
    p.warp
    p.http-types
    p.text
    p.bytestring
  ]);

  # --------------------------------------------------------------
  #  Build the .keter bundle that contains the **clean** Backend.hs
  # --------------------------------------------------------------
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

    # -----------------------------------------------------------------
    #  Quick sanity‑check that the compiler works (same as original)
    # -----------------------------------------------------------------
    echo 'main = putStrLn "Compilation test OK"' > /tmp/test.hs
    ${ghc}/bin/runghc /tmp/test.hs >&2

    # -----------------------------------------------------------------
    #  Copy the **stand‑alone** Backend.hs (the file above)
    # -----------------------------------------------------------------
    cp ${./test/Backend.hs} "$work/bin/backend.hs"

    # -----------------------------------------------------------------
    #  Compile the backend, strip it, and clean up intermediate files
    # -----------------------------------------------------------------
    ${ghc}/bin/ghc -O2 -threaded -rtsopts -with-rtsopts=-N \
      -o "$work/bin/backend" "$work/bin/backend.hs"
    strip -s "$work/bin/backend" || true
    rm -f "$work/bin/backend.o" "$work/bin/backend.hi"

    # -----------------------------------------------------------------
    #  Keter stanza – unchanged, just points at the compiled binary
    # -----------------------------------------------------------------
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

    # -----------------------------------------------------------------
    #  Pack everything into a .keter bundle
    # -----------------------------------------------------------------
    mkdir -p "$TMPDIR/bundle-root"
    cp -r "$work/config" "$TMPDIR/bundle-root/"
    cp -r "$work/bin"    "$TMPDIR/bundle-root/"

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
      siege curl jq coreutils gawk gnused procps lsof gzip gnutar ghc
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
      # a dummy bundle is required so that Keter starts; the real bundle
      # will be dropped into /var/lib/keter/incoming later.
      bundle = {
        appName = "dummy";
        domain   = "dummy.local";
        executable = pkgs.writeScript "dummy" "exit 0";
      };
    };
    # Prevent the real “load‑keter‑bundle.service” from trying to run a
    # non‑existent command – we will copy the bundle manually.
    systemd.services.load-keter-bundle.serviceConfig.ExecStart =
      pkgs.lib.mkForce "${pkgs.coreutils}/bin/true";
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("keter.service")

    # --------------------------------------------------------------
    #  Deploy the bundle we built above
    # --------------------------------------------------------------
    machine.succeed("mkdir -p /var/lib/keter/incoming && chmod 755 /var/lib/keter/incoming")
    machine.succeed("cp ${bundleFile}.keter /var/lib/keter/incoming/rl-bundle.keter || cp ${bundleFile} /var/lib/keter/incoming/rl-bundle.keter")

    # --------------------------------------------------------------
    #  Wait until the bundle is loaded and the web server is listening
    # --------------------------------------------------------------
    machine.wait_for_open_port(${toString port})
    machine.succeed("sleep 6")   # give Keter a little extra time to finish init

    # --------------------------------------------------------------
    #  Basic sanity checks (processes, logs, connectivity)
    # --------------------------------------------------------------
    _, logs = machine.execute("journalctl -u keter.service --no-pager -n 120")
    print("Keter logs (last 120 lines):", logs)

    _, proc = machine.execute("ps aux | grep -E '(backend)' | grep -v grep || echo 'no backend processes'")
    print("Backend processes:", proc)

    out = machine.succeed("curl -sS -H 'Host: rl.test' -w '%{http_code}' http://127.0.0.1:${toString port}/")
    print("Basic test response:", out)
    assert out.endswith("200"), "Expected 200 from rl.test, got: " + out

    # --------------------------------------------------------------
    #  Helper to run siege (used later)
    # --------------------------------------------------------------
    def run_siege(host, seconds, concurrency, xff=None, path="/"):
      hdr = "-H 'Host: {0}'".format(host)
      xhdr = "" if xff is None else "-H 'X-Forwarded-For: {0}'".format(xff)
      cmd = "bash -lc \"siege -q -t {0}s -c {1} {2} {3} http://127.0.0.1:${toString port}{4} 2>&1\"".format(seconds, concurrency, hdr, xhdr, path)
      status, out = machine.execute(cmd)
      print(out)
      return out

    # --------------------------------------------------------------
    #  Manual request sequence – verify 4×200 then at least one 429
    # --------------------------------------------------------------
    machine.succeed("sleep 6")   # reset the rate‑limit window
    responses = []
    for i in range(6):
        status, out = machine.execute("curl -sS -H 'Host: rl.test' -w '%{http_code}' http://127.0.0.1:${toString port}/ || echo 'failed'")
        if 'failed' not in out:
            responses.append(out.strip()[-3:])
        machine.succeed("sleep 0.3")
    success = responses.count("200")
    failure = responses.count("429")
    print("Manual requests – success:", success, "rate‑limited:", failure)
    assert success == 4, "Expected exactly 4 successful requests, got %d" % success
    assert failure >= 1, "Expected at least 1 rate‑limited request, got %d" % failure

    # --------------------------------------------------------------
    #  X‑Forwarded‑For handling – each distinct IP gets its own bucket
    # --------------------------------------------------------------
    machine.succeed("sleep 6")
    for ip in ["192.168.1.100", "192.168.1.101"]:
        resp = []
        for i in range(5):
            status, out = machine.execute(
                "curl -sS -H 'X-Forwarded-For: {0}' -H 'Host: rl.test' -w '%{{http_code}}' http://127.0.0.1:${toString port}/ || echo failed".format(ip))
            if 'failed' not in out:
                resp.append(out.strip()[-3:])
            machine.succeed("sleep 0.3")
        success = resp.count("200")
        print("IP", ip, "successful requests:", success)
        assert success == 4, "Expected 4 successes for IP %s, got %d" % (ip, success)

    # --------------------------------------------------------------
    #  Burst test with siege – should trigger 429 responses
    # --------------------------------------------------------------
    machine.succeed("sleep 6")
    run_siege("rl.test", 5, 15)

    status, series = machine.execute(
        "bash -lc 'for i in $(seq 1 20); do curl -s -o /dev/null -w \"%{http_code}\\n\" -H \"Host: rl.test\" http://127.0.0.1:${toString port}/; sleep 0.1; done'")
    print("Burst series:", series)
    assert "429" in series, "Expected at least one 429 in burst test, got: " + series

    # --------------------------------------------------------------
    #  Test the second host defined in the stanza (xff.test)
    # --------------------------------------------------------------
    machine.succeed("sleep 6")
    run_siege("xff.test", 3, 2)

    # --------------------------------------------------------------
    #  Concurrent requests with different X‑Forwarded‑For values
    # --------------------------------------------------------------
    machine.succeed("sleep 6")
    for j in range(1, 3):
        for i in range(1, 5):
            ip = "192.168.2.{0}".format(j)
            out = machine.succeed(
                "curl -H 'X-Forwarded-For: {0}' -H 'Host: rl.test' --write-out '%{{http_code}}' http://127.0.0.1:${toString port}/".format(ip))
            assert out[-3:] == "200"

    # --------------------------------------------------------------
    #  Verify that the rate‑limit window resets after a pause
    # --------------------------------------------------------------
    machine.succeed("sleep 6")
    out = machine.succeed("curl -H 'Host: rl.test' --write-out '%{http_code}' http://127.0.0.1:${toString port}/")
    assert out[-3:] == "200", "Rate limit should have reset, got: " + out[-3:]

    print("All stanza configuration and rate‑limiting tests passed!")
  '';
}
