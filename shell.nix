{ pkgs ? import ./nixpkgs-pinned.nix {}, ... }:
let
  check = pkgs.writeShellScriptBin "check" ''
    set -e
    echo TODO
    echo "Success."
  '';
  build = pkgs.writeShellScriptBin "build" ''
    set -e
    elm make src/Main.elm --output public/index.html
  '';
  notify-github = pkgs.writeScriptBin "notify-github" ''
    CONTEXT="$2" && [ -z $2 ] && CONTEXT="gitlab-ci" || true
    DESCRIPTION="$3" && [ -z $3 ] && DESCRIPTION="test" || true
    URL="$4" && [ -z $4 ] && URL="$CI_PROJECT_URL/pipelines/$CI_PIPELINE_ID" || true
    ${pkgs.curlFull}/bin/curl --user "$GITHUB_USER:$GITHUB_TOKEN" "https://api.github.com/repos/botiboti/morris/statuses/$CI_COMMIT_SHA" -X POST --data @- <<EOF
      { "state": "$1"
      , "target_url": "$URL"
      , "description": "$DESCRIPTION"
      , "context": "$CONTEXT"
      }
    EOF
    [[ $? == 0 ]] || echo "GitHub commit status API call failed. Please set GITHUB_USER and GITHUB_TOKEN secret variables in GitLab repo CI settings"
  '';
in
  pkgs.stdenv.mkDerivation rec {
    name = "morris-shell";
    src = ./.;
    buildInputs = [
      pkgs.elmPackages.elm
      pkgs.elmPackages.elm-format
      check
      build
      notify-github
    ];
  }
