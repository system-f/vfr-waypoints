{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "vfr-waypoints": {
            "enabled": 1,
            "hidden": false,
            "description": "vfr-waypoints",
            "nixexprinput": "vfr-waypoints",
            "nixexprpath": "ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "vfr-waypoints": { "type": "git", "value": "https://github.com/qfpl/vfr-waypoints", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-18.09", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
