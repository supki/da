darauf
======

Darauf is a little utility for displaying the status of Nix channels.

```
% darauf
‘nixpkgs’ has been at 8e56452 for 9 days (since 2015-06-11 04:48:00 UTC)
```

By default, it looks into the `~/.nix-channels` file for the information about
the Nix channels added to Nix, but the location is configurable with
the `DARAUF_NIX_CHANNELS` environment variable.

```
% cat ./nix-channels
https://nixos.org/channels/nixpkgs-unstable nixpkgs
https://nixos.org/channels/nixos-13.10 nixos-13.10
https://nixos.org/channels/nixos-14.04 nixos-14.04
https://nixos.org/channels/nixos-14.12 nixos-14.12
https://nixos.org/channels/nixos-unstable nixos-unstable
% DARAUF_NIX_CHANNELS=./nix-channels ./dist/build/darauf/darauf
‘nixpkgs’ has been at 8e56452 for 9 days (since 2015-06-11 04:48:00 UTC)
‘nixos-13.10’ has been at 91e952a for 47 weeks (since 2014-07-19 23:56:00 UTC)
‘nixos-14.04’ has been at 8a3eea0 for 16 weeks (since 2015-02-25 19:22:00 UTC)
‘nixos-14.12’ has been at bc4f185 for 11 days (since 2015-06-08 14:37:00 UTC)
‘nixos-unstable’ has been at e1af50c for 8 days (since 2015-06-11 22:32:00 UTC)
```
