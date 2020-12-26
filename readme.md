Once vscode is connected to the remote container, execute the following commands inside the vscode terminal.

```bash
sudo nix-env -i cabal-install
sudo nix-env -iA nixpkgs.binutils-unwrapped
cabal update
cabal new-install --lib sort
```