# wp-test

Test WordPress using hedgehog and nixops.

## Running

```
nixops create ./wp.nix ./wp-vbox.nix -d wp
nixops deploy -d wp
```
