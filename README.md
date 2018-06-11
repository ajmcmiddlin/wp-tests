# wp-test

Test WordPress using hedgehog and nixops.

## Running

```
nixops create ./wp.nix ./wp-vbox.nix -d wp
nixops deploy -d wp
```

@ajmccluskey has had 2 issues with this:

1. I needed to have the VirtualBox GUI open, otherwise I got a "kernel driver inaccessible" error.
2. After getting an error during deployment, I had to reboot the machine and deploy again.

```
nixops reboot -d wp
nixops deploy -d wp
```
