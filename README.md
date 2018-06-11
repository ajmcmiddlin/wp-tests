# wp-test

Test WordPress using hedgehog and nixops.

## Running WordPress

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

You should see the IP address of the virtual machine in the deploy output. On my machine it's in the 192.168.56.0/24 subnet. If you don't see it, you can do the following:

```
[me@host]$ nixops ssh -d wp wordpress
[root@wordpress]# ip a
```

Now you can hit that IP in a browser and finish the WordPress installation. Once you've finished the installation, go to the plugins section in the admin panel and activate plugins. We need the basic auth plugin to simplify auth while testing.

## Testing

When testing, we drop the WordPress database and reinstate it using a database dump called `wordpress.sql`. To create a `wordpress.sql` after wordpress is running and plugins have been activated, run the following:

```
nixops ssh -d wp wordpress "mysqldump --databases wordpress" > wordpress.sql
```
