---
layout: post
title:  "Secure, Declarative Key Management with NixOps, Pass, and nix-plugins"
date:   2018-06-24
categories:
---

[NixOps](https://nixos.org/nixops/) is a declarative deployment tool
for [NixOS](https://nixos.org/), the purely functional Linux
distribution. NixOps is traditionally used for cloud deployments,
while personal NixOS machines are usually configured with
`nixos-rebuild`. But NixOps can be used to configure your personal
machine as well. All my NixOS machines are configured with a file
looking something like this:

```nix
{
  my-machine = { ... }: {
    deployment.targetHost = "localhost";
    imports = [./hardware-configuration.nix ./configuration.nix];
  };
}
```

By deploying my machine with NixOps, I can provision other resources
simultaneously, like a private EC2 instance for VPN, and an S3 bucket
to store the build cache for the Hydra server running on my desktop.

# NixOS and the Nix store

With NixOS, almost anything you put in your system configuration is
likely to be placed in the Nix store. `etc` files, for instance.

```nix
# configuration.nix
{ ... }: {
  environment.etc.foo.text = builtins.readFile ./foo;
}
```

```bash
$ ls -l /etc/foo
lrwxrwxrwx 1 root root 20 Jun 24 03:32 /etc/foo -> /etc/static/foo

$ ls -l /etc/static
lrwxrwxrwx 1 root root 51 Jun 24 03:32 /etc/static -> /nix/store/jx8wna7pamzwsan81g1lkvk6i48m05qc-etc/etc
```

As you can see, the `foo` file ultimately links back to a file in
`/nix/store`. This is because a NixOS configuration is just an
ordinary Nix derivation, so everything involved must be in the Nix
store. This is good for reasons like reproducibility and rollbacks,
but bad for secrets. Since the Nix store is globally readable to all
users, any secrets that end up there are as well. To avoid this,
NixOps has [a key management
feature](https://nixos.org/nixops/manual/#idm140737318276736) for
deploying keys without putting them in the Nix store.

```nix
{ ... }: {
  deployment.keys.secret-foo = {
    text = builtins.readFile ./secret-foo;
    user = "will";
    group = "wheel";
	permissions = "0640";
  };
}
```

Including this module in one of your NixOps deployments instructs
NixOps to perform an extra step where it copies the contents of
`secret-foo` from the Nix expression to `/run/keys/secret-foo` on the
target machine without ever touching the Nix store. `/run/keys` is a
temporary file system, so by default the keys won't even hit the
disk. NixOps commands will make sure to redeploy keys when you run
`nixops reboot`, but you can't use this command for to reboot a
machine that deploys to itself, since the `nixops` process attending
the reboot will be shut down. The `destDir` option can be used to
store the keys on disk instead, which is fine for me since my disk is
encrypted.

```nix
{ ... }: {
  deployment.keys.secret-foo = {
    text = builtins.readFile ./secret-foo;
    destDir = "/secrets";
  };
}
```

**NOTE:** Keys deployed with NixOps are **not** deleted from the
target machine when you remove the declaration from the Nix
configuration. This is a shortcoming in NixOps, so remember to delete
keys not stored in `/run/keys` manually when removing them from the
configuration.

# Sharing secrets

I keep all my system configurations in a git repo that I sync with my
server. For more reasons than one, secrets should not be checked into
this repo in plaintext. But I do want some secrets deployed to several
machines in plaintext. [Pass, the "standard" unix password
manager](https://www.passwordstore.org/) is a convenient, useful tool
for keeping secrets encrypted and synced via Git. This is the tool I
use to keep secrets synced across machines.

It's not possible by default to access `pass` keys in Nix. But there
are a couple of ways to make it possible.

1. The `allow-unsafe-native-code-during-evaluation` option in Nix
   gives access to a `builtins.exec` function, which is sort of like
   `unsafePerformIO` in Haskell. It can be used to execute arbitrary
   commands and parse their stdout as a Nix expression.

2. [Nix plugins](https://nixos.org/nix/manual/#conf-plugin-files) are
   just shared libraries that are loaded at runtime via command line
   option to extend the Nix interpreter. This is more precise, so we
   can use it to avoid giving all Nix code access to `exec`.

Though option 1 is much more convenient, option 2 is more technically
preferable. To avoid the inconvenience of having to write C++ and
understand the Nix API, Shea Levy has created
[nix-plugins](https://github.com/shlevy/nix-plugins), a single Nix
plugin that lets you add your own plugins written in Nix by providing
priveledged access to `exec`. This restores much of the convenience of
option 1.

```nix
# configuration.nix
{ pkgs, config, ... }: {
  nix.extraOptions = ''
    plugin-files = ${pkgs.nix-plugins_4.override { nix = config.nix.package; }}/lib/nix/plugins/libnix-extra-builtins.so
  '';
}
```

This configuration will give your Nix interpreter an
`extra-builtins-file` option, which you can use to write plugins in
Nix. The `nix` package is overridden to ensure that `nix-plugins` is
built with the version of Nix that your system has. You can also
always use `--option plugin-files /nix/store/.../libnix-extra-builtins.so` to
avoid having to bootstrap `nix-plugins` into your configuration.

Here's a simple plugin that execs `echo \"hello\"` to return the
string "hello" to Nix.

```nix
# extra-builtins.nix
{ exec, ... }: {
  hello = exec ["echo" "\"hello\""];
}
```

```
bash$ nix repl --option extra-builtins-file $(pwd)/extra-builtins.nix

nix-repl> builtins.extraBuiltins.hello
"hello"
```

`exec` can be used to run `pass`, which will then be run as a child of
the process running the Nix interpreter; i.e. as your user and in your
shell, not as the user running the nix-daemon. This means it'll
properly prompt your TTY for your master password. `exec` requires
that the stdout of the process is formatted as a Nix expression, so a
wrapper is needed to reformat the output of `pass` to a Nix
expression.

```bash
#!/usr/bin/env bash

# nix-pass.sh

set -euo pipefail

f=$(mktemp)
trap "rm $f" EXIT
pass show "$1" > $f
nix-instantiate --eval -E "builtins.readFile $f"
```

```nix
# extra-builtins.nix
{ exec, ... }: {
  pass = name: exec [./nix-pass.sh name];
}
```

If you call this function with a password name from your
password-store, you will be prompted for your master password and the
contents of that password will be returned to Nix.

```
nix-repl> builtins.extraBuiltins.pass "secret-foo"
"bar\n"
```

# Putting it all together

Finally, we can use this to deploy a key from an encrypted
password-store to the file system.

```nix
{ ... }: {
  deployment.keys.secret-foo.text = builtins.extraBuiltins.pass "secret-foo";
}
```

```bash
bash$ nixops deploy -d my-deployment --option extra-builtins-file $(pwd)/extra-builtins.nix
```

You can configure the `extra-builtins-file` option in `nix.conf` so
you never need to specify it, but I don't recommend giving all Nix
evaluations access to your password store. If the gpg-agent still has
the master password cached, you won't be prompted and your secret
could leak to less trustworthy Nix code.

# Conclusion

This takes a bit of setup, but it achieves highly declarative secret
management. Secrets are safely shared between trusted machines,
protected by GPG, and deployed to machines simply by declaring the
password-store name and destination. This can be used for both local
machines and remote NixOps deployments, so this could even be a sane
way to deploy secrets to production environments.
