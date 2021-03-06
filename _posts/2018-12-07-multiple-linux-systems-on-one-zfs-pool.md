---
layout: post
title:  "Multiple Linux Systems on One ZFS Pool"
date:   2018-12-07
categories:
---

ZFS is a volume manager in addition to a filesystem. Traditional
volume managers are essentially virtual RAID systems. LVM, for
instance, allows you to combine several block devices into one block
device, which you can then partition as normal. ZFS's main filesystem
concept, known as "datasets", are different though. A ZFS pool isn't a
block device that needs to be formatted and partitioned, and datasets
aren't partitions formatted with filesystems. Rather, ZFS abstracts
all of that away. A dataset is just a logical filesystem managed by
the ZFS pool, and all datasets in the pool share the space provided by
all the underlying block devices.

So if you ever need multiple Linux systems on one hard drive (or any
group of block devices), they can all share space if you simply
install each one in its own dataset in the same ZFS pool. In this
post, I'll be doing this with two NixOS installations, but in theory
it would work with any Linux distros that support a ZFS root
filesystem.

This post assumes you already have NixOS installed on ZFS. I described
how I did this [in another
post](/2018/12/06/encrypted-boot-on-zfs-with-nixos.html), including
encryption and encrypted `/boot`. Once you have this, there's not even
any need for a live CD; you can do everything from the main system, at
least with NixOS.

First, create the ZFS dataset that the new system will be installed on.

```
$ zfs create zroot/new-system -o mountpoint=legacy
$ mount -t zfs zroot/new-system /mnt
```

Unfortunately, we do need to let NixOS try to install Grub, since the
`grub.cfg` is needed, and Grub will complain that `/boot` is not an
EFI partition. The easiest way to fix this is to trick Grub by
creating a small zvol, formatting it to FAT32, and pointing
`efiSysMountPoint` at it. Grub will install itself there, and it'll
just end up completely ignored, since the main system's Grub will be
in charge.

```
$ zfs create -V 50M zroot/new-system/fake-efi
$ gdisk /dev/zvol/zroot/new-system/fake-efi
# Create a GPT and a single partition taking the 
# entire drive, with partition type hex code: ef00
...
$ mkfs.fat /dev/zvol/zroot/new-system/fake-efi-part1
$ mkdir /mnt/efi
$ mount /dev/zvol/zroot/new-system/fake-efi-part1 /mnt/efi
```

Now generate the NixOS config. But it needs a few edits.

```
$ nixos-generate-config --root /mnt
```

Edit `/mnt/etc/nixos/configuration.nix` to boot from ZFS, in the same
way as your main system. For me, with LUKS, this meant copying my
extra initrd to the new `/mnt/boot` and replacing the default
`boot.loader` lines with this:

```
  boot.initrd.luks.devices.zroot = {
    device = "/dev/disk/by-uuid/YOUR_UUID_HERE";
    keyFile = "/keyfile.bin";
  };

  boot.loader.grub = {
    device = "nodev";
    efiSupport = true;
    extraInitrd = "/boot/initrd.keys.gz";
    enableCryptodisk = true;
    zfsSupport = true;
    efiInstallAsRemovable = true;
    forceInstall = true;
  };
  boot.loader.efi.efiSysMountPoint = "/efi";

  networking.hostId = "75c7b830";
```

Of course don't forget to create a new `networking.hostId` for the new
system. I got mine from `openssl rand -hex 4`. Some of the Grub
configuration above may not be necessary, since the Grub installed by
the new system won't actually be used, but I didn't take the time to
figure out what is unneeded.

The last change required for `/mnt/etc/nixos/configuration.nix` has to
do with the fake EFI zvol. `nixos-generate-config` more than likely
set its device to something like `/dev/zd0p1` or something. This
*might* work, but ZFS provides no guarantees that a zvol will always
be given the same name in `/dev`. So this value needs to be overridden
manually. Luckily NixOS provides `mkForce` to override the
autogenerated definition, without having to edit the hardware config
everytime it's regenerated. Add this to
`/mnt/etc/nixos/configuration.nix`:

```
  fileSystems."/efi".device = pkgs.lib.mkForce "/dev/zvol/zroot/new-system/fake-efi-part1";
```

That should be enough to install NixOS on this dataset.

```
$ nixos-install --root /mnt
```

Finally, we just need to teach the Grub installed by the main system
how to boot the other system. I just added a custom `menuentry` that
loads the new system's `grub.cfg`. You could in theory have the new
system install Grub to your actual ESP alongside the main system's
Grub and use Grub's `chainloader` command in the main `grub.cfg` to
load the new system's boot loader directly. But I couldn't quite
figure out how to accomplish this, and it would have required a second
LUKS password prompt from the second Grub anyway, which I find very
undesirable.

```
  boot.loader.grub.extraEntries = ''
    menuentry "New System" {
      search --set=drive1 --label zroot
      configfile ($drive1)//new-system/@/boot/grub/grub.cfg
    }
  '';
```

Adding this to your *main system's* `configuration.nix` will add the
new system's Grub menu under a submenu to the main system's Grub
menu. `nixos-rebuild` the main system, reboot, and you should see a
`New System` menu entry that will bring you to the new system's Grub
menu, from which you can boot the new system.

So that's two NixOS systems on the same ZFS pool! Eventually, I plan
to use this as the basis for dual booting macOS on my MacBook (hear me
out). I want to have macOS running on a zvol that's managed by NixOS
in order to get all the benefits of ZFS, like snapshots and
`znapzend`. So I'm planning on virtualizing macOS and passing hardware
like USB and the GPU through directly with VFIO. But that's a post for
another time.
