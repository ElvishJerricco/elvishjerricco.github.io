---
layout: post
title:  "Encrypted /boot on ZFS with NixOS"
date:   2018-12-06
categories:
---

**UPDATE:** This does not work with LUKS 2, because Grub does not
support reading from it. At least on NixOS, LUKS 2 is now the
default. I have not updated this post to show how to force it to use
LUKS 1, and I'm not sure that's a good idea anyway. I'm moving my
system back to a normal `/boot` because of this and because Grub takes
absolutely forever to decrypt the disk. Grub also has issues with new
ZFS feature flags on occasion, and I'm not willing to take that risk.

---

Encryption and ZFS are each extremely valuable to have on your root
disk. ZFS provides integrity checking and snapshotting, among *many*
other things. And encryption can prevent data from being stolen or
tampered with by attackers with physical access (assuming the system
is powered off, or you use multi-key encryption like APFS and the
right keys are currently discarded).

However, most people forgo this value for `/boot`, instead leaving it
on an unencrypted FAT32 partition or something. Considering your
kernel and grub config lives there, that doesn't seem very wise to
me. A physical attacker doesn't need to attack your kernel, so the
encryption isn't a very important advantage in this case, but with
Secure Boot, TPM, and memory encryption like AMD's TSME, you can at
least make it harder, though obviously not impossible, to attack the
hardware. But the focus of this post is on keeping your kernel on ZFS,
where you can at least find out when a block containing part of your
kernel has become corrupted. A corrupted kernel is something you
really want to avoid.

Fortunately the answer to both questions is pretty easy: Grub. Grub
happens to have support for both LUKS and ZFS, if you enable
them. This will leave Grub on an unencrypted partition with no
integrity checking, so it's up to Secure Boot or something to verify
that, but I won't get into that today.

ZFS has recently gained support for native encryption, currently in
pre-release 0.8.0-rc2. I chose LUKS over this for a couple of
reasons.

1. Grub doesn't understand it, so you can't encrypt `/boot` with
it. Again, I know the value of encrypting `/boot` is limited, but it
is there. Also, I'm not sure if Grub can understand pools that have an
unencrypted `/boot` dataset, but other encrypted datasets. It probably
does, but I didn't test it.

2. ZFS encryption is actually kind of leaky. Per `man zfs`:

  > zfs will not encrypt metadata related to the pool structure,
  > including dataset names, dataset hierarchy, file size, file holes,
  > and dedup tables.

  This isn't a big deal, but I see no reason to open these holes when
  LUKS exists. LUKS just encrypts every block blindly, so it can't
  possibly leak this kind of information. There are definitely use
  cases where having ZFS encryption makes sense, but just trying to
  encrypt your whole disk isn't one of them. It's about as easy and
  more secure to use LUKS.

So without further ado, here are the instructions. This is largely
adapted from [this
gist](https://gist.github.com/ladinu/bfebdd90a5afd45dec811296016b2a3f),
with some minor differences for using ZFS, and a bit of info about
Grub that I learned while trying to get it working in VirtualBox.

Step 1 - The Disk
---

First, the disk needs to be setup after booting the [the live CD for
NixOS](https://nixos.org/nixos/download.html). This is pretty
standard, so if you know how to create a FAT32 partition and a LUKS
partition with a zpool on top of it, you can go ahead and skip to the
next section. Just remember that NixOS prefers your ZFS mountpoints to
be `legacy`, otherwise NixOS's stage 1 mounting can end up in a race
condition with ZFS's automounting and occasionally fail to boot.

The first partition needs to be an ESP that your EFI can understand
(sorry BIOS users, I don't know how to adapt this for you). FAT32 is
standardized in EFI, but some firmwares support more sophisticated
filesystems. The rest of the disk can be an empty partition that will
later be formatted with LUKS.

```
$ gdisk /dev/sdb
GPT fdisk (gdisk) version 1.0.4

Partition table scan:
  MBR: not present
  BSD: not present
  APM: not present
  GPT: not present

Creating new GPT entries in memory.

Command (? for help): o
This option deletes all partitions and creates a new protective MBR.
Proceed? (Y/N): Y

Command (? for help): n
Partition number (1-128, default 1): 
First sector (34-2097118, default = 2048) or {+-}size{KMGTP}: 
Last sector (2048-2097118, default = 2097118) or {+-}size{KMGTP}: +200M
Current type is 'Linux filesystem'
Hex code or GUID (L to show codes, Enter = 8300): ef00
Changed type of partition to 'EFI System'

Command (? for help): n
Partition number (2-128, default 2): 
First sector (34-2097118, default = 411648) or {+-}size{KMGTP}: 
Last sector (411648-2097118, default = 2097118) or {+-}size{KMGTP}: 
Current type is 'Linux filesystem'
Hex code or GUID (L to show codes, Enter = 8300): 
Changed type of partition to 'Linux filesystem'

Command (? for help): w

Final checks complete. About to write GPT data. THIS WILL OVERWRITE EXISTING
PARTITIONS!!

Do you want to proceed? (Y/N): Y
OK; writing new GUID partition table (GPT) to ./foo.
Warning: The kernel is still using the old partition table.
The new table will be used at the next reboot or after you
run partprobe(8) or kpartx(8)
The operation has completed successfully.
```

In summary:

1. `o`: Create a new GPT
2. `n`: Create a new partition
3. Leave the first sector blank to choose the start of the disk.
4. `+200M` to make it 200 megabytes long.
5. `ef00` to mark it as an EFI partition. Most firmware will work
   without this, but it's good to do it anyway.
6. `n`: Create the second new partition
7. Defaults for everything, making a Linux partition taking up the
   rest of the disk.
8. `w`: Save the changes.

Of course, replace `/dev/sdb` with your actual drive.

Format the ESP

```
$ mkfs.fat /dev/sdb1
```

Encrypt the second partition with LUKS and open it.

```
$ cryptsetup luksFormat /dev/sdb2
$ cryptsetup luksOpen /dev/sdb2 decrypted-disk-name
```

Make the ZFS pool, and make sure to use the `by-id` link for the
decrypted disk, preferably the one with the long unique identifier in
the name.

```
$ zpool create -o ashift=12 -O mountpoint=none zroot /dev/disk/by-id/dm-uuid-CRYPT-LUKS1-deadbeef-decrypted-disk-name
$ zfs create zroot/root -o mountpoint=legacy
```

Step 2 - The NixOS Config
---

This is where things will start to be a little different than
normal. Mount the filesystems such that NixOS will understand them,
but do **not** mount the ESP at `/mnt/boot`. Instead, mount it at
`/mnt/efi`. We want `/mnt/boot` to be a regular folder sitting in the
ZFS dataset, so that it is integrity checked and encrypted.

```
$ mount -t zfs zroot/root /mnt
$ mkdir /mnt/efi
$ mount /dev/sdb1 /mnt/efi
```

One annoying thing about Grub's LUKS support is that it can't pass the
key on to the kernel. So you'll be prompted for your password twice:
once in the EFI console when Grub needs to decrypt the disk, and once
in NixOS stage 1, when Linux is trying to decrypt the disk. Luckily we
can fix this by storing the key on the encrypted drive in the
initrd. This is safe because you have to decrypt the disk to get this
key. Once booted, the file will belong to `root`, so you'll need
`root` access to read it. This may seem like an extra vulnerability,
but in fact anyone with `root` access can already get the key since it
is stored in memory:

```
$ dmsetup table --showkey decrypted-disk-name
```

So the level of access to the decryption key is identical this
way. Generate and add an additional key, and store it in an
`initrd`. Later, grub will be configured to append this `initrd` to
the one installed by NixOS.

```
$ dd if=/dev/urandom of=./keyfile.bin bs=1024 count=4
$ cryptsetup luksAddKey /dev/sdb2 ./keyfile.bin
$ mkdir /mnt/boot
$ echo ./keyfile.bin | cpio -o -H newc -R +0:+0 --reproducible | gzip -9 > /mnt/boot/initrd.keys.gz
```

With the ZFS filesystem mounted at `/mnt`, and the ESP mounted at `/mnt/efi`, generate the NixOS config:

```
$ nixos-generate-config --root /mnt
```

Usually this works out of the box, but in the case of ZFS over LUKS,
NixOS doesn't quite figure it out
automatically. `hardware-configuration.nix` does correctly detect the
ZFS pool and dataset, but it doesn't realize it requires a LUKS
device. In `/mnt/etc/nixos/configuration.nix`, add the following,
replacing `deadbeef` with the UUID pointing at your *encrypted*
device, not the unencrypted `dm-0` device:

```
  boot.initrd.luks.devices.decrypted-disk-name = {
    device = "/dev/disk/by-uuid/deadbeef";
    keyFile = "/keyfile.bin";
  };
```

Note that we set the `keyFile`option to the key that we generated and
added to the LUKS device. Next we'll see how to configure Grub to add
the initrd so the kernel can see the key file, among other
things. Make sure to delete any other `boot.loader... = ...;` lines
that were generated by default.

```
  boot.loader = {
    # Tell NixOS to install Grub as an EFI application in /efi
    efi.efiSysMountPoint = "/efi";

    grub = {
      device = "nodev"; # Do not install Grub for BIOS booting.
      efiSupport = true;
      extraInitrd = "/boot/initrd.keys.gz"; # Add our LUKS key to the initrd
      enableCryptodisk = true; # Allow Grub to boot from LUKS devices.
      zfsSupport = true;
    };

    # Different systems may require a different one of the following two
    # options. The first instructs Grub to install itself in an EFI standard
    # location. And the second tells it to install somewhere custom, but
    # mutate the EFI NVRAM so EFI knows where to find it. The former
    # should work on any system. The latter allows you to share one ESP
    # among multiple OSes, but doesn't work on a few systems (namely
    # VirtualBox, which doesn't support persistent NVRAM).
    #
    # Just make sure to only have one of these enabled.
    grub.efiInstallAsRemovable = true;
    efi.canTouchEfiVariables = false;
  };
```

With these settings, Grub knows:

- Where to install itself.
- How to support EFI.
- How to boot from LUKS.
- How to boot from ZFS.

Finally, we just need to set the `networking.hostId` option in
`configuration.nix`, which is needed by ZFS. The easiest way to get a
value for this option is to run this command:

```
$ head -c 8 /etc/machine-id
```

Edit the remainder of `configuration.nix` to your liking, and you should be good to install.

```
$ nixos-install
```

I sometimes get a bunch of errors saying:

```
You have a memory leak (not released memory pool):
[0x130e700] dtree
Internal error: Unreleased memory pool(s) found.
```

But they are harmless, and I don't think they're specific to this setup.

Once that's finished, the system should be ready to boot. On startup,
Grub will load as an EFI application, and it'll ask you for your LUKS
password in the EFI console. Unfortunately, Grub is very slow to
decrypt the disk; something like 8 seconds on my machine. Then it'll
see your `grub.cfg` in `/boot`, and render the regular Grub boot
screen. Once it boots Linux, NixOS's stage 1 will try to decrypt the
LUKS device using the key from the `extraInitrd` that Grub was
configured to load. Finally Nix will import the ZFS pool and
`pivot_root` to your root dataset.

And voila, encrypted, ZFS `/boot`.
