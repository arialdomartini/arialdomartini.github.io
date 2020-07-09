---
layout: post
title: "Unbricking a bricked TADA68"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Mechanical Keyboards
- QMK
---
### TL;DR

1. [Get an ISP Programmer](#get-an-isp-programmer)
2. [Install avrdude](#Install-avrdude)
3. [Download a boatloader](#Download-a-bootloader)
4. [Connect the ISP Programmer to PC](#connect-the-isp-programmer-to-pc)
5. [Disconnect the keybard from the PC](#disconnect-the-keyboard-from-the-pc)
6. Connect the PCB to the ISP Programmer
7. [Flash the bootloader](#Flash the bootloader)

<!--more-->
# Why does TADA68 bricks
// TODO explain what does happens with Massdrop Loader

# Repair a bricked TADA68 
## Get an ISP Programmer
As an ISP Programmer, go with USBasp or a USBasp clone. Look for the following:

> USBasp USBISP 3.3V / 5V AVR Programmer USB ATMEGA8

* [eBay.com - 3.3V / 5V USBASP USBISP AVR Programmer ATMEGA8](https://www.ebay.com/sch/i.html?_from=R40&_trksid=p2380057.m570.l1313.TR0.TRC0.A0.H0.X3.3V+%2F+5V+USBASP+USBISP+AVR+Programmer+ATMEGA8.TRS1&_nkw=3.3V+%2F+5V+USBASP+USBISP+AVR+Programmer+ATMEGA8&_sacat=0)

I bought this one:

* [amazon.it - ILS - 3.3V / 5V USBASP USBISP AVR programmer for ATMEGA8 ATMEGA128](https://www.amazon.it/gp/product/B07ZP25N7R/ref=ppx_yo_dt_b_asin_title_o02_s00?ie=UTF8&psc=1)

If you go with a different ISP programmer, you will probably need to modify accordingly the `avrdude`'s parameters.

## Install avrdude

```bash
sudo pacman --sync avrdude
```

## Download a bootloader
Several bootloaders are available. I tried the following 2:

* [MassDrop Bootloader](#massdrop-bootloader) is the factory installed bootloader. It's not the best option, as it is deleted when the keyboard is ejected while in flash mode.
* [MegaAVR DFU USB](#megaavr-dfu-usb-bootloader)

### MassDrop Bootloader
Download it from the repository [https://github.com/Massdrop/mdloader](https://github.com/Massdrop/mdloader):

* [https://github.com/rwilbur/tada68-bootloader-restore/raw/master/mass_bootloader_tada68.hex](https://github.com/rwilbur/tada68-bootloader-restore/raw/master/mass_bootloader_tada68.hex)

The source code is available at:

* [https://github.com/rwilbur/tada68-bootloader-restore](https://github.com/rwilbur/tada68-bootloader-restore)

### MegaAVR DFU USB Bootloader
Download it from
* http://ww1.microchip.com/downloads/en/DeviceDoc/megaUSB_DFU_Bootloaders.zip
or
* http://www.microchip.com/wwwproducts/en/ATMEGA32U4 under `Documents -> Software`

Unzip it. Then, use

```bash
ATMega32U4-usbdevice_dfu-1_0_0.hex
```

**Disclaimer**: I wasn't able to make it work.


## Disconnect the keybard from the PC
The keyboard needn't be connected throught its USB cable. I'm not sure what would happen if the following steps are performed while the keyboard is connected. Disconnect it, just in case.

## Connect the ISP Programmer to PC
That's the hardest part. I needed the 2 hands of a second person.

I used the following set of test leads

[https://www.amazon.it/gp/product/B088LN43JT/ref=ppx_yo_dt_b_asin_title_o01_s00?ie=UTF8&psc=1](https://www.amazon.it/gp/product/B088LN43JT/ref=ppx_yo_dt_b_asin_title_o01_s00?ie=UTF8&psc=1)

which proved to be terrible. 

// TODO add a photo of the TADA68 ISP's pins
// TODO add a photo of the test leads
// TODO add a photo of the USBasp's pins
// TODO show the test leads claws
// TODO explain how to get work around the claws


## Flash the bootloader

Run:

```bash
sudo avrdude -c USBasp -p atmega32u4 -U flash:w:mass_bootloader_tada68.hex
```

It all takes about 10 seconds.

After that, disconnect the ISP programmer, and connect the keyboard to the PC with the ordinary USB cable.

# Notes
`avrdude` has tons of parameters. We would use just 3 of them:

```bash
sudo avrdude -p {partno} -c {programmer} -U {config-file}
```

| Parameter | Meaning               | Value                          | Reference                             |
|:----------|:----------------------|:-------------------------------|---------------------------------------|
| `-p`      | partno: the chip part | `atmega32u4`                   | [partno](#partno)                     |
| `-c`      | programmer-id         | `USBasp`                       | [programmer-id](#programmer-id)       |
| `-U`      | memory operation      | `flash:w:${bootloaderHexFile}` | [Memory operation](#memory-operation) |


## partno
`partno` should be the chip name. 

For TADA68 it *should* be:

```bash
ATmega32U4
```

which corresponds to 

```bash
m32u4
```

This could be inferred from:

```bash
avrdude -p \? 2>&1| grep ATmega32U4 
  m32u4    = ATmega32U4
```

## programmer-id
I made it work using `USBasp`. 

For what I got, a programmer is the electrical description of the ISP Programmer, including all its pins. Therefore, this parameter depends on the ISP programmer being used.

There is a large set rogrammers defined in `/etc/avrdude.conf`. If your programmer is not included there, it can be defined in a custom configuration file, and passed to `avrdude` via the `-C` option.

## Memory operation
This is the operation to perform, together with its parameters. It's specified with the `-U` option in `avrdude`. I used:

```bash
-U flash:w:${bootloaderHexFile}`
```

where `bootloaderHexFile` is the `.hex` file downloded in [Download a bootloader](#download-a-bootloader)


# References

* [Bricked TADA68 and How I Fixed it - A Novice's Tale](https://www.reddit.com/r/MechanicalKeyboards/comments/66sji0/bricked_tada68_and_how_i_fixed_it_a_novices_tale/)
* [Flashing bootloaders on AVR](https://deskthority.net/wiki/Flashing_bootloaders_on_AVR)
* [How to un-brick Tada68 with Raspberry Pi](https://www.reddit.com/r/MechanicalKeyboards/comments/fu7rc0/how_to_unbrick_tada68_with_raspberry_pi/)
* [[help] working on un-bricking my tada68, i think i'm close. help with the next step?](https://www.reddit.com/r/MechanicalKeyboards/comments/934jo0/help_working_on_unbricking_my_tada68_i_think_im/)
* [Replace Pro Micro bootloader with QMK DFU](https://www.reddit.com/r/olkb/comments/8sxgzb/replace_pro_micro_bootloader_with_qmk_dfu/)
