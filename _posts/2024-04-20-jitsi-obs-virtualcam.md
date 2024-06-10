---
layout: post
title: "How to stream the cam while recording it with OBS Studio
(Linux)"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- obs
- linux
---
You want both:

* to stream your cam during a video call
* *and* to record the original cam output to a file.

You need to setup Virtual Cam in OBS Studio.

* OBS Studio will record the original cam video.
* It will forward the stream to a virtual device.
* Jitsi will stream the virtual device video.

<!--more-->

## No need for plugins
There used to be dedicated plugins to support Virtual Cam (such as
[CatxFixh/obs-v4l2sink][obs-v4l2sink],
[exeldro/obs-virtual-cam-filter][exeldro] and
[Fenrirthviti/obs-virtual-cam][obs-virtual-cam]). Since OBS Studio
v26.1 those are not needed anymore (see [CatxFish/obs-v4l2sink Issue
#56][issue-56] and the [OBS Studio v26.1 release note][release-note])

Since OBS Studio v26.1, support to virtual cam is based on the
[v4l2loopback][v4l2loopback], a kernel module to create V4L2 loopback devices.

## Setup Linux
Install [v4l2loopback][v4l2loopback]. Instructions for Arch Linux are
at [v4l2loopback][arch-linux-wiki] wiki page.

The following should suffice:

```bash
yay -Sy linux-headers v4l2loopback-dkms
sudo modprobe v4l2loopback

;; optionally also install the command line tools
yay -Sy v4l2loopback-utils v4l-utils
```

## Setup OBS Studio
It's important that you first run OBS Studio and start the Virtual Cam
and *then* Jitsi.

Once v4l2loopback is installed, you will see a new button:

![Start Virtual Cam](static/img/obs-virtual-cam/obs-start-virtual-camera.png)

Click on the gear button and configure it as follows:

| Field            | Value                       |
|------------------|-----------------------------|
| Output Type      | Source                      |
| Output Selection | Video Capture Device (V4L2) |

![Configure Virtual Cam](static/img/obs-virtual-cam/configure-virtual-cam.png)


Now, setup your scene including your original cam. Then start the
Virtual Cam clicking on Start Virtual Camera.

Whatever is captured by the original cam is now forwarded to the
Virtual Camera.

## Run Jitsi
When you run Jitsi, it will *not* be able to use the original cam,
because it is used by OBS Studio. Instead, configure it to use the
Video Dummy Device:

![Jitsi Settings](static/img/obs-virtual-cam/jitsi-settings.png).

# References
* Former plugins
  * [umlaeute/v4l2loopback][v4l2loopback]
  * [CatxFish/obs-v4l2sink][obs-v4l2sink]
  * [exeldro/obs-virtual-cam-filter][exeldro]
  * [Fenrirthviti/obs-virtual-cam][obs-virtual-cam]
* [CatxFish/obs-v4l2sink Issue #56][issue-56]
* [OBS Studio v26.1.0 Release Notes][release-note]
* [Arch Linux Wiki - V4l2loopback][arch-linux-wiki]

[v4l2loopback]: https://github.com/umlaeute/v4l2loopback
[obs-v4l2sink]: https://github.com/CatxFish/obs-v4l2sink
[exeldro]: https://github.com/exeldro/obs-virtual-cam-filter
[obs-virtual-cam]: https://github.com/Fenrirthviti/obs-virtual-cam
[issue-56]: https://github.com/CatxFish/obs-v4l2sink/issues/56#issuecomment-753191690
[release-note]: https://github.com/obsproject/obs-studio/releases/tag/26.1.0
[arch-linux-wiki]: https://wiki.archlinux.org/title/V4l2loopback


