# subsonic.el

[![MELPA](https://melpa.org/packages/subsonic-badge.svg)](https://melpa.org/#/subsonic)

This is a subsonic client for emacs using mpv for music playing.

## Setup

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents
    
    machine SUBSONIC_URL login USERNAME password PASSWORD port subsonic
    

The `port` section here is used to identify the line as a subsonic
server must be `subsonic`

## Usage
The package is available on melpa as `subsonic`

Example use-package config:
```
(use-package subsonic
  :commands subsonic
  :bind (("C-c m" . subsonic))
  :custom
  (subsonic-enable-art t))
```

Use the `subsonic` command to open a transient with commonly used
commands available

- `subsonic-enable-art` to `t` to enable album art
- `subsonic-mpv-default-volume` to set default mpv volume

## Info

This uses some code from docker.el for examples of transient and
tabulated-list-mode as well as the mpv logic from mpv.el

This has only been tested with gonic however it should function with
other servers

## Contributing/Issues

Please send any patches or share any issues you may have on the mailing list here:
https://lists.sr.ht/~amk/public-inbox

Perfer sending mail to the mailinglist before opening at todo ticket --
https://todo.sr.ht/~amk/subsonic.el

## Screenshots
![album list view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/artist.png)
![podcasts view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/podcasts.png)
