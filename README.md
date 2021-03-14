# subsonic.el

This is a subsonic client for emacs using mpv for music playing.

## Setup

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents

    machine SUBSONIC_URL login USERNAME password PASSSWORD port subsonic

The `port` section here is used to identify the line as a subsonic
server must be `subsonic`

If you want to set the default volume set `mpv-default-volume`

## Usage
The package isnt on melpa yet so it must be added to load-path for usage

Example use-package config:
```
(use-package subsonic
  :load-path "subsonic.el/"
  :commands subsonic
  :bind (("C-c m" . subsonic))
  :custom
  (subsonic-enable-art t))
```

Use the `subsonic` command to open a transiente with commonly used
commands available

set `subsonic-enable-art` to `t` to enable album art.

## Info

This uses some code from docker.el for examples of transient and
tabulated-list-mode as well as the mpv logic from mpv.el

This has only been tested with gonic however it should function with
other servers

