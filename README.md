# subsonic.el

This is a subsonic client for emacs using mpv for music playing.

## Setup

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents
    
    machine SUBSONIC_URL login USERNAME password PASSSWORD port subsonic
    

The `port` section here is used to identify the line as a subsonic
server must be `subsonic`

## Usage
The package isnt on melpa yet so it must be added to load-path for usage

Example use-package config:
```
(use-package subsonic
  :load-path "subsonic.el/" ;; in .emacs.d/subsonic.el
  :commands subsonic
  :bind (("C-c m" . subsonic))
  :custom
  (subsonic-enable-art t))
```

Use the `subsonic` command to open a transiente with commonly used
commands available

Set `subsonic-enable-art` to `t` to enable album art.
The default mpv volume can be set with `mpv-default-volume`


## Info

This uses some code from docker.el for examples of transient and
tabulated-list-mode as well as the mpv logic from mpv.el

This has only been tested with gonic however it should function with
other servers

## Screenshots
![album list view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/artist.png)
![podcasts view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/podcasts.png)
