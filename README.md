# subsonic.el

[![MELPA](https://melpa.org/packages/subsonic-badge.svg)](https://melpa.org/#/subsonic)

This is a subsonic client for emacs using mpv for music playing.

## Setup

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents

    machine SUBSONIC_URL login USERNAME password PASSWORD

The `subsonic-host` must be set to the same value as SUBSONIC_URL in
your init file, example below.

## Usage

The package is available on melpa as `subsonic`

Example use-package config:

```
(use-package subsonic
  :commands subsonic
  :bind (("C-c m" . subsonic))
  :custom
  (subsonic-url "coolsubsonic.example.com")
  (subsonic-enable-art t))
```

Use the `subsonic` command to open a transient with commonly used
commands available.

For a list of available configuration options check `customize-group subsonic`

## Info

This uses some code from docker.el for examples of transient and
tabulated-list-mode as well as the mpv logic from mpv.el

This has only been tested with gonic however it should function with
other servers

## Contributing/Issues

For quick questions, I'm `amk` on libera.chat, you can find me in #emacs

Please send any patches or share any issues you may have on the mailing list here:
https://lists.sr.ht/~amk/public-inbox

or alternatively if you prefer a pull-request style flow :
https://codeberg.org/amk/subsonic.el


## Screenshots

![album list view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/artist.png)
![podcasts view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/podcasts.png)
