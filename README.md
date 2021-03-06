# subsonic.el

This is a subsonic client for emacs using mpv for music playing.

## Setup

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents

    machine SUBSONIC_URL login USERNAME password PASSSWORD port subsonic

The `port` section here is used to identify the line as a subsonic
server must be `subsonic`

If you want to set the default volume set `mpv-default-volume`

## Usage

Use `subsonic-artists` for a browsable list of artists

Use `subsonic-podcasts` for a browsable list of podcasts

## Info

This uses some code from docker.el for examples of transient and
tabulated-list-mode as well as the mpv logic from mpv.el

This has only been tested with gonic however it should function with
other servers
