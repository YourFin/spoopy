# spoopy
Safe(ish) cli http file indexer for development purposes

# Usage:
Just run `spoopy` in a directory that you want to serve; all files that are safe to serve under that directory will be available there.

For example, if you run `spoopy` from a directory like:

```
test-report.html
foo.json
test-results/
 - index.html
```

visiting `http://<hostname>:7979/foo.json` will serve `foo.json`, visiting `http://<hostname>:7979/test-results/test-report.html` will serve the `test-results/test-report.html` file, etc. 

Try `spoopy --help` or `spoopy -h` on the command line for more details on cli usage.

Spoopy matches file extensions to MIME types from [this list](https://github.com/YourFin/spoopy/blob/main/app/Data/MimeType.hs#L34) in an attempt to get files to render correctly in browsers, defaulting to `application/octet-stream` in the case that no matching extension is found.

## Safe(ish)?

spoopy will only serve files that:
1. Have a real file path - symlinks fully resolved - below the directory that you run it from.
1. Have "other" read posix file permissions (i.e. must look like `***-***-r**` - can be added with `chmod o+r <file>`)

## Installing

I'll work on putting together statically linked binaries in the future that can just be thrown into your `$PATH` if there's enough interest, but for now:

1. Install the `stack` build tool:

``` sh
# https://docs.haskellstack.org/en/stable/README/#how-to-install
curl -sSL https://get.haskellstack.org/ | sh
# or
wget -qO- https://get.haskellstack.org/ | sh
```

1. Clone this repo: `git clone https://github.com/YourFin/spoopy.git`
1. Build and install `spoopy`: `cd spoopy && stack install`
1. Make sure that the resulting executable is in your `$PATH` (may require adding `PATH="$PATH:~/.local/bin"` to your `.[ba|z]shrc`)
1. Run `spoopy`!
