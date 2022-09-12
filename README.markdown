dazu
====

`dazu` asks [random.org](https://random.org) to generate some random passwords.
To ensure the generated passwords are indeed random, the accompanying signature
is verified against the random.org's server key.  The key is compiled statically
into the `dazu` executable.

You need to [get](https://api.random.org/api-keys/beta) the API before you
run `dazu`.   Fortunately, it's free!  Once you have one, set the `RANDOMORG_API_KEY` environment
variable to it.

There is a couple of command line options:

  - `--length` - length of generated passwords (default: `12`)
  - `-n` - number of generated passwords (default: `10`)
  - `-v|--verbose` - print debug information (default: `false`)

An example
----------

```shell
$ RANDOMORG_API_KEY=... dazu --length 16 -n 5
BZm&^Zf7B8(YokE9
UVYWnZWXXqQT4Z8d
N$N)T*hWa1Ey+aAW
NuOCh7+uSc3a&IV!
_shNnez-sVFLBZkk
```
