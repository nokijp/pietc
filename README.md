# pietc

[![Build Status](https://travis-ci.org/nokijp/pietc.svg?branch=master)](https://travis-ci.org/nokijp/pietc)

pietc is a Piet compiler.
This compiles Piet programs to native executable programs.


## Requirements

This program requires the following libraries and programs:

- [Stack](https://www.haskellstack.org/), a build tool for Haskell (required for building)
- [LLVM 6](https://llvm.org/)
- ld (now only support `/usr/bin/ld`)

Make sure that the following commands run successfully:

```bash
stack --version
llvm-config --libs --link-shared
/usr/bin/ld -v
```

On macOS, it may be required to run the following command in `llvm-config --libdir`:

```bash
ln -s libLLVM.dylib libLLVM-6.dylib
```


## Installing pietc

Run the following command in the root directory of this project (where this README is located):

```bash
stack install
```

This installs `pietc` into `~/.local/bin/pietc`.


## Running pietc

### Compiling a Piet program

```bash
pietc -o executable program.png
```

This automatically guesses the codel size of the program.

The following options are available:

#### `--codel-size`

Specify the codel size.

The default value is to guess the size.

#### `--additional`

Specify the method how to deal with additional colors such as orange, gray, etc.
The default value is `nearest`.

| Value | Description |
|---|---|
| `--additional=white` | Treating as a white codel. |
| `--additional=black` | Treating as a black codel. |
| `--additional=nearest` | Treating as a codel which has the nearest color. |

#### `--multicolor`

Specify the method how to deal with multicolored codels.
The default value is `average`.

| Value | Description |
|---|---|
| `--multicolor=white` | Treating as a white codel. |
| `--multicolor=black` | Treating as a black codel. |
| `--multicolor=center` | Picking up a center pixel. |
| `--multicolor=modal` | Finding the modal color, the most frequent color. |
| `--multicolor=average` | Calculating an average color. |

#### `-O`

Specify the optimization level.

| Value | Description |
|---|---|
| `-O0` | No optimization. |
| `-O1` | Making the program faster (level 1). |
| `-O2` | Making the program faster (level 2). |
| `-O3` | Making the program faster (level 3). |
| `-Os` | Reducing code size (level 1). |
| `-Oz` | Reducing code size (level 2). |

### Running a Piet program on JIT

```bash
pietc --run program.png
```

The same options as in `--codel-size` are available.

### Visualizing a Piet program syntax

```bash
pietc --graph program.png
```

This generates a DOT script.
To convert into an image, use tools such as Graphviz.
