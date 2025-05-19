# HASC

A Haskell rewrite of the GNU `wc` (word count) utility.
This project doesn’t really serve a specific purpose — it was written mostly for learning Haskell.

## 🚀 Usage

Install `cabal` and run `cabal build`
This will build the project using cabal.
To run it find the executable inside the `dist-newstyle` directory. 
On linux you can directly run:
`/dist-newstyle/build/*/*/wc-0.1.0.0/x/wc/build/wc/wc [options] [files]`

Alternatively you can run:
`cabal run -- [options] [files]`

It will do its magic on the provided files and count words or whatever the options specify.
If no file is provided, the program will look at the stdin, so you can pipe stuff in or input your own text.

Make sure you have cabal installed.

To build the project:
```bash
cabal build
```

This compiles the executable and places it inside the dist-newstyle directory.

On Linux, you can run the binary directly like this:
```bash
./dist-newstyle/build/*/*/wc-0.1.0.0/x/wc/build/wc/wc [options] [files]
```

Or simply run:
```bash
cabal run -- [options] [files]
```

If no file is provided, it reads from stdin, so you can either pipe content into it or type input manually.

## Options

Run with the `--help` flag to get an overview over all options. (e.g. `cabal run -- --help`)

Available flags:

<table>
  <tr>
    <th>Option</th>
    <th>Description</th>
  </tr>
  <tr>
    <td><code>-c</code>, <code>--bytes</code></td>
    <td>Print the byte counts</td>
  </tr>
  <tr>
    <td><code>-m</code>, <code>--chars</code></td>
    <td>Print the character counts</td>
  </tr>
  <tr>
    <td><code>-l</code>, <code>--lines</code></td>
    <td>Print the newline (line) counts</td>
  </tr>
  <tr>
    <td><code>-L</code>, <code>--max-line-length</code></td>
    <td>Print the maximum display width of any line</td>
  </tr>
  <tr>
    <td><code>-w</code>, <code>--words</code></td>
    <td>Print the word counts</td>
  </tr>
  <tr>
    <td><code>--help</code></td>
    <td>Display help and exit</td>
  </tr>
  <tr>
    <td><code>--version</code></td>
    <td>Show version information and exit</td>
  </tr>
</table>

Output is always printed in this order: lines, words, characters, bytes, max line length.

If multiple files are provided, it prints totals as well.
