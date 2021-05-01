# DevEnv

## Setting up environment on Arch Linux

First of all, install required packages:

``` sh
yay -S stack haskell-language-server-bin
```

I use **doom-emacs** to work with Haskell. You can refer to Haskell LSP setup for doom-emacs for information on setup (it is really not hard. the hardest step was to realize to install `haskell-language-server-bin` instead of `haskell-language-server`, as it includes `haskell-language-server-wrapper`, which selects appropriate HLS version for the project).

## How to use

To build and run a project, use

    stack run

from the project directory. This will download all the necessary dependencies, compile the project and start CodeWorld canvas server on address `http://localhost:3000`.

If you are developing on a remote machine, you may use 

``` sh
ssh -L 3000:localhost:3000 user@hostname
```

to connect to the remote CodeWorld Canvas server.


## Adding new code

Add new files to the `src` directory. Pay attention to the naming — start your files with upper case letter and name the modules with the same names as the files.

<!-- To use replace the main entrypoint, alter the `app/Main.hs` file to import the required module and run the required `IO ()` function. -->
