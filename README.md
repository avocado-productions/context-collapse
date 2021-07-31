# Context Collapse

## Running the engine

The easiest way to get started is to download the latest release and
use the included script to start a webserver.

    wget https://github.com/avocado-productions/context-collapse/releases/download/v0.0.3/context-collapse-prebuilt.tgz
    tar xzvf context-collapse-prebuilt.tgz
    cd context-collapse
    python3 server.py

Then go to http://localhost:8020/ in your browser.

You can edit your game by modifying the file `script.camp`
in the `context-collapse` directory; the game will automatically
refresh when you change the script.

## Build

To build the engine from source, you need the 
[Elm compiler](https://guide.elm-lang.org/install/elm.html)
and the Camperdown repository (currently private).

    git clone -b contextcollapse git@github.com:brilliantorg/camperdown.git
    git clone -b parser git@github.com:avocado-productions/context-collapse
    cd context-collapse
    elm make src/Controller.elm --output=dist/avocomm.js --optimize

For an optimized build, replace `--optimize` in the last command with `--debug`.

You'll need to have a stable internet connection in order for the elm build
script to download all its dependencies and compile successfully.

## Creating a release

First, modify README.md to target the next release.

Second, run the build process above.

Third, use the following commands to create the prebuilt release:

    rm -f context-collapse-prebuilt.tgz
    cd ..
    tar czvf context-collapse/context-collapse-prebuilt.tgz -T context-collapse/manifest.txt


