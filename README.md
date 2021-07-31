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

You'll need to have a stable internet connection in order for the elm build
script to download all its dependencies and compile successfully.

For an debug build, replace `--optimize` in the last command with `--debug`.
To compress the result according to the recommendations
[here](https://guide.elm-lang.org/optimization/asset_size.html), run the following:

    uglifyjs dist/avocomm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output dist/avocomm.min.js
    mv dist/avocomm.min.js dist/avocomm.js

## Creating a release

First, modify README.md to target the next release.

Second, run the build process above.

Third, use the following commands to create the prebuilt release:

    rm -f context-collapse-prebuilt.tgz
    cd ..
    tar czvf context-collapse/context-collapse-prebuilt.tgz -T context-collapse/manifest.txt


