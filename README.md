# Context Collapse

## Instructions to build and run

If you downloaded a release, the easiest way to begin is by starting
a webserver. 

    tar xzvf context-collapse-prebuilt.tgz
    cd context-collapse
    python -m SimpleHTTPServer 8070

Then go to http://localhost:8070/ in your browser.

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

After doing the build process above, go into the parent directory and run
a `tar` command to collect the necessary files.

    rm -f context-collapse-prebuilt.tgz
    cd ..
    tar czvf context-collapse/context-collapse-prebuilt.tgz -T context-collapse/manifest.txt


