# Context Collapse

## Build

If you checked Context Collapse out of the repo, you'll need to compile the 
engine. This requires the [Elm compiler](https://guide.elm-lang.org/install/elm.html).

Optimized build:

    cd context-collapse
    elm make src/Controller.elm --output=dist/avocomm.js --optimize

Debug build:

    cd context-collapse
    elm make src/Controller.elm --output=dist/avocomm.js --debug

You'll need to have a stable internet connection in order for the elm build
script to download all its dependencies and compile successfully.


## Instructions to build and run

To play the game, start a webserver and go to http://localhost:8080/

    cd context-collapse
    python -m SimpleHTTPServer

Or upload the directory to a web server





Make sure you have npm installed, then from the root directory:

    $ npm i
    $ npm i elm
    $ npm start


The project will tell you what port it's running on (e.g.
http://localhost:8888/). Once compilation succeeds, you can then navigate
there in your browser to see the project.
