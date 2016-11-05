This project's setup is based on the excellent `elm-webpack-starter`: https://github.com/moarwick/elm-webpack-starter

To start the local webserver, run
````
$ npm start
````

Editing `MyCss.elm` requires the static css file to be regenerated via
````
$ npm install -g elm-css
$ elm-css src/elm/Stylesheets.elm -o src/static/styles/
````
