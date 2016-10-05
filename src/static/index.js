// pull in desired CSS/SASS files
require( './styles/pure.css' );
require( './styles/styles.css' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
Elm.Main.embed( document.getElementById( 'main' ) );
