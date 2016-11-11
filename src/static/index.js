// pull in desired CSS/SASS files
require( './styles/pure.css' );
require( './styles/styles.css' );


// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

app.ports.fileSelected.subscribe(function (id) {
  var node = document.getElementById(id);
  if (node === null) {
    return;
  }

  var file = node.files[0];
  console.log(file);
	var formData = new FormData();
	formData.append('filedata', file);
	formData.append('from', 'across-lite-binary');
	formData.append('to', 'json');
  var xhr = new XMLHttpRequest();
  var url = "http://localhost:1234/json";
  xhr.open('POST', url, true);
  xhr.onload = function () {
    if (xhr.status === 200) {
      var data = JSON.parse(xhr.response);
      app.ports.fileContentRead.send(data);
    } else {
      alert('An error occurred!');
    }
  };
  xhr.send(formData);
});

