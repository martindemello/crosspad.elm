// pull in desired CSS/SASS files
require('./styles/pure.css');
require('./styles/styles.css');

// js libraries
var FileSaver = require('file-saver');

// inject bundled Elm app into div#main
var Elm = require('../elm/Main');
var app = Elm.Main.embed(document.getElementById('main'));

var make_request = function (url, type, request, on_success) {
  console.log(request);
	var formData = new FormData();
  for (var prop in request) {
    formData.append(prop, request[prop]);
  }
  var xhr = new XMLHttpRequest();
  xhr.response_type = type;
  xhr.open('POST', url, true);
  xhr.onload = function () {
    if (xhr.status === 200) {
      on_success(xhr.response);
    } else {
      alert('An error occurred!');
    }
  };
  console.log(xhr);
  xhr.send(formData);
}

app.ports.fileSelected.subscribe(function (request) {
  console.log(request);
  var node = document.getElementById(request.file_element);
  if (node === null) {
    return;
  }
  
  var file = node.files[0];
  
  var req = {
    'filedata': file,
    'from': request.format,
    'to': 'json'
  };

  var on_load = function (response) {
    var data = JSON.parse(response);
    app.ports.fileContentRead.send(data);
  };
  
  var url = "http://localhost:1234/json";
  make_request(url, 'json', req, on_load);
});


app.ports.saveFileRequested.subscribe(function (request) {
  console.log(request);
  
  var req = {
    'filedata': request.data,
    'to': request.format,
    'from': 'json'
  };

  var on_load = function (response) {
    var blob = new Blob([response], {type: 'application/octet-stream'});
    FileSaver.saveAs(blob, 'downloaded_xword');
  };
  
  var url = "http://localhost:1234/blob";
  make_request(url, 'arraybuffer', req, on_load);
});
