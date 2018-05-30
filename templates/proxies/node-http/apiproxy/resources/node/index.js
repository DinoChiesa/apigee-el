// index.js
// ------------------------------------------------------------------
//

'use strict';
const fs = require('fs');
var http = require('http');
var url = require('url');
var applications = JSON.parse(fs.readFileSync('applications.json', 'utf-8'));

function requestHandler(request, response) {
  var queryparams = url.parse(request.url, true).query;
  var paramA = queryparams.a;
  response.writeHead(200, {"Content-Type": "application/json"});
  var payload = {};
  if (paramA) {
    payload.results = applications.filter(function(item){ return (item["soa-classification"] === paramA); });
  }
  else {
    payload.error = "you did not supply the required query param";
  }
  response.end(JSON.stringify(payload, null, 2) + '\n');
}

process.on('exit', function (code) {
   console.log('Script terminating with code %s', code); 
});

process.on('uncaughtException', function (err) {
    console.log(err.stack);
});

console.log('node.js application starting...');
var svr = http.createServer(requestHandler);
svr.listen(9000, function() {
  console.log('Node HTTP server is listening');
});
