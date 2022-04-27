// index.js
// ------------------------------------------------------------------
//
// created: Wed Jul  7 10:28:16 2021
// last saved: <2021-July-07 10:28:18>

/* jshint esversion:9, node:true, strict:implied */
/* global process, console, Buffer */

// index.js
// ------------------------------------------------------------------
//
/* jshint node:true, strict:implied, esversion:9*/
/* global process */

const fs = require('fs'),
      http = require('http'),
      url = require('url'),
      applications = JSON.parse(fs.readFileSync('applications.json', 'utf-8'));

function requestHandler(request, response) {
  let queryparams = url.parse(request.url, true).query,
      paramA = queryparams.a,
      payload = {};
  if (paramA) {
    payload.results = applications.filter(item => (item["soa-classification"] === paramA) );
    response.writeHead(200, {"Content-Type": "application/json"});
  }
  else {
    payload.error = "you did not supply the required query param";
    response.writeHead(400, {"Content-Type": "application/json"});
  }
  response.end(JSON.stringify(payload, null, 2) + '\n');
}

function serverListen() {
  let svr = http.createServer(requestHandler);
  svr.listen(9000, function() {
    console.log('Node HTTP server is listening');
  });
}

process.on('exit', function (code) {
   console.log('Script terminating with code %s', code);
});

process.on('uncaughtException', function (err) {
    console.log(err.stack);
});

console.log('node.js application starting...');

serverListen();
