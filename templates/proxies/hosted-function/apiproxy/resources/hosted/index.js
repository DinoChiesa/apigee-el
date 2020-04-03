/* jshint esversion: 8, node:true, strict:implied */
/* global process */

const http = require('http');
const url = require('url');
const util = require('util');

function logWrite() {
  let time = (new Date()).toString(),
      tstr = '[' + time.substr(11, 4) + '-' +
    time.substr(4, 3) + '-' + time.substr(8, 2) + ' ' +
    time.substr(16, 8) + '] ';
  console.log(tstr + util.format.apply(null, arguments));
}

logWrite('node.js application starting...');

function requestHandler(req, resp) {
  let query = url.parse(req.url, true).query;
  logWrite(req.method, req.url);
  resp.setHeader("Content-Type", "application/json");
  resp.end(JSON.stringify({
    date: new Date(),
    inboundUrl : req.url,
    msg: 'Hello, ' + (query.name || 'World') + '!'
  }));
}

const svr = http.createServer(requestHandler);

process.on('exit', function (code) {
logWrite('Script terminating with code %s', code);
});

process.on('uncaughtException', function (err) {
    logWrite(err.stack);
});

svr.listen(process.env.PORT || 3000, function() {
  logWrite('Node HTTP server is listening');
});
