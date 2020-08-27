/* jshint esversion:9, node:true, strict:implied */
/* global process */

const http = require('http'),
      url = require('url'),
      util = require('util');

function logWrite() {
  let time = (new Date()).toString(),
      tstr = '[' + time.substr(11, 4) + '-' +
    time.substr(4, 3) + '-' + time.substr(8, 2) + ' ' +
    time.substr(16, 8) + '] ';
  console.log(tstr + util.format.apply(null, arguments));
}

logWrite('node.js application starting...');

function requestHandler(req, resp) {
  let queryparams = url.parse(req.url, true).query;
  logWrite(req.method, req.url);
  resp.status(200)
    .setHeader("Content-Type", "application/json")
    .end(JSON.stringify({
      date: new Date(),
      inboundUrl : req.url,
      msg: 'Hello, ' + (queryparams.name || 'World') + '!'
    }));
}

const svr = http.createServer(requestHandler);

process.on('exit', code =>
  logWrite('Script terminating with code %s', code) );

process.on('uncaughtException', e => logWrite(e.stack));

svr.listen(process.env.PORT || 3000, () =>
  logWrite('Node HTTP server is listening'));
