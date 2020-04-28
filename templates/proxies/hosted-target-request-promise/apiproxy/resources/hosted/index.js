/* jshint esversion: 9, node: true, strict:implied */
/* global process, console */

const request = require('request-promise-native');
const http = require('http');
const url = require('url');
const util = require('util');
let counter = 0;

logWrite('node.js server starting...');

function logWrite() {
  var time = (new Date()).toString(),
      tstr = '[' + time.substr(11, 4) + '-' +
    time.substr(4, 3) + '-' + time.substr(8, 2) + ' ' +
    time.substr(16, 8) + '] ';
  console.log(tstr + util.format.apply(null, arguments));
}

function xform(body, response, resolveWithFullResponse) {
  return {httpResponse: response, body};
}

/* This is the function that handles _inbound_ requests */
function requestHandler(req, resp) {
  let query = url.parse(req.url, true).query;
  logWrite("inbound %s %s", req.method, req.url);
  counter++;
  let outboundRequestOptions = {
        method:"GET",
        uri: "https://buoyant-climate-208500.appspot.com",
        json: true,
        headers: {
          "Requested-Time": (new Date()).toISOString(),
          "Authorization": "Bearer FAKE-TOKEN-22C6A7FC-C1BA-4758-9BCB-D5FD6B17DE69"
        },
        transform: xform
      };

  /* for each inbound request, send a request _outbound_ */
  request(outboundRequestOptions)
    .then(response2 => {
      /* successful receipt of response from upstream.  */
      /* here, can transform it or just replay it to original client. */
      logWrite("recd status %s, %d bytes",
               response2.httpResponse.statusCode,
               response2.httpResponse.headers['content-length']);
      resp.setHeader("Content-Type", "application/json");
      resp.setHeader("Response-Received", (new Date()).toISOString());
      resp.setHeader("Request-Count", counter);
      resp.setHeader("Upstream-Status", response2.httpResponse.statusCode);
      resp.statusCode = 200;
      resp.end(JSON.stringify(response2.body));
    })
    .catch(e => {
      /* the upstream could not be contacted. */
      resp.setHeader("Content-Type", "application/json");
      resp.setHeader("Request-Count", counter);
      resp.statusCode = 500;
      resp.statusMessage = 'Internal Server Error' ;
      resp.end(JSON.stringify({"error": util.format(e)}));
    });
}

process.on('exit', function (code) {
   logWrite('Script terminating with code %s', code);
});

process.on('uncaughtException', function (err) {
    logWrite(err.stack);
});

const svr = http.createServer(requestHandler);
svr.listen(process.env.PORT || 3000, function() {
  logWrite('HTTP server is listening on port ' + util.format(svr.address().port));
});
