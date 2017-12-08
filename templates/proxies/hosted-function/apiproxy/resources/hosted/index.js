var http = require('http');
var url = require('url');

console.log('node.js application starting...');
console.log(process.env);

var svr = http.createServer(function(req, resp) {
  var query = url.parse(req.url, true).query;

  console.log(req.method, req.url);

  resp.setHeader("Content-Type", "application/json");
  resp.end(JSON.stringify({
      date: new Date(),
      msg: 'Hello, ' + (query.name || 'World') + '!'
  }));
});

svr.listen(process.env.PORT || 3000, function() {
  console.log('Node HTTP server is listening');
});