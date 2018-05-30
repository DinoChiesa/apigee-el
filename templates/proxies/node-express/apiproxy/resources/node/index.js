// index.js

var express =require('express');
var moment = require('moment-timezone');
var app = express();
var x = 0;

app.get('/servicex', function(request, response) {
  response.header('x',x)
    .status(200)
    .send({
      message: 'ok',
      now : moment().tz('GMT').format()
    });
});

app.post('/servicex', function (request, response) {
  x=x+1;
  response.header('x', x)
    .status(200)
    .send({
      message: 'ok',
      now : moment().tz('GMT').format()
    });
});

// default behavior
app.all(/^\/.*/, function(request, response) {
  var payload = {
        incomingUrl : request.url,
        message : "This is not the server you're looking for."
      };
  response.header('Content-Type', 'application/json')
    .status(404)
    .send(payload);
});

process.on('exit', function (code) {
   console.log('Script terminating with code %s', code);
});

process.on('uncaughtException', function (err) {
    console.log(err.stack);
});

app.listen(3000, function () {
  console.log('Example app listening on port 3000!');
});
