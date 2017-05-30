// index.js
// ------------------------------------------------------------------
//
// Description goes here....
//
// created: Sun Mar 12 10:32:17 2017
// last saved: <2017-March-12 11:48:30>

var express=require('express');
var app = express();
var x=0;

app.get('/servicex', function(request, response) {
  response.header('x',x)
    .status(200)
    .send({ message: 'ok' });
});

app.put('/servicex', function (request, response) {
  x=x+1;
  response.header('Content-Type', 'application/json')
    .status(200)
    .send({ value: x });
});

// default behavior
app.all(/^\/.*/, function(request, response) {
  response.header('Content-Type', 'application/json')
    .status(404)
    .send('{ "message" : "This is not the server you\'re looking for." }\n');
});

app.listen(3000, function () { console.log('Example app listening on port 3000!'); });
