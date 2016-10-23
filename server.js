var express = require('express');
var app = express();
var yaml = require('yaml');
var fs = require('fs');

app.use(express.static('static'));
app.use(express.static('output'));

// load fixture data
var topics = yaml.eval(fs.readFileSync('fixtures/topics.yml', 'utf8'));

function parseID(s) {
  return parseInt(s, 10);
}

//TODO; switch on content type
app.get('/topics', function(req, res) {
  res.json(topics);
});

app.get('/topics/:id', function(req, res) {
  var id = parseID(req.params.id);
  var topic = topics.find(function(x) { return x.id == id;});
  if (topic) {
    res.json(topic);
  } else {
    res.status(404).end();
  }
});

app.listen(8000, function() {
  console.log("Listening on port 8000");
});
