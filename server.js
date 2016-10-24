var express = require('express');
var app = express();
var yaml = require('yaml');
var fs = require('fs');

app.use(express.static('static'));
app.use(express.static('output'));

function parseID(s) {
  return parseInt(s, 10);
}

// Topics
var topicsTable = yaml.eval(fs.readFileSync('fixtures/topics.yml', 'utf8'));

//TODO; switch on content type
app.get('/topics', function(req, res) {
  res.json(topicsTable);
});

app.get('/topics/:id', function(req, res) {
  var id = parseID(req.params.id);
  var topic = topicsTable.find(function(x) { return x.id == id;});
  if (topic) {
    res.json(topic);
  } else {
    res.status(404).end();
  }
});

//cards
var cardsTable = yaml.eval(fs.readFileSync('fixtures/cards.yml', 'utf8'));

app.get('/topics/:topic_id/cards', function(req, res) {
  var topic_id = parseID(req.params.topic_id);
  var cards = cardsTable.filter(function(x) { return x.topic_id == topic_id;});
  res.json(cards);
});


app.listen(8000, function() {
  console.log("Listening on port 8000");
});
