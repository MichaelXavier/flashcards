var express = require('express');
var app = express();
var yaml = require('yaml');
var fs = require('fs');

app.use(express.static('static'));
app.use(express.static('output'));

// load fixture data
var topics = yaml.eval(fs.readFileSync('fixtures/topics.yml', 'utf8'));

app.get('/topics', function(req, res) {
  res.json(topics);
});

app.listen(8000, function() {
  console.log("Listening on port 8000");
});
