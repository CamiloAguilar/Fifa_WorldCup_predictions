// scrape_EAsports.js.js

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'EAsports_statistics.html'

page.open('https://www.easports.com/es/fifa/ultimate-team/fut/database/results?position_secondary=LF,CF,RF,ST,LW,LM,CAM,CDM,CM,RM,RW,LWB,LB,CB,RB,RWB&quality=gold,rare_gold', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});