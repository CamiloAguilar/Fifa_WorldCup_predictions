// scrape_EAsports.js.js

var webPage = require('webpage');
var page = webPage.create();
var fs = require('fs');
var path = 'EAsports_statistics.html'

page.settings.userAgent = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36';
page.viewportSize = {with: 1280, height: 1024}

page.open('https://www.easports.com/es/fifa/ultimate-team/fut/database/results?position_secondary=LF,CF,RF,ST,LW,LM,CAM,CDM,CM,RM,RW,LWB,LB,CB,RB,RWB&quality=gold,rare_gold', function (status) {
  page.includeJs('https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js', function(){
  	page.onLoadFinished = function(status) {
			console.log('Status: ' + status);
			page.render("after_submit.png");
			phantom.exit();
		};

	page.evaluate(function(){
			$(".ut-form_field").val("Camilo");
			//$("ul.discover-bar:nth-child(6) > li:nth-child(1)").click();
		});
	//phantom.exit();
  };

  //var content = page.content;
  //fs.write(path, content, 'w')
  //phantom.exit();
  console.log('listo el pollo!');
});