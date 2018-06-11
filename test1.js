var page = require('webpage').create();
page.settings.userAgent = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36';
page.viewportSize = {with: 1280, height: 1024}

page.open('https://www.detectmybrowser.com/', function(status){
	console.log("Status: " + status);

	if(status === "success"){
		page.render('example.png');
		//console.log(page.plainText);
	}
	phantom.exit();
});