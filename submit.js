
var page = require('webpage').create();
var system = require("system");

page.open("http://www.w3schools.com/php/demo_form_validation_complete.php", function(status){
	page.includeJs('https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js', function(){
		
		page.onLoadFinished = function() {
			page.render("after_submit.png");
			phantom.exit();
		};

		page.evaluate(function(){
			$("body > form:nth-child(3) > input:nth-child(1)").val("Camilo");
			$("body > form:nth-child(3) > input:nth-child(5)").val("camilo@gmail.com");
			$("body > form:nth-child(3) > input:nth-child(9)").val("www.camilo.com");
			$("body > form:nth-child(3) > textarea:nth-child(13)").val("carreta");
			$("body > form:nth-child(3) > input:nth-child(17)").prop("checked", true);
			$("body > form:nth-child(3) > input:nth-child(22)").click();
		});

		page.render("before_submit.png");
		//phantom.exit();
	});
	
console.log('listo el pollo!');
});

