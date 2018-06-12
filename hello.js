console.log('Hello, world!');
page.open('http://m.bing.com', function(status) {

  var title = page.evaluate(function(s) {
    return document.querySelector(s).innerText;
  }, 'title');

  console.log(title);
  phantom.exit();

});