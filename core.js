var $if = function(cond, f1, f2) {
  if(cond) {
    return f1();
  } else {
    return f2();
  }
};
var $apply = function(f) {
  return f();
};
var $tick = window.setImmediate ? setImmediate : setTimeout;
var $extend = Object.assign;
var http = {
  get: function(url) {
    return function task(cb) {
      var xhr = new XMLHttpRequest();
      xhr.open('GET', url, true);
      xhr.responseType = 'json';
      xhr.onload = function(e) {
        if (this.status == 200) {
          cb(this.response);
        }
      };
      xhr.send();
    };
  }
};

var render = function(html) {
  return function task() {
    document.body.innerHTML = html;
  }
};
var $noop = function(){}
var count = 0;
var $runTask = function(task) {
  $tick(function() {
    var newTask = task();
    console.log(++count);
    if(newTask) {
      $runTask(newTask);
    }
  });
}
var run = function() {
  $runTask(main);
};
