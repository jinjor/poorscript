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
var $extend = function(a, b) {
  return Object.assign({}, a, b);
};
var $modules = {};
var $register = function(name, imports, module) {
  $modules[name] = module;
};
var json = JSON;
var html = function(str) {
  return function(onEvent) {
    var div = document.createElement('div');
    div.innerHTML = str;
    div.addEventListener('click', function(e) {
      var task = onEvent({
        type: 'event',
        event: e
      });
      $runTask(task);
    });
    return div;
  };
};
var http = {
  get: function(url, cb1, cb2) {
    return function task() {
      var xhr = new XMLHttpRequest();
      xhr.open('GET', url, true);
      xhr.responseType = 'json';
      xhr.onload = function(e) {
        if (this.status == 200) {
          var newPromise = $extend(promise, {
            data: this.response
          })
          $runTask(cb2(newPromise));
        } else {
          var newPromise = $extend(promise, {
            error: this.response
          });
          $runTask(cb2(this.status));
        }
      };
      xhr.onerror = function(e) {
        var newPromise = $extend(promise, {
          error: e
        });
        cb2(newPromise);
      };
      xhr.send();
      var promise = {};
      $runTask(cb1(promise));
    };
  }
};

var render = function(element, cb) {
  return function task() {
    document.body.innerHTML = "";
    document.body.appendChild(element);
    cb && cb();
  }
};
var $noop = function(){}
var count = 0;
var $runTask = function(task) {
  if (!task) {
    return;
  }
  $tick(function() {
    var newTask = task();
    console.log(++count);
    if(newTask) {
      $runTask(newTask);
    }
  });
}
var $run = function() {
  $runTask($modules.Main.main);
};
