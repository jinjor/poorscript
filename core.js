var $if = function(cond, f1, f2) {
  if(cond) {
    return f1();
  } else {
    return f2();
  }
};
var pipe = function(task1, task2) {
  return function task(model, cb) {
    task1(model, function(model) {
      setTimeout(function() {
        task2(model, cb);
      });
    });
  };
};
var render = function(html) {
  return function task(model, cb) {
    document.body.innerHTML = html;
    setTimeout(function() {
      cb(model);
    });
  }
};
var action = function(e) {
  return function task(model, cb) {
    setTimeout(function() {
      run(e, model, function(model) {
        cb(model);
      });
    });
  }
};
var update = function(property, value) {
  return function task(model, cb) {
    console.log(property, value);
    model = JSON.parse(JSON.stringify(model));
    model[property] = value;
    cb(model);
  };
};
var $noop = function(){}
var run = function(e, model, cb) {
  model = model || {}
  cb = cb || $noop;
  var task = main(e, model);
  cb(model);
  setTimeout(function() {
    task(model, $noop);
  });

};
