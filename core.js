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
var pipe = function(task1, next) {
  return function task(model, cb) {
    task1(model, function(model, something) {
      var task2 = next(something);
      setTimeout(task2.bind(null, model, cb));
    });
  };
};
var render = function(html) {
  return function task(model, cb) {
    document.body.innerHTML = html;
    cb(model);
  }
};
var action = function(e) {
  return function task(model, cb) {
    run(e, model, cb.bind(null, model));
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
  setTimeout(task.bind(null, model, $noop));

};
