function main(e, model) {
  var ret;
  if (e.type == '$INIT') {
    get1 = $http('GET', 'http://aaa');
    req = $wait([get1], function([e]) {
      var get2 = $http('GET', 'http://aaa/' + e.data);
      var get3 = $http('GET', 'http://bbb/' + e.data);
      return $wait([get2, get3], function([e2, e3]) {
        return $action('READY', e2.data + e3.data);
      });//must return task
    });
    ret = $wait($update('count', 0), function(e, model) {
      return req;
    });
  } else if (e.type == 'READY') {
    ret = $update('user', e.data);
  } else if (e.type == 'CLICK') {
    ret = $update('count', model.count + 1);
  } else if (e.type == '$RENDER') {
    ret = render(world);
  }
  return ret;//must return task
}

function render($model) {
  var click = onClick(function(e) {
    return $action('CLICK');
  });
  var view = div([click], [model.user + world.count]);
  return $updateView(view)
}
