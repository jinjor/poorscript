import My.Util;

startApp = (init, upd, view) => {
  loop = (e, state) => {
    a = console.log(e);
    if (e == "afterUpdate") {
      loop("afterUpdate2", state # {
        model: {
          title: 1
        }
      });
    } else if (e == "afterUpdate2") {
      render(view(state.model));
    } else {
      loop("afterUpdate", state # {
        model: init.model
      });
    };
  };
  loop(0, {});
};

init = {
  model: {
    title: "Hello"
  }
};
upd = (e, model) => {
  model = model# {
    title: e.data
  };
  model;
};

view = (model) => {
  "<h1>" + model.title + "</h1>";
};
main = startApp(init, upd, view);
