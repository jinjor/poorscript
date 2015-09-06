
startApp = (init, upd, view) => {
  (e, state) => {
    a = console.log(e);
    if(e == "afterUpdate") {
      render(view(state.model));
    } else {
      pipe(update("model", init.model), () => {action("afterUpdate")});
    };
  };
};

init = {
  model : {
    title: "Hello"
  }
};
upd = (e, model) => {
  model = model # {
    title: e.data
  };
  model
};

view = (model) => {
  "<h1>" + model.title + "</h1>";
};
main = startApp(init, upd, view);
