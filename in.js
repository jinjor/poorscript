
startApp = (init, upd, view) => {
  (e, state) => {
    a = console.log(e);
    if(e == "afterUpdate") {
      render(view(state.model));
    } else {
      pipe(update("model", init), () => {action("afterUpdate")});
    };
  };
};

init = "Hello";
upd = (e, model) => {
  model
};

view = (model) => {
  "<h1>" + model + "</h1>";
};
main = startApp(init, upd, view);
