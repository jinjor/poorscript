
startApp = (view) => {
  {{{b(c(d(e())))}}}
};

main = (e, state) => {
  a = console.log(e);
  ret = if(e == "afterUpdate") {
    render(view(state));
  } else {
    pipe(update("a", "Hello!"), () => {action("afterUpdate")});
  };
  ret;
};

view = (state) => {
  "<h1>" + state.a + "</h1>";
};
