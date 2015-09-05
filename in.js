

main = (e, state) => {
  if(e) {
    render(state.a);
  } else {
    pipe(update("a", "Hello!"), action("render"));
  }
};
