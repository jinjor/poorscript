

main = (e, state) => {
  if(e) {
    render(html(state));
  } else {
    pipe(update("a", "Hello!"), () => {action("update")});
  }
};

html = (state) => {
  state.a
};
