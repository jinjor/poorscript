import My.Util;

startApp = (upd, view) => {
  listener = (state) => {
    (e) => {
      newState = state # {
        model: upd(e, state.model)
      };
      onEvent = listener(newState);
      render(view(newState.model)(onEvent));
    };
  };

  loop = (e, state) => {
    if (e.type == "init") {
      http.get("sample.json", (p) => {
        render(view(state.model)(listener(state)));
      }, (p) => {
        newState = state # {
          model: upd({
              type: "loaded",
              data: p.data
            }, state.model)
        };
        render(view(newState.model)(listener(newState)));
      });
    } else null;
  };
  loop({type: "init"}, {
    mainLoop: loop,
    model: upd({}, null)
  });
};

pararell = (f, g) => {
  (cb) => {
    loop = (pf, pg) => {
      if(!pf && !pg) {
        f((pf) => {
          loop(pf, pg);
          g((pg) => {
            loop(pf, pg);
          }, (e, data) => {
            loop(pf, pg # {
              error: e,
              data: data
            });
          });
        }, (e, data) => {
          loop(pf # {
            error: e,
            data: data
          }, pg);
        });
      } else if(pf && (pf.data || pf.error) && pg && (pg.data || pg.error)) {
        cb(pf, pg);
      } else {
        null;
      };
    };
  };
};

upd = (e, model) => {
  a = console.log("action:", e);
  if (e.type == "loaded") {
    model # {
      data : json.stringify(e.data)
    };
  } else if (e.type == "event") {
    model # {
      count : model.count + 1
    };
  } else {
    {
      title: "Hello",
      data : null,
      count: 0
    };
  };
};

view = (model) => {
  a = console.log("model:", model);
  html("<h1>" + model.title + "</h1>" + "<div>" + model.count + "</div>" + "<div>" + model.data + "</div>");
};
main = (e, state) => {
  a = console.log(e, state);

  if(e == "a:waiting") {
    update({
      a: "waiting"
    });
  } else if(e == "a:done") {
    update({
      a: "done"
    });
  } else if(e == "b:waiting") {
    update({
      b: "waiting"
    });
  } else if(e == "b:done") {
    update({
      b: "done"
    });
  } else if(state.a == "done" && state.b == "done") {
    render(html("<h1>hello</h1>"));
  } else if(!state.a) {
    timeout(2000, "a:waiting", "a:done");
  } else if(!state.b) {
    timeout(1000, "b:waiting", "b:done");
  } else {
    null;
  };


};
