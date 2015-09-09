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
