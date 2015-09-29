$register("Main",
["My.Util"],
$apply(function (My$Util) {
   var startApp = function (upd,
   view) {
      var listener = function (state) {
         return function (e) {
            var newState = $extend(state,
            {"model": upd(e,state.model)});
            var onEvent = listener(newState);
            return render(view(newState.model)(onEvent));
         };
      };
      var loop = function (e,state) {
         return $if(e.type === "init",
         function () {
            return $apply(function () {
               return http.get("sample.json",
               function (p) {
                  return render(view(state.model)(listener(state)));
               },
               function (p) {
                  var newState = $extend(state,
                  {"model": upd({"type": "loaded"
                                ,"data": p.data},
                  state.model)});
                  return render(view(newState.model)(listener(newState)));
               });
            });
         },
         function () {
            return null;
         });
      };
      return loop({"type": "init"},
      {"mainLoop": loop
      ,"model": upd({},null)});
   };
   var pararell = function (f,g) {
      return function (cb) {
         var loop = function (pf,pg) {
            return $if(!pf && !pg,
            function () {
               return $apply(function () {
                  return f(function (pf) {
                     return loop(pf,pg);
                     return g(function (pg) {
                        return loop(pf,pg);
                     },
                     function (e,data) {
                        return loop(pf,
                        $extend(pg,
                        {"error": e,"data": data}));
                     });
                  },
                  function (e,data) {
                     return loop($extend(pf,
                     {"error": e,"data": data}),
                     pg);
                  });
               });
            },
            function () {
               return $if(pf && (pf.data || pf.error) && pg && (pg.data || pg.error),
               function () {
                  return $apply(function () {
                     return cb(pf,pg);
                  });
               },
               function () {
                  return $apply(function () {
                     return null;
                  });
               });
            });
         };
      };
   };
   var upd = function (e,model) {
      var a = console.log("action:",
      e);
      return $if(e.type === "loaded",
      function () {
         return $apply(function () {
            return $extend(model,
            {"data": json.stringify(e.data)});
         });
      },
      function () {
         return $if(e.type === "event",
         function () {
            return $apply(function () {
               return $extend(model,
               {"count": model.count + 1});
            });
         },
         function () {
            return $apply(function () {
               return {"title": "Hello"
                      ,"data": null
                      ,"count": 0};
            });
         });
      });
   };
   var view = function (model) {
      var a = console.log("model:",
      model);
      return html("<h1>" + model.title + "</h1>" + "<div>" + model.count + "</div>" + "<div>" + model.data + "</div>");
   };
   var main = function (e,state) {
      var a = console.log(e,state);
      return $if(e === "a:waiting",
      function () {
         return $apply(function () {
            return update({"a": "waiting"});
         });
      },
      function () {
         return $if(e === "a:done",
         function () {
            return $apply(function () {
               return update({"a": "done"});
            });
         },
         function () {
            return $if(e === "b:waiting",
            function () {
               return $apply(function () {
                  return update({"b": "waiting"});
               });
            },
            function () {
               return $if(e === "b:done",
               function () {
                  return $apply(function () {
                     return update({"b": "done"});
                  });
               },
               function () {
                  return $if(state.a === "done" && state.b === "done",
                  function () {
                     return $apply(function () {
                        return render(html("<h1>hello</h1>"));
                     });
                  },
                  function () {
                     return $if(!state.a,
                     function () {
                        return $apply(function () {
                           return timeout(2000,
                           "a:waiting",
                           "a:done");
                        });
                     },
                     function () {
                        return $if(!state.b,
                        function () {
                           return $apply(function () {
                              return timeout(1000,
                              "b:waiting",
                              "b:done");
                           });
                        },
                        function () {
                           return $apply(function () {
                              return null;
                           });
                        });
                     });
                  });
               });
            });
         });
      });
   };
   return {"startApp": startApp
          ,"pararell": pararell
          ,"upd": upd
          ,"view": view
          ,"main": main};
}));