$register("Main",
["My.Util"],
$apply(function (My$Util) {
   var startApp = function (init,
   upd,
   view) {
      var loop = function (e,
      state) {
         var a = console.log(e);
         return $if(e === "afterUpdate",
         function () {
            return $apply(function () {
               return loop("afterUpdate2",
               $extend(state,
               {"model": {"title": 1}}));
            });
         },
         function () {
            return $if(e === "afterUpdate2",
            function () {
               return $apply(function () {
                  return render(view(state.model));
               });
            },
            function () {
               return $apply(function () {
                  return loop("afterUpdate",
                  $extend(state,
                  {"model": init.model}));
               });
            });
         });
      };
      return loop(0,{});
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
   var init = {"model": {"title": "Hello"}};
   var upd = function (e,model) {
      var model = $extend(model,
      {"title": e.data});
      return model;
   };
   var view = function (model) {
      return "<h1>" + model.title + "</h1>";
   };
   var main = startApp(init,
   upd,
   view);
   return {"startApp": startApp
          ,"pararell": pararell
          ,"init": init
          ,"upd": upd
          ,"view": view
          ,"main": main};
}));