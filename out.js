var startApp = function (init,
upd,
view) {
   var loop = function (e,
   state) {
      var a = console.log(e);
      return $if(e === "afterUpdate",
      function () {
         return $apply(function () {
            return function () {
               return loop("afterUpdate2",
               $extend(state,
               {"model": {"title": 1}}));
               ;
            };
            ;
         });
      },
      function () {
         return $if(e === "afterUpdate2",
         function () {
            return $apply(function () {
               return render(view(state.model));
               ;
            });
         },
         function () {
            return $apply(function () {
               return function () {
                  return loop("afterUpdate",
                  $extend(state,
                  {"model": init.model}));
                  ;
               };
               ;
            });
         });
      });
      ;
   };
   return loop(0,
   $apply(function () {    ;}));
   ;
};
var init = {"model": {"title": "Hello"}};
var upd = function (e,model) {
   var model = $extend(model,
   {"title": e.data});
   return model;
};
var view = function (model) {
   return "<h1>" + model.title + "</h1>";
   ;
};
var main = startApp(init,
upd,
view);
;