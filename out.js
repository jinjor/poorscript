var startApp = function (init,
upd,
view) {
   return function (e,state) {
      var a = $if(e,
      console.log(e),
      0);
      return $if(e === "afterUpdate",
      $apply(function () {
         return render(view(state.model));
         ;
      }),
      $if(e === "something",
      $apply(function () {
         return render(view(state.model));
         ;
      }),
      $apply(function () {
         return pipe(update("model",
         init.model),
         function () {
            return action("afterUpdate");
         });
         ;
      })));
      ;
   };
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