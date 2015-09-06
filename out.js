var startApp = function (init,
upd,
view) {
   return function (e,state) {
      var a = console.log(e);
      return $if(e === "afterUpdate",
      function () {
         return render(view(state.model));
         ;
      },
      function () {
         return pipe(update("model",
         init.model),
         function () {
            return action("afterUpdate");
         });
         ;
      });
      ;
   };
   ;
};
var init = {"model": "Hello"};
var upd = function (e,model) {
   return model;
};
var view = function (model) {
   return "<h1>" + model + "</h1>";
   ;
};
var main = startApp(init,
upd,
view);
;