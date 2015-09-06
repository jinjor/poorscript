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