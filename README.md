# PoorScript

Yes, immutability! No, side-effects!

(Work in progress)

## Syntax

Assignment values to objects are not allowed.

|JavaScript|PoorScript|
|:--|:--|
|`a.b = c`|N/A|
|`Object.assign(a, b)`|`a # b`|

## Sample
```javascript
startApp = (init, upd, view) => {
  loop = (e, state) => {
    if (e == "afterUpdate") {
      render(view(state.model));
    } else {
      loop("afterUpdate", state # {
        model: init.model
      });
    };
  };
  loop(0, {});
};

init = {
  model: {
    title: "Hello"
  }
};

upd = (e, model) => {
  model # {
    title: e.data
  };
};

view = (model) => {
  "<h1>" + model.title + "</h1>";
};
main = startApp(init, upd, view);
```
