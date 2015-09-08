# PoorScript

Yes, immutability! No, side-effects!

(Work in progress)

## Syntax

### Statements

A semicolon is required at the end of every statements.
All variables are declared in local scope. Keywords like `var`, `let` are not required.

```javascript
a = 1;
```

### Assignment

Assignment values to objects are not allowed.
The `#` operator merges two objects and returns new object.

|JavaScript|PoorScript|
|:--|:--|
|`var a = b`|`a = b`|
|`a.b = c`|N/A|
|`Object.assign(a, b)`|`a # b`|

### if

`if` returns the result.

```javascript
a = if (true) 1 else 0;
```

### Function

The last expression will be returned.
```javascript
distance = (x, y) => {
  a = x^2 + y^2;
  sqrt(a);
};
```

### Block

Block is used for make new scope.

```javascript
a = 0;
b = {
  a = 1;
  a + 5;
};
// a = 0
// b = 6
```

### Avoid stack overflow

`tick` lets system call the passed function at the next tick.

```javascript
loop = (count) => {
  _ = console.log(count);
  tick(() => {
    loop(count + 1);
  });
}
```

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
  () => {
    loop(0, {});
  };
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
