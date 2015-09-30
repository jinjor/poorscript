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

Assigning values to objects are not allowed.
Instead, the `#` operator merges two objects and returns new object.

|JavaScript|PoorScript|
|:--|:--|
|`var a = b`|`a = b`|
|`a.b = c`|N/A|
|`Object.assign(a, b)`|`a # b`|

### If

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

// TODO
