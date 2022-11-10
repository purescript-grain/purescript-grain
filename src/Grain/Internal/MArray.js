'use strict';

const newImpl = () => {
  return [];
};

export { newImpl as new };

export function cons(x, xs) {
  xs.unshift(x);
}

export function snoc(xs, x) {
  xs.push(x);
}

export function cutFrom(i, xs) {
  xs.splice(i);
}

export function deleteIfEqRef(x, xs) {
  var idx = xs.indexOf(x);
  if (idx < 0) {
    return;
  }
  xs.splice(idx, 1);
}

export function clear(xs) {
  xs.splice(0);
}
