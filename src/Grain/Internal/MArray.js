'use strict';

exports.new = function() {
  return [];
}

exports.cons = function(x, xs) {
  xs.unshift(x);
}

exports.snoc = function(xs, x) {
  xs.push(x);
}

exports.cutFrom = function(i, xs) {
  xs.splice(i);
}

exports.deleteIfEqRef = function(x, xs) {
  var idx = xs.indexOf(x);
  if (idx < 0) {
    return;
  }
  xs.splice(idx, 1);
}

exports.clear = function(xs) {
  xs.splice(0);
}
