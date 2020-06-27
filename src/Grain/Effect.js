'use strict';

exports.forEImpl = function(lo, hi, f) {
  var i = lo;

  while (i < hi) {
    f(i)();
    i = (i + 1) | 0;
  }
}

exports.foreachEImpl = function(xs, f) {
  var i = 0;

  while (i < xs.length) {
    f(xs[i])();
    i = (i + 1) | 0;
  }
}

exports.sequenceEImpl = function(fs) {
  var i = 0;

  while (i < fs.length) {
    fs[i]();
    i = (i + 1) | 0;
  }
}

exports.forObjectEImpl = function(obj, f) {
  var k, i = 0, keys = Object.keys(obj);

  while (i < keys.length) {
    k = keys[i];
    f(k)(obj[k])();
    i = (i + 1) | 0;
  }
}
