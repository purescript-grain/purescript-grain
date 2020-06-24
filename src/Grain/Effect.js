'use strict';

exports.sequenceEImpl = function(fs) {
  var i = 0;

  while (i < fs.length) {
    fs[i]();
    i = (i + 1) | 0;
  }

  return {};
}

exports.traverseEImpl = function(f, xs) {
  var i = 0, results = new Array(xs.length);

  while (i < xs.length) {
    results[i] = f(xs[i])();
    i = (i + 1) | 0;
  }

  return results;
}

exports.forObjectEImpl = function(obj, f) {
  var k, i = 0, keys = Object.keys(obj);

  while (i < keys.length) {
    k = keys[i];
    f(k)(obj[k])();
    i = (i + 1) | 0;
  }

  return {};
}
