'use strict';

exports.whenE = function(bool, f) {
  if (bool) {
    f();
  }
}

exports.forE = function(lo, hi, f) {
  var i = lo;

  while (i < hi) {
    f(i);
    i = (i + 1) | 0;
  }
}

exports.foreachE = function(xs, f) {
  var i = 0;

  while (i < xs.length) {
    f(xs[i]);
    i = (i + 1) | 0;
  }
}

exports.sequenceE = function(fs) {
  var i = 0;

  while (i < fs.length) {
    fs[i]();
    i = (i + 1) | 0;
  }
}
