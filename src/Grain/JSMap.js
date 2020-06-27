'use strict';

exports.new = function() {
  return new Map();
}

exports.getImpl = function(k, m) {
  return m.get(k);
}

exports.setImpl = function(k, v, m) {
  m.set(k, v);
}

exports.delImpl = function(k, m) {
  m.delete(k);
}
