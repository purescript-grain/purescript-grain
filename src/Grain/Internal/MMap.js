'use strict';

exports.new = function() {
  return new Map();
}

exports.getImpl = function(k, m) {
  return m.get(k);
}

exports.set = function(k, v, m) {
  m.set(k, v);
}

exports.del = function(k, m) {
  m.delete(k);
}

exports.unsafeGet = function(k, m) {
  return m.get(k);
}
