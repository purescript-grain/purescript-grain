'use strict';

exports.new = function() {
  return {};
}

exports.keysImpl = function(o) {
  return Object.keys(o);
}

exports.valuesImpl = function(o) {
  return Object.values(o);
}

exports.sizeImpl = function(o) {
  return Object.keys(o).length;
}

exports.hasImpl = function(k, o) {
  return !!o[k];
}

exports.getImpl = function(k, o) {
  return o[k];
}

exports.setImpl = function(k, v, o) {
  o[k] = v;
}

exports.delImpl = function(k, o) {
  delete o[k];
}
