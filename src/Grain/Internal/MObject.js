'use strict';

exports.new = function() {
  return {};
}

exports.keys = function(o) {
  return Object.keys(o);
}

exports.values = function(o) {
  return Object.values(o);
}

exports.unsafeSize = function(o) {
  return Object.keys(o).length;
}

exports.has = function(k, o) {
  return !!o[k];
}

exports.getImpl = function(k, o) {
  return o[k];
}

exports.set = function(k, v, o) {
  o[k] = v;
}

exports.del = function(k, o) {
  delete o[k];
}

exports.unsafeGet = function(k, o) {
  return o[k];
}
