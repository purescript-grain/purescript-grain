'use strict';

exports.new = function (val) {
  return { value: val };
}

exports.read = function (ref) {
  return ref.value;
}

exports.modify = function (f, ref) {
  ref.value = f(ref.value);
  return ref.value;
}

exports.write = function (val, ref) {
  ref.value = val;
}
