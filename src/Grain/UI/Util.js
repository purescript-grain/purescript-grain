'use strict';

exports.setForeignImpl = function(name, foreign, element) {
  element[name] = foreign;
  return {};
}

exports.setAttributeNSImpl = function(ns, name, val, element) {
  element.setAttributeNS(ns, name, val);
  return {};
}

exports.removeAttributeNSImpl = function(ns, name, element) {
  element.removeAttributeNS(ns, name);
  return {};
}

exports.isPropertyImpl = function(name, element) {
  return name in element;
}

exports.isBooleanImpl = function(name, element) {
  return typeof element[name] === "boolean";
}
