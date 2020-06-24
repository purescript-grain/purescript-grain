'use strict';

exports.setAnyImpl = function(name, any, element) {
  element[name] = any;
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
