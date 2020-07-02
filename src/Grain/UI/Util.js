'use strict';

exports.nodeIndexOfImpl = function(node) {
  return Array.prototype.indexOf.call(node.parentNode.children, node);
}

exports.setAnyImpl = function(name, any, element) {
  element[name] = any;
}

exports.setAttributeNSImpl = function(ns, name, val, element) {
  element.setAttributeNS(ns, name, val);
}

exports.removeAttributeNSImpl = function(ns, name, element) {
  element.removeAttributeNS(ns, name);
}

exports.isPropertyImpl = function(name, element) {
  return name in element;
}

exports.isBooleanImpl = function(name, element) {
  return typeof element[name] === "boolean";
}
