'use strict';

exports.byIdx = function(xs, i) {
  return xs[i];
}

exports.raf = function(f) {
  window.requestAnimationFrame(f);
}

exports.head = function() {
  return document.head;
};

exports.createTextNode = function(text) {
  return document.createTextNode(text);
}

exports.createElement = function(tag) {
  return document.createElement(tag);
}

exports.createElementNS = function(tag) {
  return document.createElementNS("http://www.w3.org/2000/svg", tag);
}

exports.appendChild = function(node, parent) {
  return parent.appendChild(node);
}

exports.removeChild = function(node, parent) {
  parent.removeChild(node);
}

exports.putChild = function(i, node, parent) {
  var target = parent.childNodes[i];
  if (target) {
    parent.insertBefore(node, target);
  } else {
    parent.appendChild(node);
  }
}

exports.nodeIndexOf = function(node) {
  return Array.prototype.indexOf.call(node.parentNode.children, node);
}

exports.setTextContent = function(text, node) {
  node.textContent = text;
}

exports.setAny = function(name, any, element) {
  element[name] = any;
}

exports.setAttribute = function(name, val, element) {
  element.setAttribute(name, val);
}

exports.removeAttribute = function(name, element) {
  element.removeAttribute(name);
}

exports.isProperty = function(name, element) {
  return name in element;
}

exports.isBoolean = function(name, element) {
  return typeof element[name] === "boolean";
}

exports.mkEventListener = function (f) {
  return function(evt) {
    f(evt)();
  }
}
