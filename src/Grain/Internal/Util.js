'use strict';

exports.byIdx = function(xs, i) {
  return xs[i];
}

exports.byIdxNullable = function(xs, i) {
  return xs[i];
}

exports.mapNullable = function(f, a) {
  if (a === null || a === undefined) {
    return a;
  }
  return f(a);
}

exports.eqNullable = function(a, b) {
  if (a === null || a === undefined || b === null || b === undefined) {
    return false;
  }
  return a === b;
}

var readOnlyProps = { style: true, list: true, form: true, dropzone: true }

exports.shouldAttribute = function(name) {
  return readOnlyProps[name] === true;
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

exports.tailRecE = function(f, s) {
  var step = s;
  while (!step.done) {
    step = f(step);
  }
  return step;
}
