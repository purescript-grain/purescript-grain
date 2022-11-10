'use strict';

export function byIdx(xs, i) {
  return xs[i];
}

export function byIdxNullable(xs, i) {
  return xs[i];
}

export function keyNullable(f, i, a) {
  if (a === null || a === undefined) {
    return null;
  }
  return f(i, a);
}

export function mapNullable(f, a) {
  if (a === null || a === undefined) {
    return null;
  }
  return f(a);
}

export function eqNullable(a, b) {
  if (a === null || a === undefined || b === null || b === undefined) {
    return false;
  }
  return a === b;
}

var readOnlyProps = { style: true, list: true, form: true, dropzone: true }

export function shouldAttribute(name) {
  return readOnlyProps[name] === true;
}

export function raf(f) {
  window.requestAnimationFrame(f);
}

export function head() {
  return document.head;
};

export function createTextNode(text) {
  return document.createTextNode(text);
}

export function createElement(tag) {
  return document.createElement(tag);
}

export function createElementNS(tag) {
  return document.createElementNS("http://www.w3.org/2000/svg", tag);
}

export function unsafeParentNode(node) {
  return node.parentNode;
}

export function appendChild(node, parent) {
  return parent.appendChild(node);
}

export function removeChild(node, parent) {
  parent.removeChild(node);
}

export function replaceChild(newNode, oldNode, parent) {
  parent.replaceChild(newNode, oldNode);
}

export function putChild(i, node, parent) {
  var target = parent.childNodes[i];
  if (target) {
    parent.insertBefore(node, target);
  } else {
    parent.appendChild(node);
  }
}

export function nodeIndexOf(node) {
  return Array.prototype.indexOf.call(node.parentNode.children, node);
}

export function setTextContent(text, node) {
  node.textContent = text;
}

export function setAny(name, any, element) {
  element[name] = any;
}

export function setAttribute(name, val, element) {
  element.setAttribute(name, val);
}

export function removeAttribute(name, element) {
  element.removeAttribute(name);
}

export function isProperty(name, element) {
  return name in element;
}

export function isBoolean(name, element) {
  return typeof element[name] === "boolean";
}

export function mkEventListener (f) {
  return function(evt) {
    f(evt)();
  }
}

export function whenE(bool, f) {
  if (bool) {
    f();
  }
}

export function forE(lo, hi, f) {
  var i = lo;

  while (i < hi) {
    f(i);
    i = (i + 1) | 0;
  }
}

export function foreachE(xs, f) {
  var i = 0;

  while (i < xs.length) {
    f(xs[i]);
    i = (i + 1) | 0;
  }
}

export function sequenceE(fs) {
  var i = 0;

  while (i < fs.length) {
    fs[i]();
    i = (i + 1) | 0;
  }
}

export function tailRecE(f, s) {
  var step = s;
  while (!step.done) {
    step = f(step);
  }
  return step;
}
