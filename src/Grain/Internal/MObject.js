'use strict';

const newImpl = () => {
  return {};
};

export { newImpl as new };

export function keys(o) {
  return Object.keys(o);
}

export function values(o) {
  return Object.values(o);
}

export function has(k, o) {
  return !!o[k];
}

export function getImpl(k, o) {
  return o[k];
}

export function set(k, v, o) {
  o[k] = v;
}

export function del(k, o) {
  delete o[k];
}

export function unsafeGet(k, o) {
  return o[k];
}
