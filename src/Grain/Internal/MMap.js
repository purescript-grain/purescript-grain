'use strict';

const newImpl = () => {
  return new Map();
};

export { newImpl as new };

export function getImpl(k, m) {
  return m.get(k);
}

export function set(k, v, m) {
  m.set(k, v);
}

export function del(k, m) {
  m.delete(k);
}

export function unsafeGet(k, m) {
  return m.get(k);
}
