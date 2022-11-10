'use strict';

const newImpl = (val) => {
  return { value: val };
};

export { newImpl as new };

export function read(ref) {
  return ref.value;
}

export function modify(f, ref) {
  ref.value = f(ref.value);
  return ref.value;
}

export function write(val, ref) {
  ref.value = val;
}
