'use strict';

exports.new = function() {
  return new Map();
}

exports.getImpl = function(k, map) {
  return map.get(k);
}

exports.setImpl = function(k, v, map) {
  map.set(k, v);
  return {};
}

var getRandomValues =
  (typeof crypto !== 'undefined' &&
    crypto.getRandomValues &&
    crypto.getRandomValues.bind(crypto)) ||
  (typeof msCrypto !== 'undefined' &&
    typeof msCrypto.getRandomValues === 'function' &&
    msCrypto.getRandomValues.bind(msCrypto));

var baseBytes = new Uint8Array(16);

var byteToHex = [];

for (var i = 0; i < 256; ++i) {
  byteToHex.push((i + 0x100).toString(16).substr(1));
}

exports.randomKey = function() {
  var bytes = getRandomValues(baseBytes);
  bytes[6] = (bytes[6] & 0x0f) | 0x40;
  bytes[8] = (bytes[8] & 0x3f) | 0x80;

  return (
    byteToHex[bytes[0]] +
    byteToHex[bytes[1]] +
    byteToHex[bytes[2]] +
    byteToHex[bytes[3]] +
    byteToHex[bytes[4]] +
    byteToHex[bytes[5]] +
    byteToHex[bytes[6]] +
    byteToHex[bytes[7]] +
    byteToHex[bytes[8]] +
    byteToHex[bytes[9]] +
    byteToHex[bytes[10]] +
    byteToHex[bytes[11]] +
    byteToHex[bytes[12]] +
    byteToHex[bytes[13]] +
    byteToHex[bytes[14]] +
    byteToHex[bytes[15]]
  ).toLowerCase();
}
