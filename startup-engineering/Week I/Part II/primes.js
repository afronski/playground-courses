#!/usr/bin/env node

var fs = require("fs");

function isPrime(n) {
  var l = Math.sqrt(n),
      i;

  for (i = 2; i <= l; ++i) {
    if (n % i == 0) {
      return false;
    }
  }

  return true;
}

function getPrimes(N) {
  var l = Math.pow(2, 32) - 1,
      i,
      primesFound,
      result = [];

  for (i = 2, primesFound = 0; i < l; ++i) {
    if (isPrime (i)) {
      primesFound++;
      result.push(i);
    }

    if (primesFound == 100) {
      break;
    }
  }

  return result;
}

fs.writeFile("primes.txt", getPrimes(100).join(","));