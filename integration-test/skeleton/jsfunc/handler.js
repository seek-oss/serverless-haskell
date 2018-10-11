'use strict';

module.exports.main = (event, context, callback) => {
  callback(null, {
    result: "Hello from JavaScript"
  });
};
