'use strict';

exports.splitAtImpl = function(just, nothing, tuple, i, s) {
  return i >= 0 && i < s.length ?
         just(tuple(s.substring(0, i), s.substring(i))) : nothing;
};
