'use strict';

exports.splitAtImpl = function(just, nothing, tuple, i, s) {
  return i >= 0 && i < s.length ?
         just(tuple(s.substring(0, i), s.substring(i))) : nothing;
};

exports.initModal = function(elements) {
  return function() {
    elements.modal();
  };
};

exports.openModal = function(elements) {
  return function() {
    elements.modal('open');
  };
};

exports.closeModal = function(elements) {
  return function() {
    elements.modal('close');
  };
};
