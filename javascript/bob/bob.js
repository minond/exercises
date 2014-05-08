'use strict';

function Bob () {}

Bob.prototype.hey = function (message) {
    var response = 'Whatever.',
        has_letter = /[a-zA-Z]/.test(message),
        is_empty = !message.replace(/\s+/, ''),
        is_question = message.substr(-1) === '?',
        is_upper = message === message.toUpperCase();

    if (is_empty) {
        response = 'Fine. Be that way!';
    } else if (has_letter && is_upper) {
        response = 'Woah, chill out!';
    } else if (is_question) {
        response = 'Sure.';
    }

    return response;
};

module.exports = Bob;
