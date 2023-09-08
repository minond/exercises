'use strict';

function normalize (word) {
    return word.toLowerCase().split("").sort().join("");
}

module.exports = function (word) {
    var normalized = normalize(word);

    return {
        matches: function (words) {
            return [].concat.apply([], arguments).filter(function (cword) {
                return normalized === normalize(cword) &&
                    word.toLowerCase() !== cword.toLowerCase();
            });
        }
    };
};