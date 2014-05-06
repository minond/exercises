'use strict';

function normalize (word) {
    return word.toLowerCase().split("").sort().join("");
}

function anagram (word) {
    var normalized = normalize(word);

    return {
        matches: function (words) {
            return [].concat.apply([], arguments).filter(function (cword) {
                return normalized === normalize(cword) &&
                    word.toLowerCase() !== cword.toLowerCase();
            });
        }
    };
}

module.exports = anagram;
