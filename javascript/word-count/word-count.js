'use strict';

var splitter = /[\s,.~`<>/\\=!¡?¿@$#%^&*():;{}[\]'"\|\-]+/g;

function empty_word (word) {
    return !!word;
}

function words (phrase) {
    var counter = {};

    phrase.toLowerCase().split(splitter).filter(empty_word)
        .forEach(function (word) {
            if (!counter.hasOwnProperty(word)) {
                counter[ word ] = 0;
            }

            counter[ word ]++;
        });

    return counter;
}

module.exports = words;
