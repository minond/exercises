'use strict';

function empty_word (word) {
    return !!word;
}

function counter (word) {
    if (!this.hasOwnProperty(word)) {
        this[ word ] = 0;
    }

    this[ word ]++;
}

function words (phrase) {
    var tracker = {},
        splitter = /[\s,.~`<>/\\=!¡?¿@$#%^&*():;{}[\]'"\|\-]+/g;

    phrase.toLowerCase().split(splitter)
        .filter(empty_word)
        .forEach(counter, tracker);

    return tracker;
}

module.exports = words;
