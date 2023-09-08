'use strict';

var STARTING_AMOUNT = 99;
var VERSE = ':count :subject of beer on the wall, :count :subject of beer.\n' +
    ':action, :ncount :next_subject of beer on the wall.\n';

function it_or_one (count) {
    return count === 1 ? 'it' : 'one';
}

function subject (count) {
    return count === 1 ? 'bottle' : 'bottles';
}

function first_count_label (count) {
    return count || 'No more';
}

function count_label (count) {
    return count || 'no more';
}

function next_action (count) {
    return count ? 'Take :pronoun down and pass it around' :
        'Go to the store and buy some more';
}

function verse (count) {
    var ncount;

    ncount = count - 1;
    ncount = ncount < 0 ? STARTING_AMOUNT : ncount;

    return VERSE
        .replace(/:action/g, next_action(count))
        .replace(/:pronoun/g, it_or_one(count))
        .replace(/:count/, first_count_label(count))
        .replace(/:count/g, count_label(count))
        .replace(/:ncount/g, count_label(ncount))
        .replace(/:subject/g, subject(count))
        .replace(/:next_subject/g, subject(ncount));
}

function sing (from, to) {
    var lines = [];

    for (to = to || 0; from >= to; from--) {
        lines.push(verse(from));
    }

    return lines.join("\n");
}

module.exports = {
    sing: sing,
    verse: verse
};