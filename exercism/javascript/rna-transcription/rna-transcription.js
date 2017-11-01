'use strict';

var COMPLEMENTS = {
    'G': 'C',
    'C': 'G',
    'T': 'A',
    'A': 'U'
};

module.exports = function (strand) {
    return strand.split('').map(function (nucleotide) {
        return COMPLEMENTS[ nucleotide ];
    }).join('');
};
