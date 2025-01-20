# a-language-not-for-speaking
Kavi Duvvoori

An esoteric programming language used to create the dialogue "These Words, an Execution" for [Ensemble Park](https://www.ensemblepark.com/).

## To use the language

- Download and install a Racket interpreter, instructing your operating system to trust it. [DrRacket](https://racket-lang.org/) is the likeliest and possibly only option.
- Download or clone the repository. 
- Open a-language-not-for-speaking.rkt with DrRacket or your hypothetical other interpreter of choice.
- Click run, the green arrow.
- Now you can talk with it and be evaluated. Put your prompts in the form (? "This is a prompt.") after the ">" in the console that pops up in the bottom of the editor.

## Language specification

The language is based on "brainfuck" - one of the smallest possible ways of describing a Universal Turing Machine. Statements in the form (? "...") are executed with the output following. Some more information about esoteric programming languages can be found on this excellent blog / archive by Daniel Temking [esoteric.codes](https://esoteric.codes) or this fascinating article by Brandee Easter [“Feminist_brevity_in_light_of_masculine_long-windedness” code, space, and online misogyny](https://doi.org/10.1080/14680777.2018.1447335). Here is a rough summary of the rules of the language interpreter:

Data is stored, rather than in bytes, as positions in a list of alphabetized wordlists (subject pronouns, adverbs, verbs, determiners, adjectives, and nouns). There is a pointer that identifies one of the wordlists as active, always initially pointed to the list of adjectives. Each time the language is initiated, the starting point (the "zero") in each list is randomized but kept consistent across that interaction. When a phrase is generated grammatically, it uses the current status of the relevant wordlist. The interpreter prints the state of its memory, as a sentence, at the beginning and end of the exchange. This could be translated back into arbitrary numeric data.

The input is split into words by whitespaces, and evaluated by consulting, in order, the following table. Rather than single words causing an operation, categories or lists of words do so. Membership of a category like "author" or "noun" means belonging to the short corresponding corpora list - most authors or nouns will not be recognized by a-language-not-for-speaking.

| Instruction | Result |
| --- | ---|
| Invalid input, or an author's last name | Say something [1] |
| First in a closed rhyming pair [2] | If the active list is where it started, skip forward to after partner rhyme, and say a preposition and an environmental hazard (in dashes) |
| Second in a closed rhyming pair | Jump backward to partner rhyme and say an infinitive verb phrase (up to depth limit = 99) |
| An interjection | Say the phrase until the next stopword [3], replacing each ultraconserved word [4] with its translation into a random Indo-European language (incl. Esperanto) surrounded by "_" |
| Word with even number of letters starting in a vowel | Switch active list to the next one, and say the active word |
| Word with even number of letters starting in a consonant | Switch active list to the previous one, and say the active word |
| Word with odd number of letters ending in a vowel | Change word in active list to the next one, and say the active word |
| Word with odd number of letters ending in a consonant | Change word in active list to the previous one, and say the active word |

[1] Say something will consist of an operation from the the Burroughs B6x00-7x00 computer instruction set, a colon, and a Harvard sentence used as a sample for acoustic testing

[2] A closed pair consists of two words that only rhyme with each other

[3] A word frequently filtered out in natural language processing for allegedly carrying little meaning of its own

[4] A list of words some linguists argue remain largely conserved from Proto-Indo-European

The full language with some documentation can be found at, and interacted with live by downloading and using a Racket interpreter (eg DrRacket): [ https://github.com/notkrd/a-language-not-for-speaking](https://github.com/notkrd/a-language-not-for-speaking) 

It's likely, due to bugs and other mistakes, that the actual behavior of the language does not exactly follow the intentions described here.
